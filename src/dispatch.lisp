(in-package polymorphic-functions)

(defun recursive-function-p (name body)
  (flet ((%recursive-p (node)
           (if (listp node)
               (if (eq name (first node))
                   (return-from recursive-function-p t)
                   node)
               nil)))
    (traverse-tree body #'%recursive-p)
    nil))

;;; - run-time correctness requires
;;;   - DEFINE-POLYMORPH-FUNCTION -> DEFUN
;;;   - DEFPOLYMORPH
;;; - compile-time correctness requires
;;;   - DEFINE-POLYMORPH-FUNCTION -> DEFINE-COMPILER-MACRO
;;;   - GET-TYPE-LIST
;;;   - DEFPOLYMORPH-COMPILER-MACRO

(defmacro define-polymorphic-function (name untyped-lambda-list
                                       &key overwrite
                                         (documentation nil docp)
                                         (default '(function no-applicable-polymorph))
                                       &environment env)
  "Define a function named NAME that can then be used for DEFPOLYMORPH
for specializing on various argument types.

If OVERWRITE is T, all the existing polymorphs associated with NAME are deleted,
and new polymorphs will be ready to be installed.
If OVERWRITE is NIL, a continuable error is raised if the LAMBDA-LIST has changed.

DEFAULT should be a FUNCTION that can be called with two arguments at run-time
and compile-time in case no polymorph is applicable.
- the first of these arguments is the NAME, while
- the second argument is the argument list with which the polymorphic-function
  was called or compiled.
At compile-time *COMPILER-MACRO-EXPANDING-P* is bound to non-NIL."
  (declare (type function-name       name)
           (type untyped-lambda-list untyped-lambda-list))
  (when docp (check-type documentation string))
  (let ((*name*        name)
        (*environment*  env)
        (untyped-lambda-list (if (member '&key untyped-lambda-list)
                                 (let ((key-position (position '&key untyped-lambda-list)))
                                   (append (subseq untyped-lambda-list 0 key-position)
                                           '(&key)
                                           (sort (subseq untyped-lambda-list (1+ key-position))
                                                 #'string<)))
                                 untyped-lambda-list)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,(when overwrite
            `(undefine-polymorphic-function ',name))
         (register-polymorphic-function ',name ',untyped-lambda-list ,documentation
                                        ,default
                                        :source #+sbcl (sb-c:source-location) #-sbcl nil)
         #+sbcl (sb-c:defknown ,name * * nil :overwrite-fndb-silently t)
         (setf (compiler-macro-function ',name) #'pf-compiler-macro))
       (fdefinition ',name))))

(defun extract-declarations (body)
  "Returns two values: DECLARATIONS and remaining BODY"
  (cond ((null body)
         (values `(declare) nil))
        ((and (listp (car body))
              (eq 'declare (caar body)))
         (multiple-value-bind (declarations rest-body) (extract-declarations (rest body))
           (values (nconc (cons 'declare (cdar body))
                          (rest declarations))
                   rest-body)))
        (t
         (values `(declare) body))))

(defun ensure-unambiguous-call (name type-list effective-type-list)
  (loop :for polymorph :in (polymorphic-function-polymorphs (fdefinition name))
        :for existing-type-list := (polymorph-type-list polymorph)
        :for existing-effective-type-list := (polymorph-effective-type-list polymorph)
        :for new-specific-p := (type-list-more-specific-p effective-type-list existing-effective-type-list)
        :for existing-specific-p := (type-list-more-specific-p existing-effective-type-list effective-type-list)
        :do (when (or (and new-specific-p
                           existing-specific-p
                           (not (equalp type-list existing-type-list)))
                      (and (not new-specific-p)
                           (not existing-specific-p)
                           (not (type-list-intersection-null-p effective-type-list
                                                               existing-effective-type-list))))
              (cerror "Undefine existing polymorph"
                      "The given TYPE-LIST ~%  ~S~%effectively~%  ~S~%will cause ambiguous call with an existing polymorph with type list ~%  ~S~%and effective type list~%  ~S~%"
                      type-list
                      effective-type-list
                      existing-type-list
                      existing-effective-type-list)
              (undefpolymorph name existing-type-list))))

(defmacro with-muffled-compilation-warnings (&body body)
  `(locally (declare #+sbcl (sb-ext:muffle-conditions warning))
     (let ((*error-output* (make-string-output-stream)))
       (let (#+sbcl (sb-c::*in-compilation-unit* nil)
             (*compile-verbose* nil))
         (#+sbcl progn
          #-sbcl with-compilation-unit #-sbcl (:override t)
          ;; TODO: Improve error reporting on other systems
          ;; This works on SBCL and CCL
          ,@body)))))

(defun null-env-compilation-warnings (lambda-form)
  (let* ((warnings))
    (with-muffled-compilation-warnings
      (handler-bind ((warning (lambda (c)
                                (push c warnings)
                                (muffle-warning c))))
        (compile nil lambda-form)))
    (if warnings
        (format nil "~{~A~^~%~}" (nreverse warnings))
        nil)))

;;; Earlier (until commit e7f11394023cf06075459ea4baa735ec8bda89f3),
;;; we attempted to use a code-walker to determine if there are
;;; free variables in the form, and accordingly decline to inline
;;; the polymorph. However, cases such as this (and while this is nonsense):
;;;   (define-polymorphic-function foo (a) :overwrite t)
;;;   (let ((a 5))
;;;     (defpolymorph foo ((symbol (eql a))) t
;;;       (declare (ignore symbol))
;;;       a)
;;;     (defun bar () (foo 'a)))
;;; demand a user supplied value for INLINE. We put the same to use and avoid
;;; depending on the code-walker altogether.

;;; The BODY of the DEFPOLYMORPH FORM may contain macro calls referenced to the
;;; null lexenv. When this BODY gets substituted in order to INLINE the PF,
;;; we want to avoid MACROLET (and FLET, LABELS) from overriding the elements
;;; of the null lexenv. That is what the behavior of INLINE-d functions defined
;;; by DEFUN is. (Does the spec say that?)
;;;   The MACROLET can be taken care of by MACROEXPAND-ALL. However, because
;;; compiling to support SUBTYPE and PARAMETRIC polymorphism requires type
;;; information that is only available at the call-compilation site rather than
;;; at the defpolymorph-definition site. Thus, the MACROEXPAND-ALL must happen
;;; within the pf-compiler-macro rather than DEFPOLYMORPH below.
;;;   That still leaves FLET and LABELS though. And that does form a limitation
;;; of polymorphic functions at the time of this writing.

;;; Do minimal work at macro-expansion time?
;;; 1. Well, to be able to handle closures, the compilation phase of the lambda
;;;    needs the env. However, env objects cannot be dumped; nor does it seem like
;;;    a wise idea to do so.
;;; 2. That means, the minimum work that we need to do during macroexpansion time
;;;    involves the emission of the lambda-body.

(defmacro defpolymorph (&whole form name typed-lambda-list return-type
                        &body body &environment env)
  "  Expects OPTIONAL or KEY args to be in the form

    ((A TYPE) DEFAULT-VALUE) or ((A TYPE) DEFAULT-VALUE AP).

  - NAME could also be (NAME &KEY (INLINE T) STATIC-DISPATCH-NAME).
  - Possible values for INLINE are T, NIL and :MAYBE
  - STATIC-DISPATCH-NAME could be useful for tracing or profiling

  **Note**: INLINE T or :MAYBE can result in infinite expansions for recursive polymorphs.
Proceed at your own risk."
  (destructuring-bind (name &key (inline t ip) (static-dispatch-name nil static-dispatch-name-p))
      (if (typep name 'function-name)
          (list name)
          name)
    (declare (type function-name name)
             (optimize debug))
    (let* ((block-name       (blockify-name name))
           (*environment*    env)
           (unsorted-typed-lambda-list (normalize-typed-lambda-list typed-lambda-list))
           (typed-lambda-list (if (member '&key unsorted-typed-lambda-list)
                                  (let ((key-position (position '&key unsorted-typed-lambda-list)))
                                    (append (subseq unsorted-typed-lambda-list 0 key-position)
                                            '(&key)
                                            (sort (subseq unsorted-typed-lambda-list (1+ key-position))
                                                  #'string<
                                                  :key #'caar)))
                                  unsorted-typed-lambda-list))
           (untyped-lambda-list (untyped-lambda-list typed-lambda-list))
           (pf-lambda-list      (if (and (fboundp name)
                                         (typep (fdefinition name) 'polymorphic-function))
                                    (mapcar (lambda (elt)
                                              (if (atom elt) elt (first elt)))
                                            (polymorphic-function-lambda-list
                                             (fdefinition name)))
                                    untyped-lambda-list))
           (parameters          (make-polymorph-parameters-from-lambda-lists
                                 pf-lambda-list typed-lambda-list))
           (lambda-list-type  (lambda-list-type typed-lambda-list :typed t)))
      (declare (type typed-lambda-list typed-lambda-list))
      (multiple-value-bind (param-list type-list effective-type-list)
          (polymorph-effective-lambda-list parameters)
        (multiple-value-bind (declarations body) (extract-declarations body)
          ;; USE OF INTERN BELOW:
          ;;   We do want STATIC-DISPATCH-NAME symbol collision to actually take place
          ;; when type lists of two polymorphs are "equivalent".
          ;;   (Credits to phoe for pointing out in the issue at
          ;;   https://github.com/digikar99/polymorphic-functions/issues/3)
          ;;   Consider a file A to be
          ;; compiled before restarting a lisp image, and file B after the
          ;; restart. The use of GENTEMP meant that two "separate" compilations of
          ;; the same polymorph in the two files, could result in different
          ;; STATIC-DISPATCH-NAMEs. If the two files were then loaded
          ;; simultaneously, and the polymorphs static-dispatched at some point,
          ;; then there remained the possibility that different static-dispatches
          ;; could be using "different versions" of the polymorph.
          ;;   Thus, we actually do want collisions to take place so that a same
          ;; deterministic/latest version of the polymorph is called; therefore we
          ;; use INTERN.
          (let* ((static-dispatch-name (if static-dispatch-name-p
                                           static-dispatch-name
                                           (let* ((p-old (and (fboundp name)
                                                              (typep (fdefinition name)
                                                                     'polymorphic-function)
                                                              (find-polymorph name type-list)))
                                                  (old-name
                                                    (when p-old
                                                      (polymorph-static-dispatch-name p-old))))
                                             (if old-name
                                                 old-name
                                                 (intern (write-to-string
                                                          `(polymorph ,name ,type-list))
                                                         '#:polymorphic-functions.nonuser)))))
                 (lambda-declarations (lambda-declarations parameters))
                 (lambda-body `(list-named-lambda (polymorph ,name ,type-list)
                                   ,(symbol-package block-name)
                                   ,param-list
                                 ,lambda-declarations
                                 ,declarations
                                 (block ,block-name
                                   ,(multiple-value-bind (form form-return-type)
                                        (ensure-type-form return-type
                                                          `(progn ,@body)
                                                          (augment-environment
                                                           env
                                                           :variable (remove-if
                                                                      #'null
                                                                      (mapcar #'third
                                                                              (rest lambda-declarations)))
                                                           :declare (rest lambda-declarations)))
                                      (setq return-type form-return-type)
                                      form))))
                 ;; Currently we need INLINE-LAMBDA-BODY and the checks in M-V-B
                 ;; below for DEFTRANSFORM; as well as to avoid the ASSERTs in
                 ;; pf-compiler-macro emitted by ENSURE-TYPE-FORM used for LAMBDA-BODY
                 (inline-lambda-body (when inline
                                       `(lambda ,param-list
                                          ,lambda-declarations
                                          ,declarations
                                          ;; The RETURN-TYPE here would be augmented by
                                          ;; PF-COMPILER-MACRO
                                          (block ,block-name
                                            ,@body))))
                 #+sbcl
                 (sbcl-transform-body (make-sbcl-transform-body name
                                                                typed-lambda-list
                                                                inline-lambda-body
                                                                parameters)))
            (multiple-value-bind (inline-safe-lambda-body inline-note)
                (cond ((and ip inline)
                       (values inline-lambda-body
                               (if-let (null-env-compilation-warnings
                                        (null-env-compilation-warnings inline-lambda-body))
                                 (with-output-to-string (*error-output*)
                                   (note-null-env inline-lambda-body
                                                  null-env-compilation-warnings))
                                 nil)))
                      ((and ip (not inline))
                       (values nil nil))
                      ((and (not ip)
                            (recursive-function-p name inline-lambda-body))
                       (values nil
                               (with-output-to-string (*error-output*)
                                 (note-no-inline form "it is suspected to result in infinite recursive expansion;~%  supply :INLINE T option to override and proceed at your own risk"))))
                      (t
                       (if-let (null-env-compilation-warnings
                                (null-env-compilation-warnings inline-lambda-body))
                         (values nil
                                 (with-output-to-string (*error-output*)
                                   (note-no-inline form "~%")
                                   (pprint-logical-block (*error-output* nil
                                                                         :per-line-prefix "  ")
                                     (note-null-env inline-lambda-body
                                                    null-env-compilation-warnings))
                                   (format *error-output* "~&PROCEED AT YOUR OWN RISK!~%~%")))
                         (values inline-lambda-body
                                 nil))))
              (setq inline (case inline
                             ((t) (if ip
                                      inline
                                      (if inline-note nil t)))
                             (otherwise inline)))
              ;; NOTE: We need the LAMBDA-BODY due to compiler macros,
              ;; and "objects of type FUNCTION can't be dumped into fasl files"
              `(progn
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (unless (and (fboundp ',name)
                                (typep (function ,name) 'polymorphic-function))
                     #+sbcl (sb-c:defknown ,name * * nil :overwrite-fndb-silently t)
                     (register-polymorphic-function ',name ',untyped-lambda-list nil
                                                    (function no-applicable-polymorph))
                     (setf (compiler-macro-function ',name) #'pf-compiler-macro)))
                 #+sbcl ,(when inline-safe-lambda-body
                           (if optim-debug
                               sbcl-transform-body
                               `(locally (declare (sb-ext:muffle-conditions style-warning))
                                  (handler-bind ((style-warning #'muffle-warning))
                                    ,sbcl-transform-body))))
                 ,(when inline-note
                    ;; Even STYLE-WARNING isn't appropriate to this, because we want to
                    ;; inform the user of the warnings even when INLINE option is supplied.
                    `(compiler-macro-notes:with-notes (',form nil :unwind-on-signal nil)
                       (signal 'defpolymorph-note :datum ,inline-note)
                       t))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   ;; We are not "fixing inlining" using DEFUN made functions
                   ;; because we need to take parametric polymorphism into account
                   ;; while inlining.
                   ,(if inline-note
                        `(with-muffled-compilation-warnings
                           (setf (fdefinition ',static-dispatch-name) ,lambda-body))
                        `(setf (fdefinition ',static-dispatch-name) ,lambda-body))
                   ,(let ((proclaimation
                            `(proclaim ',(ftype-for-static-dispatch static-dispatch-name
                                                                    effective-type-list
                                                                    return-type
                                                                    env))))
                      (if optim-debug
                          proclaimation
                          `(handler-bind ((warning #'muffle-warning))
                             ,proclaimation)))
                   (register-polymorph ',name ',inline
                                       ',typed-lambda-list
                                       ',type-list
                                       ',effective-type-list
                                       ',return-type
                                       ',inline-safe-lambda-body
                                       ',static-dispatch-name
                                       ',lambda-list-type
                                       ',(run-time-applicable-p-form parameters)
                                       ,(compiler-applicable-p-lambda-body parameters)
                                       #+sbcl (sb-c:source-location))
                   ',name)))))))))

(defmacro defpolymorph-compiler-macro (name type-list compiler-macro-lambda-list
                                       &body body)
  "Example TYPE-LISTs:
  (NUMBER NUMBER)
  (STRING &OPTIONAL INTEGER)
  (STRING &KEY (:ARG INTEGER))
  (NUMBER &REST)"
  (declare (type function-name name)
           (type type-list type-list))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-polymorph-compiler-macro
        ',name ',type-list
        (compile nil (parse-compiler-macro ',(if (and (listp name)
                                                      (eq 'setf (first name)))
                                                 (second name)
                                                 name)
                                           ',compiler-macro-lambda-list
                                           ',body))
        #+sbcl (sb-c:source-location))
       ',name))

(defun undefpolymorph (name type-list)
  "Remove the POLYMORPH associated with NAME with TYPE-LIST"
  ;; FIXME: Undefining polymorphs can also lead to polymorph call ambiguity.
  ;; One (expensive) solution is to insert afresh the type lists of all polymorphs
  ;; to resolve it.
  #+sbcl
  (unless (extended-type-list-p type-list)
    (let ((info  (sb-c::fun-info-or-lose name))
          (ctype (sb-c::specifier-type (list 'function type-list '*))))
      (setf (sb-c::fun-info-transforms info)
            (remove-if (curry #'sb-c::type= ctype)
                       (sb-c::fun-info-transforms info)
                       :key #'sb-c::transform-type))))
  (remove-polymorph name type-list)
  (update-polymorphic-function-lambda (fdefinition name) t))

(defun undefine-polymorphic-function (name)
  "Remove the POLYMORPH(-WRAPPER) defined by DEFINE-POLYMORPH"
  (fmakunbound name)
  #+sbcl (sb-c::undefine-fun-name name)
  (setf (compiler-macro-function name) nil))
