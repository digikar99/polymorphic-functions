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

(defun translate-body (body translation-alist)
  (flet ((translate (node)
           (if (listp node)
               node
               (or (cdr (assoc node translation-alist))
                   node))))
    (traverse-tree body #'translate)))

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
        (*environment*  env))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,(when overwrite
            `(undefine-polymorphic-function ',name))
         (register-polymorphic-function ',name ',untyped-lambda-list ,documentation
                                        ,default)
         #+sbcl (sb-c:defknown ,name * * nil :overwrite-fndb-silently t)
         (setf (compiler-macro-function ',name) #'pf-compiler-macro))
       #+sbcl (setf (slot-value (fdefinition ',name) 'sb-pcl::source)
                    (sb-c:source-location))
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
        :do (when (and (type-list-causes-ambiguous-call-p effective-type-list
                                                          existing-effective-type-list)
                       (not (equalp type-list existing-type-list)))
              (error "The given TYPE-LIST ~%  ~S~%effectively~%  ~S~%will cause ambiguous call with an existing polymorph with type list ~%  ~S~%and effective type list~%  ~S~%"
                     type-list
                     effective-type-list
                     existing-type-list
                     existing-effective-type-list)
              (return))))

(defun null-env-compilation-warnings (lambda-form)
  (let* ((warnings))
    (with-output-to-string (*error-output*)
      (let (#+sbcl (sb-c::*in-compilation-unit* nil))
        (#+sbcl progn
         #-sbcl with-compilation-unit #-sbcl (:override t)
         ;; TODO: Improve error reporting on other systems
         ;; This works on SBCL and CCL
         (handler-bind ((warning (lambda (c)
                                   (push c warnings)
                                   (muffle-warning c))))
           (compile nil lambda-form)))))
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
  NAME could also be (NAME &KEY (INLINE T)).
  Possible values for INLINE are T, NIL and :MAYBE

  **Note**: INLINE T or :MAYBE can result in infinite expansions for recursive polymorphs.
Proceed at your own risk."
  (destructuring-bind (name &key (inline t ip))
      (if (typep name 'function-name)
          (list name)
          name)
    (declare (type function-name name))
    (let* ((block-name       (if (and (listp name)
                                      (eq 'setf (first name)))
                                 (second name)
                                 name))
           (*environment*    env)
           (typed-lambda-list (normalize-typed-lambda-list typed-lambda-list))
           (untyped-lambda-list (untyped-lambda-list typed-lambda-list))
           (lambda-list-type  (lambda-list-type typed-lambda-list :typed t)))
      (declare (type typed-lambda-list typed-lambda-list))
      (multiple-value-bind (param-list type-list effective-type-list)
          (compute-effective-lambda-list typed-lambda-list :typed t)
        (multiple-value-bind (declarations body) (extract-declarations body)
          (let* ((static-dispatch-name (let* ((p-old (and (typep (fdefinition name)
                                                                 'polymorphic-function)
                                                          (find-polymorph name type-list)))
                                              (old-name
                                                (when p-old
                                                  (polymorph-static-dispatch-name p-old))))
                                         (if old-name
                                             old-name
                                             (gentemp (write-to-string
                                                       `(polymorph ,name ,type-list))
                                                      '#:polymorphic-functions.nonuser))))
                 (lambda-body #+sbcl
                              `(sb-int:named-lambda (polymorph ,name ,type-list) ,param-list
                                 ,(lambda-declarations typed-lambda-list :typed t)
                                 ,declarations
                                 (block ,block-name
                                   ,@(butlast body)
                                   (the ,return-type ,@(or (last body) '(nil)))))
                              #+ccl
                              `(ccl:nfunction (polymorph ,name ,type-list)
                                              (lambda ,param-list
                                                ,(lambda-declarations typed-lambda-list :typed t)
                                                ,declarations
                                                (block ,block-name
                                                  ,@(butlast body)
                                                  (the ,return-type ,@(or (last body) '(nil))))))
                              #-(or ccl sbcl)
                              `(lambda ,param-list
                                 ,(lambda-declarations typed-lambda-list :typed t)
                                 ,declarations
                                 (block ,block-name
                                   ,@(butlast body)
                                   (the ,return-type ,@(or (last body) '(nil))))))
                 ;; Currently we only need INLINE-LAMBDA-BODY and the checks in M-V-B
                 ;; below for DEFTRANSFORM
                 ;; FIXME: Do away with this use even
                 (inline-lambda-body (when inline
                                       #-(or ccl sbcl) lambda-body
                                       #+ccl (nth 2 lambda-body)
                                       #+sbcl `(lambda ,@(nthcdr 2 lambda-body))))
                 #+sbcl
                 (sbcl-transform-body (make-sbcl-transform-body name
                                                                typed-lambda-list
                                                                inline-lambda-body)))
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
                                   (note-no-inline form "")
                                   (note-null-env inline-lambda-body
                                                  null-env-compilation-warnings)
                                   (format *error-output* "~&; PROCEED AT YOUR OWN RISK!~%~%")))
                         (values inline-lambda-body
                                 nil))))
              (setq inline (case inline
                             ((t) (if inline-note nil t))
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
                 ;; FIXME: Use COMPILER-MACRO-NOTES to emit these notes to allow muffling
                 ;; ,(when inline-note
                 ;;    `(when (or (= 3 (introspect-environment:policy-quality 'debug))
                 ;;               (= 3 (introspect-environment:policy-quality 'safety)))
                 ;;       (write-string ,inline-note *error-output*)))
                 ,(when inline-note `(write-string ,inline-note *error-output*))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   ;; We are not "fixing inlining" using DEFUN made functions
                   ;; because we need to take parametric polymorphism into account
                   ;; while inlining.
                   (setf (fdefinition ',static-dispatch-name) ,lambda-body)
                   (register-polymorph ',name ',inline ',type-list
                                       ',effective-type-list
                                       ',return-type
                                       ',inline-safe-lambda-body
                                       ',static-dispatch-name
                                       ',lambda-list-type
                                       ,(compiler-applicable-p-lambda-body
                                         lambda-list-type
                                         untyped-lambda-list
                                         effective-type-list))
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
                                           ',body)))
       ',name))

(defun undefpolymorph (name type-list)
  "Remove the POLYMORPH associated with NAME with TYPE-LIST"
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
