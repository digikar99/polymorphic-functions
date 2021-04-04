(in-package adhoc-polymorphic-functions)

(defun recursive-function-p (name body)
  (when body
    (cond ((listp body)
           (if (eq name (car body))
               t
               (some (curry 'recursive-function-p name) (cdr body))))
          (t nil))))

;;; - run-time correctness requires
;;;   - DEFINE-POLYMORPH-FUNCTION -> DEFUN
;;;   - DEFPOLYMORPH
;;; - compile-time correctness requires
;;;   - DEFINE-POLYMORPH-FUNCTION -> DEFINE-COMPILER-MACRO
;;;   - GET-TYPE-LIST
;;;   - DEFPOLYMORPH-COMPILER-MACRO

(defmacro with-compiler-note (&body body)
  `(handler-case (progn ,@body)
     (no-applicable-polymorph (condition)
       (format *error-output*
               "~%; While compiling ~S: ~&; ~A"
               form
               (str-join (uiop:strcat #\newline ";  ")
                         (str-split #\newline (format nil "~A" condition))))
       original-form)
     (form-type-failure (condition)
       (when optim-speed
         (format *error-output*
                 (uiop:strcat "~%; Optimization of ~&;  "
                              (str-join (uiop:strcat #\newline ";  ")
                                        (str-split #\newline (format nil " ~S"
                                                                     original-form)))
                              "~%; is left to ~A because ADHOC-POLYMORPHIC-FUNCTIONS "
                              "is unable to optimize it ~%; because~&; ~A~&; ~&")
                 (lisp-implementation-type)
                 (str-join (uiop:strcat #\newline ";  ")
                           (str-split #\newline (format nil "~A" condition)))))
       (cond (optim-debug original-form)
             ((and optim-speed
                   (member :sbcl *features*)
                   original-form))
             ((or optim-speed optim-slight-speed) block-form)
             (t (error "Non-exhaustive cases in WITH-COMPILER-NOTE!"))))
     (polymorph-has-no-inline-lambda-body (condition)
       (when optim-speed
         (format *error-output*
                 (uiop:strcat "~&; "
                              (format nil "Unable to optimize ~S because:" original-form)
                              "~&;  ~A")
                 (str-join (uiop:strcat #\newline ";  ")
                           (str-split #\newline (format nil "~A" condition)))))
       ;; FIXME: Will SBCL optimize these cases - should these conditions be merged
       ;; into the previous FORM-TYPE-FAILURE case?
       (cond (optim-debug original-form)
             ((and optim-speed
                   (member :sbcl *features*)
                   original-form))
             ((or optim-speed optim-slight-speed) block-form)
             (t (error "Non-exhaustive cases in WITH-COMPILER-NOTE!"))))))

(defmacro define-polymorphic-function (name untyped-lambda-list &key overwrite &environment env)
  "Define a function named NAME that can then be used for DEFPOLYMORPH
for specializing on various argument types.

If OVERWRITE is T, all the existing polymorphs associated with NAME are deleted,
and new polymorphs will be ready to be installed.
If OVERWRITE is NIL, a continuable error is raised if the LAMBDA-LIST has changed."
  (declare (type function-name       name)
           (type untyped-lambda-list untyped-lambda-list))
  ;; TODO: Handle the case of redefinition
  (let ((*name*        name)
        (*environment*  env))
    `(,@(if optim-debug
            `(progn)
            `(handler-bind ((style-warning #'muffle-warning))))
      (eval-when (:compile-toplevel :load-toplevel :execute)
        ,(when overwrite
           `(undefine-polymorphic-function ',name))
        (register-polymorphic-function ',name ',untyped-lambda-list))
      #+sbcl (sb-c:defknown ,name * * nil :overwrite-fndb-silently t)
      (define-compiler-macro ,name (&whole form &rest args &environment env)
        (declare (ignore args))
        ;; FIXME: Is the assumption that FORM either starts with ',NAME or FUNCALL correct?
        (let ((*environment*                 env)
              (*compiler-macro-expanding-p*    t)
              (original-form                form)
              (block-form                    nil))
          (with-compiler-note
            (when (eq 'funcall (car form))
              (setf form (cons (second (second form))
                               (cddr form))))
            (setq block-form
                  (let* ((name       (first form))
                         (gensyms    (make-gensym-list (length (rest form))))
                         (block-name (if (and (listp name)
                                              (eq 'setf (first name)))
                                         (second name)
                                         name)))
                    ;; This block is necessary for optimization of &optional and &key
                    ;; and &rest args; otherwise, we will need to cons at runtime!
                    `(block ,block-name
                       (let (,@(loop :for sym :in gensyms
                                     :for form :in (rest form)
                                     :collect `(,sym ,form)))
                         (funcall (the function
                                       (polymorph-lambda
                                        ,(retrieve-polymorph-form
                                          name
                                          ',(lambda-list-type untyped-lambda-list)
                                          gensyms)))
                                  ,@gensyms)))))
            (let* ((arg-list  (rest form))
                   (polymorph (apply #'retrieve-polymorph ',name arg-list)))
              (with-slots (inline-lambda-body return-type type-list
                           compiler-macro-lambda)
                  polymorph
                (cond (compiler-macro-lambda
                          (funcall compiler-macro-lambda
                                   (cons inline-lambda-body (rest form))
                                   env))
                      (optim-speed
                       ;; TODO: Use some other declaration for inlining as well
                       ;; Optimized for speed and type information available
                       (if inline-lambda-body
                           `(the ,return-type (,inline-lambda-body ,@(cdr form)))
                           (signal 'polymorph-has-no-inline-lambda-body
                                   :name ',name :type-list type-list)))
                      (optim-debug        original-form)
                      (optim-slight-speed block-form)
                      (t (error "Non-exhaustive cases!")))))))))))

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

(defun ensure-unambiguous-call (name type-list)
  (loop :for list :in (polymorphic-function-type-lists
                       (fdefinition
                        name))
        :do (when (and (type-list-causes-ambiguous-call-p type-list list)
                       (not (equalp type-list list)))
              (error "The given TYPE-LIST ~%  ~S~%will cause ambiguous call with the type list ~%  ~S~% of an existing polymorph"
                     type-list
                     list)
              (return))))

(defun ensure-non-intersecting-type-lists (name type-list)
  (loop :for list :in (polymorphic-function-type-lists
                       (fdefinition
                        name))
        :do (when (and (type-list-intersect-p type-list list)
                       (not (equalp type-list list)))
              (error "The given TYPE-LIST ~%  ~S~%intersects with the type list ~%  ~S~% of an existing polymorph"
                     type-list
                     list)
              (return))))

(defun null-env-compilation-warnings (lambda-form)
  (let* ((warnings)
         (error-output
           (with-output-to-string (apf-error-output)
             (with-output-to-string (*error-output*)
               (let (#+sbcl (sb-c::*in-compilation-unit* nil))
                 (#+sbcl progn
                  #-sbcl with-compilation-unit #-sbcl (:override t)
                  ;; TODO: Improve error reporting on other systems
                  ;; This works on SBCL and CCL
                   (handler-bind ((warning (lambda (c)
                                             (push c warnings)
                                             (format apf-error-output "~&~A~&" c)
                                             (muffle-warning c))))
                     (compile nil lambda-form))))))))
    (if warnings error-output nil)))

;;; Earlier (prior to commit e7f11394023cf06075459ea4baa735ec8bda89f3),
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
  "  Expects OPTIONAL or KEY args to be in the form ((A TYPE) DEFAULT-VALUE) or ((A TYPE) DEFAULT-VALUE AP).
  NAME could also be (NAME &KEY INLINE)

  **Note**: `:inline t` can result in infinite expansions for recursive polymorphs. Proceed
at your own risk."
  (declare (type typed-lambda-list typed-lambda-list))
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
           (lambda-list-type (lambda-list-type typed-lambda-list :typed t)))
      (multiple-value-bind (param-list type-list)
          (defun-lambda-list typed-lambda-list :typed t)
        (multiple-value-bind (declarations body) (extract-declarations body)
          (let* ((lambda-body #+sbcl
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
                 (inline-lambda-body (when inline
                                       #-(or ccl sbcl) lambda-body
                                       #+ccl (nth 2 lambda-body)
                                       #+sbcl `(lambda ,@(nthcdr 2 lambda-body))))
                 #+sbcl
                 (node (gensym "NODE"))
                 #+sbcl
                 (sbcl-transform `(sb-c:deftransform ,name
                                      (,param-list ,(if (eq '&rest (lastcar type-list))
                                                        (butlast type-list)
                                                        type-list) *
                                       :policy (< debug speed)
                                       :node ,node)
                                    ;; FIXME: This leads to an O(n^2) complexity
                                    ;; Could sorting help?
                                    (if (most-specialized-applicable-transform-p
                                         ',name ,node ',type-list)
                                        `(apply ,',lambda-body
                                                ,@',(sbcl-transform-body-args typed-lambda-list
                                                                              :typed t))
                                        (sb-c::give-up-ir1-transform)))))
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
                                   (format *error-output* "~&; PROCEED AT YOUR OWN RISK!")))
                         (values inline-lambda-body
                                 nil))))
              ;; NOTE: We need the LAMBDA-BODY due to compiler macros,
              ;; and "objects of type FUNCTION can't be dumped into fasl files
              `(progn
                 #+sbcl ,(when inline-safe-lambda-body
                           (if optim-debug
                               sbcl-transform
                               `(locally (declare (sb-ext:muffle-conditions style-warning))
                                  (handler-bind ((style-warning #'muffle-warning))
                                    ,sbcl-transform))))
                 ,(when inline-note `(write-string ,inline-note *error-output*))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (register-polymorph ',name ',type-list ',return-type
                                       ',inline-safe-lambda-body
                                       ,lambda-body
                                       ',lambda-list-type)
                   ',name)))))))))

(defmacro defpolymorph-compiler-macro (name type-list compiler-macro-lambda-list
                                       &body body)
  "An example of a type-list for a function with optional args would be (STRING &OPTIONAL INTEGER)"
  (declare (type function-name name)
           (type type-list type-list))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-polymorph-compiler-macro
        ',name ',type-list
        (compile nil (parse-compiler-macro nil
                                           ',compiler-macro-lambda-list
                                           ',body)))
       ',name))

(defun undefpolymorph (name type-list)
  "Remove the POLYMORPH associated with NAME with TYPE-LIST"
  #+sbcl
  (let ((info  (sb-c::fun-info-or-lose name))
        (ctype (sb-c::specifier-type (list 'function type-list '*))))
    (setf (sb-c::fun-info-transforms info)
          (remove-if (curry #'sb-c::type= ctype)
                     (sb-c::fun-info-transforms info)
                     :key #'sb-c::transform-type)))
  (remove-polymorph name type-list))

(defun undefine-polymorphic-function (name)
  "Remove the POLYMORPH(-WRAPPER) defined by DEFINE-POLYMORPH"
  (fmakunbound name)
  (setf (compiler-macro-function name) nil))
