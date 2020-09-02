(in-package typed-dispatch)

(defun get-type-list (arg-list &optional env)
  ;; TODO: Improve this
  (loop :for arg :in arg-list
        :collect (cond ((symbolp arg)   (variable-type arg env))
                       ((constantp arg) (type-of arg))
                       ((and (listp arg)
                             (eq 'the (first arg)))
                        (second arg))
                       (t (error "Cannot optimize this case!")))))

(defmacro define-typed-function (name lambda-list)
  "Define a function named NAME that can then be used for DEFUN-TYPED for specializing on ORDINARY and OPTIONAL argument types."
  (declare (type symbol        name)
           (type list   lambda-list))
  ;; TODO: The LAMBDA-LIST here should not have default values for REQUIRED and OPTIONAL args
  ;; in fact, it should not have default values for any arguments used in typing
  ;; TODO: Handle the case of redefinition
  (let ((typed-args  (remove-untyped-args lambda-list :typed nil))
        ;; TODO: Handle the case of parsed-args better
        (parsed-args (parse-lambda-list   lambda-list :typed nil)))
    `(progn
       (register-typed-function-name ',name ',lambda-list)
       (defun ,name ,lambda-list
         (let ((type-list (mapcar #'type-of (list ,@typed-args))))
           (funcall (nth-value 1 (retrieve-typed-function ',name type-list))
                    ,@parsed-args)))
       (define-compiler-macro ,name (&whole form ,@lambda-list &environment env)
         (declare (ignorable ,@typed-args))
         (if (eq (car form) ',name)
             (progn
               ;; TODO: Check this!
               (let ((type-list (get-type-list (list ,@typed-args) env)))
                 ;; (format t "~%COMPILE-TIME TYPE-LIST: ~D~%" type-list)
                 (cond ((and (every (curry #'eq t) type-list)
                             (< 1 (policy-quality 'speed env)))
                        (format t
                                "~%Unable to optimize call to ~D because TYPE-LIST was concluded to be ~D"
                                form type-list)
                        form)
                       ((< 1 (policy-quality 'speed env)) ; inline
                        `((lambda ,@(subseq (nth-value 0 (retrieve-typed-function ',name type-list))
                                    2))
                          ,@(cdr form)))
                       (t ; no inline
                        `(funcall ,(nth-value 0 (retrieve-typed-function ',name type-list))
                                  ,@(cdr form))))))
             (progn
               (format t "COMPILER-MACRO can only optimize raw function calls.")
               form))))))

(defun foo ())

(defmacro defun-typed (name lambda-list &body body)
  "Expects OPTIONAL args to be in the form ((A TYPE) DEFAULT-VALUE) or ((A TYPE) DEFAULT-VALUE AP)."
  (declare (type symbol        name)
           (type list   lambda-list))
  ;; TODO: Handle the case when NAME is not bound to a TYPED-FUNCTION
  (let* ((actual-lambda-list (typed-function-lambda-list
                              (retrieve-typed-function-with-name name)))
         (type-list          (nth-value 1 (remove-untyped-args lambda-list :typed t)))
         (lambda-body        `(named-lambda ,name ,actual-lambda-list ,@body)))
    ;; We need the LAMBDA-BODY due to compiler macros, and "objects of type FUNCTION can't be dumped into fasl files.
    `(progn
       (register-typed-function ',name ',type-list
                                ',lambda-body
                                (named-lambda ,name ,actual-lambda-list
                                  ,@body))
       ',name)))

