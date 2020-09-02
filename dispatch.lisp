(in-package typed-dispatch)

(defun get-type-list (arg-list &optional env)
  ;; TODO: Improve this
  (loop :for arg :in arg-list
        :collect (cond ((symbolp arg) (variable-type arg env))
                       (t (error "Cannot optimize this case!")))))

(defmacro define-typed-function (name lambda-list)
  (declare (type symbol        name)
           (type list   lambda-list))
  `(progn
     (register-typed-function-name ',name ',lambda-list)
     (defun ,name ,lambda-list
       (funcall (nth-value 1 (retrieve-typed-function ',name (mapcar #'type-of
                                                                     (list ,@lambda-list))))
                ,@lambda-list))
     (define-compiler-macro ,name (&whole form ,@lambda-list &environment env)
       (declare (ignorable ,@lambda-list))
       (if (eq (car form) ',name)
           (progn
             (let ((type-list (get-type-list (cdr form) env)))
               ;; (format t "~%COMPILE-TIME TYPE-LIST: ~D~%" type-list)
               (if (every (curry #'eq t) type-list)
                   (when (< 1 (policy-quality 'speed env))
                     (format t "~%Unable to optimize call to ~D because TYPE-LIST was concluded to be ~D" form type-list)
                     form)
                   (print ` (,(nth-value 0 (retrieve-typed-function ',name type-list))
                             ,@(cdr form))))))
           (progn
             (format t "COMPILER-MACRO can only optimize raw function calls.")
             form)))))

(defmacro defun-typed (name lambda-list &body body)
  (declare (type symbol        name)
           (type list   lambda-list))
  (let* ((actual-lambda-list (mapcar #'first lambda-list))
         (type-list          (mapcar (compose #'typexpand #'second) lambda-list))
         (lambda-body         `(lambda ,actual-lambda-list ,@body)))
    ;; We need the LAMBDA-BODY due to compiler macros, and "objects of type FUNCTION can't be dumped into fasl files.
    `(register-typed-function ',name ',type-list
                              ',lambda-body
                              (lambda ,actual-lambda-list
                                ,@body))))

