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

(defmacro define-typed-function (name untyped-lambda-list)
  "Define a function named NAME that can then be used for DEFUN-TYPED for specializing on ORDINARY and OPTIONAL argument types."
  (declare (type function-name       name)
           (type untyped-lambda-list untyped-lambda-list))
  ;; TODO: Handle the case of redefinition
  (let* ((lambda-list untyped-lambda-list)
         (typed-args  (remove-untyped-args lambda-list :typed nil))
         ;; TODO: Handle the case of parsed-args better
         (parsed-args (parse-lambda-list   lambda-list :typed nil)))

    (register-typed-function-wrapper name lambda-list)
    `(progn

       (eval-when (:compile-toplevel)
         ;; Take this out of progn?
         ;; > Perhaps, keep it inside; helps macroexpanders know better what the macro is doing
         (register-typed-function-wrapper ',name ',lambda-list))
       
       (defun ,name ,lambda-list
         (let ((type-list (list ,@(loop :for typed-arg :in typed-args
                                        :collect `(type-of ,typed-arg)))))
           (funcall (nth-value 1 (retrieve-typed-function ',name type-list))
                    ,@parsed-args)))
       
       (define-compiler-macro ,name (&whole form ,@lambda-list &environment env)
         (declare (ignorable ,@typed-args)) ; typed-args are a subset of lambda-list
         (with-compiler-note
           (if (eq (car form) ',name)
               (progn
                 ;; TODO: Check this!
                 (let ((type-list (get-type-list (list ,@typed-args) env)))
                   ;; (format t "~%COMPILE-TIME TYPE-LIST: ~D~%" type-list)
                   (if (every (curry #'eq t) type-list) ; check for valid type list
                       (when (< 1 (policy-quality 'speed env))
                         ;; Optimized for speed, but type information not available
                         (signal 'optimize-speed-note
                                 :form form
                                 :reason "TYPE-LIST was concluded to be ~D"
                                 :args (list type-list))
                         form)
                       ;; TODO: Use some other declaration for inlining as well
                       ;; Optimized for speed and type information available
                       ;; We have a valid TYPE-LIST
                       (handler-case
                           (cond
                             ((retrieve-typed-function-compiler-macro ',name type-list)
                              (funcall (retrieve-typed-function-compiler-macro ',name type-list)
                                       form
                                       env))
                             ((< 1 (policy-quality 'speed env)) ; inline
                              `((lambda ,@(subseq (nth-value 0
                                                   (retrieve-typed-function ',name type-list))
                                           2))
                                ,@(cdr form)))
                             (t ; Check the types since they are declared
                              (retrieve-typed-function ',name type-list)
                              form))
                         (error (condition)
                           (signal 'compiler-note :reason
                                   (str:replace-all (string #\newline)
                                                    (uiop:strcat #\newline #\; #\space)
                                                    (format nil "~D" condition)))
                           form)))))
               (progn
                 (signal 'optimize-speed-note
                         :form form
                         :reason "COMPILER-MACRO of ~D can only optimize raw function calls."
                         :args (list ',name))
                 form)))))))

(defmacro defun-typed (name typed-lambda-list &body body)
  "  Expects OPTIONAL args to be in the form ((A TYPE) DEFAULT-VALUE) or ((A TYPE) DEFAULT-VALUE AP)."
  (declare (type function-name name)
           (type typed-lambda-list typed-lambda-list))
  ;; TODO: Handle the case when NAME is not bound to a TYPED-FUNCTION
  (let* ((lambda-list        typed-lambda-list)
         (actual-lambda-list (typed-function-wrapper-lambda-list
                              (retrieve-typed-function-wrapper name)))
         (type-list          (nth-value 1 (remove-untyped-args lambda-list :typed t)))
         (lambda-body        `(named-lambda ,name ,actual-lambda-list ,@body)))
    ;; We need the LAMBDA-BODY due to compiler macros, and "objects of type FUNCTION can't be dumped into fasl files.
    `(progn
       (register-typed-function ',name ',type-list
                                ',lambda-body
                                (named-lambda ,name ,actual-lambda-list
                                  ,@body))
       ',name)))

(defmacro define-compiler-macro-typed (name type-list compiler-macro-lambda-list
                                       &body body)
  (declare (type function-name name)
           (type type-list type-list))
  ;; TODO: Handle the case when NAME is not bound to a TYPED-FUNCTION
  (let ((gensym (gensym)))
    `(progn
       (compile ',gensym (parse-compiler-macro ',gensym
                                              ',compiler-macro-lambda-list
                                              ',body))
       (register-typed-function-compiler-macro ',name ',type-list
                                               (symbol-function ',gensym))
       ',name)))


