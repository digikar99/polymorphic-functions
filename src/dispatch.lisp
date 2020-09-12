(in-package typed-dispatch)

(defun get-type-list (arg-list &optional env)
  ;; TODO: Improve this
  (flet ((type-declared-p (var)
           (cdr (assoc 'type (nth-value 2 (variable-information var env))))))
    (let* ((undeclared-args ())
           (type-list
             (loop :for arg :in arg-list
                   :collect (cond ((symbolp arg)
                                   (unless (type-declared-p arg)
                                     (push arg undeclared-args))
                                   (variable-type arg env))
                                  ((constantp arg) (type-of arg))
                                  ((and (listp arg)
                                        (eq 'the (first arg)))
                                   (second arg))
                                  (t (signal 'compiler-note "Cannot optimize this case!"))))))
      (if undeclared-args
          (mapcar (lambda (arg)
                    (signal 'undeclared-type :var arg))
                  (nreverse undeclared-args))
          type-list))))

(defmacro define-typed-function (name untyped-lambda-list)
  "Define a function named NAME that can then be used for DEFUN-TYPED for specializing on ORDINARY and OPTIONAL argument types."
  (declare (type function-name       name)
           (type untyped-lambda-list untyped-lambda-list))
  ;; TODO: Handle the case of redefinition
  (let* ((lambda-list untyped-lambda-list)
         (processed-lambda-list (process-untyped-lambda-list untyped-lambda-list))
         (typed-args  (remove-untyped-args processed-lambda-list :typed nil))
         ;; TODO: Handle the case of parsed-args better
         ;; (parsed-args (parse-lambda-list   lambda-list :typed nil))
         )

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; Take this out of progn?
       ;; > Perhaps, keep it inside; helps macroexpanders know better what the macro is doing
       (register-typed-function-wrapper ',name ',lambda-list)
       
       (defun ,name ,processed-lambda-list
         (declare (ignorable ,@(loop :for arg :in processed-lambda-list
                                     :unless (member arg lambda-list-keywords)
                                     :appending (etypecase arg
                                                  (symbol (list arg))
                                                  (list (list (first arg) (third arg)))))))
         ,(let ((typed-args  typed-args)
                (type-list-code nil)
                (arg-list-code  nil))
            (loop :for typed-arg := (first typed-args)
                  :while (and typed-arg (symbolp typed-arg))
                  :do (push `(type-of ,typed-arg) type-list-code)
                  :do (push typed-arg arg-list-code)
                  :do (setq typed-args (rest typed-args)))
            `(let ((type-list (list ,@type-list-code))
                   (arg-list  (list ,@arg-list-code)))
               ,@(when (and typed-args (listp (first typed-args)))
                   ;; some typed-args are still pending
                   ;; - these should be the &optional ones
                   (loop :for (typed-arg default argp) :in typed-args
                         :collect `(when ,argp
                                     (push (type-of ,typed-arg) type-list)
                                     (push ,typed-arg arg-list))))
               (nreversef type-list)
               (nreversef arg-list)
               (apply (nth-value 1 (retrieve-typed-function ',name type-list))
                      arg-list))))
       
       (define-compiler-macro ,name (&whole form &rest args &environment env)
         (declare (ignorable args))
         (if (eq (car form) ',name)
             (if (< 1 (policy-quality 'speed env)) ; optimize for speed
                 (handler-case
                     (let* ((type-list (get-type-list (subseq (cdr form)
                                                              0
                                                              (min (length (cdr form))
                                                                   (length ',typed-args)))
                                                      env)))
                       (multiple-value-bind (body function)
                           (retrieve-typed-function ',name type-list)
                         (when (closurep function)
                           (signal "~%~D ~D is a closure" ',name type-list))
                         (if-let ((compiler-function (retrieve-typed-function-compiler-macro
                                                      ',name type-list)))
                           (funcall compiler-function
                                    (cons `(lambda ,@(subseq body 2)) (rest form))
                                    env)
                           ;; TODO: Use some other declaration for inlining as well
                           ;; Optimized for speed and type information available
                           `((lambda ,@(subseq body 2)) ,@(cdr form)))))
                   (condition (condition)
                     (format *error-output* "~%~%; Unable to optimize ~D because:" form)
                     (write-string
                      (str:replace-all (string #\newline)
                                       (uiop:strcat #\newline #\; "  ")
                                       (format nil "~D" condition)))
                     form))
                 (progn
                   (handler-case
                       (let ((type-list (get-type-list (subseq (cdr form)
                                                               0
                                                               (min (length (cdr form))
                                                                    (length ',typed-args)))
                                                       env)))
                         (retrieve-typed-function ',name type-list))
                     (condition (condition)
                       (format *error-output* "~%~%; While compiling ~D: " form)
                       (write-string
                        (str:replace-all (string #\newline)
                                         (uiop:strcat #\newline #\; "   ")
                                         (format nil "~D" condition)))))
                   form))
             (progn
               (signal 'optimize-speed-note
                       :form form
                       :reason "COMPILER-MACRO of ~D can only optimize raw function calls."
                       :args (list ',name))
               form))))))


(defmacro defun-typed (name typed-lambda-list &body body)
  "  Expects OPTIONAL args to be in the form ((A TYPE) DEFAULT-VALUE) or ((A TYPE) DEFAULT-VALUE AP)."
  (declare (type function-name name)
           (type typed-lambda-list typed-lambda-list))
  ;; TODO: Handle the case when NAME is not bound to a TYPED-FUNCTION
  (let* ((lambda-list        typed-lambda-list)
         (processed-lambda-list (process-typed-lambda-list lambda-list)))
    (multiple-value-bind (param-list type-list)
        (remove-untyped-args lambda-list :typed t)
      (let* ((lambda-body `(named-lambda ,name ,processed-lambda-list
                             (declare ,@(let ((type-declarations nil))
                                          (loop :for type :in type-list
                                                :with param-list := param-list
                                                :do (unless (eq type '&optional)
                                                      (push `(type ,type ,(first param-list))
                                                            type-declarations)
                                                      (setq param-list (rest param-list))))
                                          type-declarations))
                             ,@body)))
        ;; TODO: We need the LAMBDA-BODY due to compiler macros, and "objects of type FUNCTION can't be dumped into fasl files. TODO: Is that an issue after 2.0.8+ as well?
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (register-typed-function ',name ',type-list
                                    ',lambda-body
                                    ,lambda-body)
           ',name)))))

(defmacro define-compiler-macro-typed (name type-list compiler-macro-lambda-list
                                       &body body)
  "An example of a type-list for a function with optional args would be (STRING &OPTIONAL INTEGER)"
  (declare (type function-name name)
           (type type-list type-list))
  ;; TODO: Handle the case when NAME is not bound to a TYPED-FUNCTION

  (let ((gensym (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-typed-function-compiler-macro
        ',name ',type-list
        (compile nil (parse-compiler-macro ',gensym
                                           ',compiler-macro-lambda-list
                                           ',body)))
       ',name)))


