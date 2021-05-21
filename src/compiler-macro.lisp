(in-package :polymorphic-functions)

(defun apf-compiler-macro (form &optional env)
  (when (eq 'apply (first form))
    (format *error-output* "~A can optimize cases other than FUNCALL and raw call!~%Assk maintainer of ADHOC-POLYMORPHIC-FUNCTIONS to provide support for this case!"
            (lisp-implementation-type))
    (return-from apf-compiler-macro form))
  (let* ((*environment*                 env)
         (*compiler-macro-expanding-p*    t)
         (original-form                form)
         (form                         (if (eq 'funcall (car form))
                                           (setf form (cons (second (second form))
                                                            (cddr form)))
                                           form))
         (name                         (first form)))
    (compiler-macro-notes:with-notes
        (original-form :name (fdefinition name)
                       :unwind-on-signal nil
                       :optimization-note-condition optim-speed)
      (let* ((arg-list  (mapcar (lambda (form)
                                  (with-output-to-string (*error-output*)
                                    (setq form
                                          (cond ((not (listp form)) form)
                                                ((macro-function (first form) env)
                                                 (macroexpand form env))
                                                ((compiler-macro-function (first form) env)
                                                 (compiler-macroexpand form env))
                                                (t form))))
                                  form)
                                (rest form)))
             ;; Expanding things here results in O(n) calls to this function
             ;; rather than O(n^2); although the actual effects of this are
             ;; insignificant for day-to-day compilations
             (arg-types (mapcar (lambda (form)
                                  (nth-form-type form env 0 t t))
                                arg-list))
             (polymorph (apply #'compiler-retrieve-polymorph name arg-types)))
        (when optim-debug
          (return-from apf-compiler-macro original-form))
        (cond ((and optim-speed
                    (null polymorph)
                    (member t arg-types))
               (signal 'form-type-failure
                       :form (nth (position t arg-types)
                                  (rest form))))
              ((and optim-safety
                    (null polymorph)
                    (not (member t arg-types)))
               (signal 'no-applicable-polymorph/compiler-note
                       :name name
                       :arg-list arg-list
                       :effective-type-lists
                       (polymorphic-function-effective-type-lists (fdefinition name)))))
        (when (or (null polymorph)
                  (not optim-speed))
          (return-from apf-compiler-macro original-form))
        (with-slots (return-type type-list
                     compiler-macro-lambda lambda-list-type)
            polymorph
          (let ((inline-lambda-body (polymorph-inline-lambda-body polymorph)))
            (when inline-lambda-body
              (setq inline-lambda-body
                    (destructuring-bind (lambda args declarations &body body)
                        inline-lambda-body
                      (declare (ignore declarations))
                      `(,lambda ,args
                         ;; The source of parametric-polymorphism
                         ,(enhanced-lambda-declarations lambda-list-type
                                                        type-list
                                                        args
                                                        arg-types)
                         ,@body))))
            (return-from apf-compiler-macro
              (cond (compiler-macro-lambda
                     (funcall compiler-macro-lambda
                              (cons inline-lambda-body (rest form))
                              env))
                    (optim-speed
                     ;; TODO: Use some other declaration for inlining as well
                     ;; Optimized for speed and type information available
                     (if inline-lambda-body
                         `(the ,return-type (,inline-lambda-body ,@arg-list))
                         (progn
                           (signal 'polymorph-has-no-inline-lambda-body
                                   :name name :type-list type-list)
                           form)))
                    (t form)))))))))
