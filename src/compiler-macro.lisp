(in-package :adhoc-polymorphic-functions)

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
                                  (cond ((not (listp form)) form)
                                        ((macro-function (first form) env)
                                         (macroexpand form env))
                                        ((compiler-macro-function (first form) env)
                                         (compiler-macroexpand form env))
                                        (t form)))
                                (rest form)))
             ;; Expanding things here results in O(n) calls to this function
             ;; rather than O(n^2); although the actual effects of this are
             ;; insignificant for day-to-day compilations
             (form-type-failure-signalled)
             (polymorph (handler-case (apply #'retrieve-polymorph name arg-list)
                          (form-type-failure (c)
                            (signal c)
                            (setq form-type-failure-signalled t)
                            nil))))
        (when optim-debug
          (return-from apf-compiler-macro original-form))
        (when (and (or optim-speed optim-safety)
                   (null polymorph)
                   (not form-type-failure-signalled))
          (signal 'no-applicable-polymorph/compiler-note
                  :arg-list arg-list
                  :effective-type-lists
                  (polymorphic-function-effective-type-lists (fdefinition name))))
        (when (null polymorph)
          (return-from apf-compiler-macro original-form))
        (with-slots (inline-lambda-body return-type type-list
                     compiler-macro-lambda)
            polymorph
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
                  (t form))))))))
