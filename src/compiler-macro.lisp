(in-package :polymorphic-functions)

(defun pf-compiler-macro (form &optional env)
  (when (eq 'apply (first form))
    (format *error-output* "~A can optimize cases other than FUNCALL and raw call!~%Assk maintainer of ADHOC-POLYMORPHIC-FUNCTIONS to provide support for this case!"
            (lisp-implementation-type))
    (return-from pf-compiler-macro form))
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
             ;; FORM-TYPE-FAILURE: We not only want to inform the user that there
             ;; was a failure, but also inform them what the failure-ing form was.
             ;; However, even if there was what appeared to be a failure on an
             ;; earlier polymorph (because its type list was more specific than T),
             ;; the possibility that there exists a later polymorph remains.
             ;; Therefore, we first muffle the duplicated FORM-TYPE-FAILUREs;
             ;; then, if a polymorph was found (see COND below), we further muffle
             ;; the non-duplicated failures as well.
             (form-type-failures nil)
             (polymorph (handler-bind ((form-type-failure
                                         (lambda (c)
                                           (if (find c form-type-failures
                                                     :test (lambda (c1 c2)
                                                             (equalp (form c1)
                                                                     (form c2))))
                                               (compiler-macro-notes:muffle c)
                                               (push c form-type-failures)))))
                          (apply #'compiler-retrieve-polymorph
                                 name (mapcar #'cons (rest form) arg-types)))))
        (when optim-debug
          (return-from pf-compiler-macro original-form))
        (cond ((and (null polymorph)
                    optim-speed
                    ;; We can be sure that *something* will be printed
                    ;; However, if there were no failures, and no polymorphs
                    ;; *nothing* will be shown! And then we rely on the
                    ;; NO-APPLICABLE-POLYMORPH/COMPILER-NOTE below.
                    form-type-failures)
               (return-from pf-compiler-macro original-form))
              ((not (null polymorph))
               (mapc #'compiler-macro-notes:muffle form-type-failures)))
        (when (and (null polymorph)
                   (or optim-speed optim-safety))
          (signal 'no-applicable-polymorph/compiler-note
                  :name name
                  :arg-list arg-list
                  :effective-type-lists
                  (polymorphic-function-effective-type-lists (fdefinition name))))
        (when (or (null polymorph)
                  (not optim-speed))
          (return-from pf-compiler-macro original-form))
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
            (return-from pf-compiler-macro
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
