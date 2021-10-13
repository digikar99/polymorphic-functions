(in-package :polymorphic-functions)


;;; TODO: Allow user to specify custom optim-speed etc
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
        (original-form env :name (fdefinition name)
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

        (when (and optim-debug
                   (not (declaration-information 'pf-defined-before-use env)))
          (return-from pf-compiler-macro original-form))
        (cond ((and (null polymorph)
                    optim-speed
                    ;; We can be sure that *something* will be printed
                    ;; However, if there were no failures, and no polymorphs
                    ;; *nothing* will be shown! And then we rely on the
                    ;; NO-APPLICABLE-POLYMORPH/COMPILER-NOTE below.
                    form-type-failures)
               (return-from pf-compiler-macro original-form))
              (polymorph
               ;; We muffle, because unsuccessful searches could have resulted
               ;; in compiler notes
               (mapc #'compiler-macro-notes:muffle form-type-failures)))
        (when (and (null polymorph)
                   (or optim-speed optim-safety
                       (declaration-information 'pf-defined-before-use env)))
          (handler-case (funcall (polymorphic-function-default (fdefinition name))
                                 name env arg-list arg-types)
            (condition (c)
              (if (declaration-information 'pf-defined-before-use env)
                  (error c)
                  (signal c)))))
        (when (or (null polymorph)
                  (not optim-speed))
          (return-from pf-compiler-macro original-form))

        (with-slots (inline-p return-type type-list
                     compiler-macro-lambda lambda-list-type
                     static-dispatch-name parameters)
            polymorph
          (let ((inline-lambda-body (polymorph-inline-lambda-body polymorph)))
            (when inline-lambda-body
              (setq inline-lambda-body
                    ;; The only thing we want to preserve from the ENV are the OPTIMIZE
                    ;; declarations.
                    ;; Otherwise, the other information must be excluded
                    ;; because the POLYMORPH was originally expected to be defined in
                    ;; null env
                    (let* ((augmented-env (augment-environment
                                           env
                                           :declare (list
                                                     (cons 'optimize
                                                           (declaration-information 'optimize env)))))
                           (notes nil)
                           ;; The source of compile-time subtype-polymorphism
                           (lambda-with-enhanced-declarations
                             (destructuring-bind (lambda args declarations
                                                   more-decl (block block-name &body body))
                                 inline-lambda-body
                               (declare (ignore block declarations))
                               `(,lambda ,args
                                  ;; The source of compile-time subtype-polymorphism
                                  ,@(multiple-value-bind (enhanced-decl deparameterized-return-type)
                                        (enhanced-lambda-declarations parameters
                                                                      arg-types
                                                                      return-type)
                                      (if (equalp return-type deparameterized-return-type)
                                          `(,enhanced-decl
                                            ,more-decl
                                            (block ,block-name ,@body))
                                          (progn
                                            (setq return-type deparameterized-return-type)
                                            `(,enhanced-decl
                                              ,more-decl
                                              (block ,block-name
                                                ,@(butlast body)
                                                ,(car (cdaadr (lastcar body)))))))))))
                           (macroexpanded-form
                             (handler-bind ((form-type-failure
                                              (lambda (note)
                                                (push note notes))))
                               (macroexpand-all
                                lambda-with-enhanced-declarations augmented-env))))
                      ;; MUFFLE because they would already have been reported!
                      (mapc #'compiler-macro-notes:muffle notes)
                      ;; Some macroexpand-all can produce a (function (lambda ...)) from (lambda ...)
                      ;; Some others do not
                      (if (eq 'function (first macroexpanded-form))
                          (second macroexpanded-form)
                          macroexpanded-form))))
            (cond (compiler-macro-lambda
                   (return-from compiler-macro-notes:with-notes
                     (funcall compiler-macro-lambda
                              (cons inline-lambda-body (rest form))
                              env)))
                  (optim-speed
                   (return-from compiler-macro-notes:with-notes
                     (let ((inline-pf
                             (assoc 'inline-pf
                                    (nth-value 2 (function-information name env))))
                           (inline-dispatch-form
                             `(the ,return-type (,inline-lambda-body ,@arg-list)))
                           (non-inline-dispatch-form
                             `(the ,return-type (,static-dispatch-name
                                                 ,@arg-list))))
                       (ecase inline-p
                         ((t)
                          (assert inline-lambda-body)
                          (if (eq 'notinline-pf (cdr inline-pf))
                              non-inline-dispatch-form
                              inline-dispatch-form))
                         ((nil)
                          (when (eq 'inline-pf (cdr inline-pf))
                            (signal 'polymorph-has-no-inline-lambda-body
                                    :name name :type-list type-list))
                          non-inline-dispatch-form)
                         ((:maybe)
                          (cond ((null inline-pf)
                                 non-inline-dispatch-form)
                                ((eq 'inline-pf (cdr inline-pf))
                                 (assert inline-lambda-body)
                                 inline-dispatch-form)
                                ((eq 'notinline-pf (cdr inline-pf))
                                 non-inline-dispatch-form)
                                (t
                                 (error "Unexpected case in pf-compiler-macro!"))))))))
                  (t
                   (return-from pf-compiler-macro form)))))))))
