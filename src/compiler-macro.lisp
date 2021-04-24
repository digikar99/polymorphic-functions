(in-package :adhoc-polymorphic-functions)

(defun apf-compiler-macro (form &optional env)
  (when (eq 'apply (first form))
    (format *error-output* "~A can optimize cases other than FUNCALL and raw call!~%Assk maintainer of ADHOC-POLYMORPHIC-FUNCTIONS to provide support for this case!"
            (lisp-implementation-type))
    (return-from apf-compiler-macro form))
  (let* ((*environment*                 env)
         (*compiler-macro-expanding-p*    t)
         (original-form                form)
         (block-form                    nil)
         (block-name                    nil)
         (form                         (if (eq 'funcall (car form))
                                           (setf form (cons (second (second form))
                                                            (cddr form)))
                                           form))
         (name                         (first form)))
    (setq block-form
          (let* ((gensyms (make-gensym-list (length (rest form)))))
            (setq block-name
                  (if (and (listp name)
                           (eq 'setf (first name)))
                      (second name)
                      name))
            ;; This block is necessary for optimization of &optional and &key
            ;; and &rest args
            `(block ,block-name
               (let (,@(loop :for sym :in gensyms
                             :for form :in (rest form)
                             :collect `(,sym ,form)))
                 (funcall (the function
                               (polymorph-lambda
                                ,(retrieve-polymorph-form
                                  name
                                  (polymorphic-function-lambda-list-type
                                   (fdefinition name))
                                  gensyms)))
                          ,@gensyms)))))
    (compiler-macro-notes:with-notes
        (original-form :name (fdefinition name)
                       :unwind-on-signal nil
                       :optimization-note-condition optim-speed)
      (let* ((arg-list  (rest form))
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
          (return-from apf-compiler-macro
            (cond ((and optim-speed
                        (member :sbcl *features*))
                   ;; Transforms will take care of them
                   original-form)
                  ((or optim-speed optim-slight-speed) block-form)
                  (t (error "Non exhaustive cases! (loc 2)")))))
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
                       `(the ,return-type (,inline-lambda-body ,@(cdr form)))
                       (progn
                         (signal 'polymorph-has-no-inline-lambda-body
                                 :name name :type-list type-list)
                         block-form)))
                  (optim-slight-speed block-form)
                  (t (error "Non exhaustive cases! (loc 1)")))))))))
