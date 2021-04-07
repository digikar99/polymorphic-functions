(in-package :adhoc-polymorphic-functions)

(defvar *note-on-apf-form-type-failure* t
  "If non-NIL, the compiler macro of ADHOC-POLYMORPHIC-FUNCTIONS emits FORM-TYPE
inference failure notes to aid portable optimizations.")

(defmacro with-compiler-note (&body body)
  `(handler-case (progn ,@body)
     (no-applicable-polymorph (condition)
       (format *error-output* "~%")
       (let ((*print-pretty* t))
         (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
           (format *error-output*
                   "While compiling ~%")
           (pprint-logical-block (*error-output* nil :per-line-prefix "  ")
             (format *error-output* "~S" form))
           (format *error-output* "~A" condition)))
       original-form)
     (form-type-failure (condition)
       (when (and optim-speed *note-on-apf-form-type-failure*)
         (format *error-output* "~%")
         (let ((*print-pretty* t))
           (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
             (write-string "Optimization of" *error-output*)
             (terpri *error-output*)
             (pprint-logical-block (*error-output* nil :per-line-prefix "  ")
               (format *error-output* "~S" original-form))
             (format *error-output* "~%is left to ~A because ADHOC-POLYMORPHIC-FUNCTIONS~%"
                     (lisp-implementation-type))
             (format *error-output* "is unable to optimize it because~&")
             (pprint-logical-block (*error-output* nil :per-line-prefix "  ")
               (format *error-output* "~A" condition)))))
       (cond (optim-debug original-form)
             ((and optim-speed
                   (member :sbcl *features*)
                   original-form))
             ((or optim-speed optim-slight-speed) block-form)
             (t (error "Non-exhaustive cases in WITH-COMPILER-NOTE!"))))
     (polymorph-has-no-inline-lambda-body (condition)
       (when optim-speed
         (format *error-output* "~%")
         (let ((*print-pretty* t))
           (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
             (format *error-output* "Unable to optimize")
             (pprint-logical-block (*error-output* nil :per-line-prefix "  ")
               (format *error-output* "~S" original-form))
             (format *error-output* "~&because~&")
             (pprint-logical-block (*error-output* nil :per-line-prefix "  ")
               (format *error-output* "~A" condition)))))
       ;; SBCL cannot optimize these cases, since no deftransform was produced
       (cond (optim-debug original-form)
             ((and optim-speed
                   (member :sbcl *features*)
                   original-form))
             ((or optim-speed optim-slight-speed) block-form)
             (t (error "Non-exhaustive cases in WITH-COMPILER-NOTE!"))))))

(defun apf-compiler-macro-lambda (name untyped-lambda-list)
  "Given NAME and UNTYPED-LAMBDA-LIST returns a compiler-macro-lambda for installation
for static-dispatch of ADHOC-POLYMORPHIC-FUNCTION"
  (compile nil
           (parse-compiler-macro
            name
            '(&whole form &rest args &environment env)
            `((declare (ignore args))
              ;; FIXME: Is the assumption that FORM either starts with ',NAME
              ;; or FUNCALL correct?
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
