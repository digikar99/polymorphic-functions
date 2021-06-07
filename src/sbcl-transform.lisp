(in-package :polymorphic-functions)

(defun most-specialized-applicable-transform-p (name arg-types-alist type-list)
  (let ((*compiler-macro-expanding-p* t))
    (equalp type-list
            (polymorph-type-list (apply #'compiler-retrieve-polymorph name arg-types-alist)))))

(defun make-sbcl-transform-body (name typed-lambda-list inline-lambda-body)
  (multiple-value-bind (param-list type-list effective-type-list)
      (compute-effective-lambda-list typed-lambda-list :typed t)
    (declare (ignore effective-type-list))
    (let ((lambda-list-type (lambda-list-type typed-lambda-list :typed t)))
      (with-gensyms (node env arg compiler-macro-lambda
                          inline-lambda-body-sym param-list-sym lambda declarations body
                          arg-types-alist arg-types type arg-syms lvars lvar arg-sym)
        `(sb-c:deftransform ,name
             (,param-list
              ,(if (eq '&rest (lastcar type-list))
                   (append type-list '(*))
                   type-list)
              *
              :policy (< debug speed)
              :node ,node)
           (declare (optimize debug))
           ,(let* ((transformed-args (sbcl-transform-body-args typed-lambda-list :typed t)))
              `(let* (;; This is especially useful because for &REST ARGS,
                      ;;  ARGS is a list of SB-C::LVARs
                      (,lvars (flatten (list ,@transformed-args)))
                      (,arg-types-alist
                        (mapcar (lambda (,arg)
                                  (if (keywordp ,arg)
                                      (cons ,arg '(eql ,arg))
                                      (let ((,type
                                              (sb-c::type-specifier
                                               (sb-c::%lvar-derived-type ,arg))))
                                        (cons ,arg
                                              (if (eq 'cl:* ,type)
                                                  t
                                                  (nth 1 ,type))))))
                                ,lvars))
                      (,arg-types (mapcar #'cdr ,arg-types-alist)))
                 (unless (most-specialized-applicable-transform-p
                          ;; FIXME: Length of ARG-TYPES-ALIST for &rest lambda-lists
                          ',name ,arg-types-alist ',type-list)
                   (sb-c::give-up-ir1-transform))
                 (let ((,inline-lambda-body-sym
                         (destructuring-bind (,lambda ,param-list-sym ,declarations &body ,body)
                             ',inline-lambda-body
                           `(lambda ,',param-list
                              ;; The source of parametric-polymorphism
                              ,(enhanced-lambda-declarations ',lambda-list-type
                                                             ',type-list
                                                             ',param-list
                                                             ;; FIXME: length of ARG-TYPES
                                                             ,arg-types)
                              ,@,body)))
                       (,arg-syms (make-gensym-list (length ,lvars))))
                   ;; Yes, we are returning a LAMBDA-FORM
                   `(lambda ,,arg-syms
                      ,(if-let ((,compiler-macro-lambda
                                 (polymorph-compiler-macro-lambda
                                  (find-polymorph ',name ',type-list)))
                                (,env (sb-c::node-lexenv ,node)))
                         ;; Firstly, call the COMPILER-MACRO-LAMBDA with SB-C::LVARs
                         ;;   We expect it to be able to deal with them
                         ;; Secondly, replace any SB-C::LVAR in the result form
                         ;;   with the appropriate variable name
                         ;; FIXME: SB-CLTL2:MACROEXPAND-ALL does not expand commas
                         ;;   as of SBCL 2.1.5
                         (translate-body (trivial-macroexpand-all:macroexpand-all
                                          (funcall ,compiler-macro-lambda
                                                   (cons ,inline-lambda-body-sym ,lvars)
                                                   ,env))
                                         (mapcar (lambda (,lvar ,arg-sym) (cons ,lvar ,arg-sym))
                                                 ,lvars ,arg-syms))
                         `(funcall ,,inline-lambda-body-sym ,@,arg-syms)))))))))))
