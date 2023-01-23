(in-package :polymorphic-functions)

(defun most-specialized-applicable-transform-p (name arg-types-alist type-list)
  (let ((*compiler-macro-expanding-p* t))
    (declare (optimize debug))
    (equalp type-list
            (polymorph-type-list (apply #'compiler-retrieve-polymorph name arg-types-alist)))))

(defun make-sbcl-transform-body (name typed-lambda-list inline-lambda-body polymorph-parameters)
  (declare (optimize debug))
  (multiple-value-bind (param-list type-parameter-list type-list effective-type-list)
      (polymorph-effective-lambda-list polymorph-parameters)
    (declare (ignore effective-type-list))
    (when (or (some #'parametric-type-specifier-p type-list)
              ;; FIXME: Not a simple mapping, is it?
              (extended-type-list-p type-list))
      (return-from make-sbcl-transform-body nil))
    (let ((lambda-list-type (lambda-list-type typed-lambda-list :typed t))
          (transform-lambda-list (untyped-lambda-list typed-lambda-list)))
      (with-gensyms (node env arg arg-lvar-alist compiler-macro-lambda
                          inline-lambda-body-sym param-list-sym lambda declarations body
                          arg-types-alist arg-types type arg-syms lvar lvars lvar-syms lvar-sym
                          arg-sym lvar-types lvar-type compiler-macro-env sym)

        ;; For the polymorph compiler macro:
        ;; Firstly, call the COMPILER-MACRO-LAMBDA with SB-C::LVARs
        ;;   We expect it to be able to deal with them
        ;; Secondly, replace any SB-C::LVAR in the result form
        ;;   with the appropriate variable name
        ;; FIXME: SB-CLTL2:MACROEXPAND-ALL does not expand commas
        ;;   as of SBCL 2.1.5

        `(sb-c:deftransform ,name (,transform-lambda-list
                                   ,(if (eq 'rest lambda-list-type)
                                        (append type-list '(*))
                                        type-list)
                                   *
                                   :policy (< debug speed)
                                   :node ,node)

           (when *disable-static-dispatch*
             (sb-c::abort-ir1-transform))

           (let* ((,arg-lvar-alist
                    ,(sbcl-transform-arg-lvars-from-lambda-list-form
                      transform-lambda-list :typed nil))
                  ;; Although we call it LVARS, these may also contain KEYWORDs
                  (,lvars      (mapcar #'rest  ,arg-lvar-alist))
                  (,lvar-syms  (make-gensym-list (length ,lvars)))
                  (,lvar-types (loop :for ,lvar :in ,lvars
                                     :for ,lvar-type
                                       := (if (typep ,lvar 'sb-c::lvar)
                                              (sb-c::type-specifier
                                               (sb-c::%lvar-derived-type ,lvar))
                                              (nth-form-type ,lvar nil 0))
                                     :collect (optima:match ,lvar-type
                                                ((list* 'values ,type _)
                                                 ,type)
                                                (_
                                                 ,lvar-type))))
                  (,arg-syms (mapcar #'first ,arg-lvar-alist))
                  (,arg-types-alist
                    (mapcar (lambda (,arg)
                              (if (keywordp ,arg)
                                  (cons ,arg `(eql ,,arg))
                                  (let ((,type
                                          (sb-c::type-specifier
                                           (sb-c::%lvar-derived-type ,arg))))
                                    (cons ,arg
                                          (if (eq 'cl:* ,type)
                                              t
                                              (nth 1 ,type))))))
                            ,lvars))
                  (,arg-types (mapcar #'rest ,arg-types-alist))

                  (,compiler-macro-lambda (polymorph-compiler-macro-lambda
                                           (find-polymorph ',name ',type-list)))
                  (,env                   (sb-c::node-lexenv ,node))
                  (,compiler-macro-env    (augment-environment
                                           (augment-environment
                                            ,env
                                            :variable ,lvar-syms)
                                           :declare (mapcar (lambda (,type ,sym)
                                                              (list 'type ,type ,sym))
                                                            ,lvar-types
                                                            ,lvar-syms))))

             (unless (most-specialized-applicable-transform-p
                      ',name ,arg-types-alist ',type-list)
               (sb-c::give-up-ir1-transform))

             (let ((,inline-lambda-body-sym
                     (destructuring-bind (,lambda ,param-list-sym ,declarations &body ,body)
                         ',inline-lambda-body
                       (declare (ignore ,lambda ,param-list-sym))
                       `(cl:lambda ,',param-list
                          ;; The source of parametric-polymorphism
                          ,(enhanced-lambda-declarations (polymorph-parameters
                                                          (find-polymorph ',name ',type-list))
                                                         ,arg-types)
                          ,,declarations
                          (let ,,type-parameter-list
                            ,@,body)))))

               ,(if (eq 'rest lambda-list-type)
                    ;; Yes, we are returning a LAMBDA-FORM below
                    ``(cl:lambda ,,arg-syms
                        ,(if ,compiler-macro-lambda
                             (translate-body
                              (macroexpand-all
                               (funcall ,compiler-macro-lambda
                                        (cons ,inline-lambda-body-sym ,lvar-syms)
                                        ,compiler-macro-env))
                              (mapcar (lambda (,lvar-sym ,arg-sym)
                                        (cons ,lvar-sym ,arg-sym))
                                      ,lvar-syms ,arg-syms))
                             `(funcall ,,inline-lambda-body-sym ,@,arg-syms)))
                    `(if ,compiler-macro-lambda
                         (translate-body (macroexpand-all
                                          (funcall ,compiler-macro-lambda
                                                   (cons ,inline-lambda-body-sym
                                                         ,lvar-syms)
                                                   ,compiler-macro-env))
                                         (mapcar (lambda (,lvar-sym ,arg-sym)
                                                   (cons ,lvar-sym ,arg-sym))
                                                 ,lvar-syms ,arg-syms))
                         `(funcall ,,inline-lambda-body-sym ,@,arg-syms))))))))))
