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
      (with-gensyms (node env compiler-macro-lambda
                          inline-lambda-body-sym param-list-sym lambda declarations body
                          arg-types-alist arg-types type)
        `(sb-c:deftransform ,name
             (,param-list
              ,(if (eq '&rest (lastcar type-list))
                   (butlast type-list)
                   type-list)
              *
              :policy (< debug speed)
              :node ,node)
           (declare (optimize debug))
           ,(let ((transformed-args (sbcl-transform-body-args typed-lambda-list :typed t)))
              `(let* ((,arg-types-alist
                        (list ,@(mapcar
                                 (lambda (arg)
                                   (if (keywordp arg)
                                       `(cons ,arg '(eql ,arg))
                                       `(cons ,arg
                                              (when ,arg
                                                ;; &rest lambda lists can result in NIL
                                                ;; Test system polymorph.callable to check
                                                ;; this issue
                                                (let ((,type
                                                        (sb-c::type-specifier
                                                         (sb-c::%lvar-derived-type ,arg))))
                                                  (if (eq 'cl:* ,type)
                                                      t
                                                      (nth 1 ,type)))))))
                                 (remove-if #'null transformed-args))))
                      (,arg-types (mapcar #'cdr ,arg-types-alist)))
                 (unless (most-specialized-applicable-transform-p
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
                                                             ,arg-types)
                              ,@,body))))
                   (if-let ((,compiler-macro-lambda
                             (polymorph-compiler-macro-lambda
                              (find-polymorph ',name ',type-list)))
                            (,env (sb-c::node-lexenv ,node)))
                     ,(let ((compiler-macro-arg-syms
                              (loop :for arg :in transformed-args
                                    :unless (null arg)
                                      :collect (gensym (symbol-name arg)))))
                        `(let (,@(loop :for compiler-macro-arg-sym
                                         :in compiler-macro-arg-syms
                                       :for arg :in transformed-args
                                       :collect `(,compiler-macro-arg-sym ,arg)))
                           (translate-body
                            (trivial-macroexpand-all:macroexpand-all
                             (funcall ,compiler-macro-lambda
                                      (cons ,inline-lambda-body-sym
                                            (list ,@compiler-macro-arg-syms))
                                      ,env))
                            (list ,@(mapcar (lambda (x y) `(cons ,x ',y))
                                            compiler-macro-arg-syms
                                            transformed-args)))))
                     (progn
                       `(apply ,,inline-lambda-body-sym ,@',transformed-args)))))))))))
