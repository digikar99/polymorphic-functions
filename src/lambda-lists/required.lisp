(in-package :polymorphic-functions)

(defmethod %lambda-list-type ((type (eql 'required)) (lambda-list list))
  (if *lambda-list-typed-p*
      (every (lambda (arg)
               (valid-parameter-name-p (first arg)))
             lambda-list)
      (every 'valid-parameter-name-p lambda-list)))

(def-test type-identification (:suite lambda-list)
  (is (eq 'required (lambda-list-type '(a b))))
  (is-error (lambda-list-type '(a 5)))
  (is-error (lambda-list-type '(a b &rest))))

(defmethod %effective-lambda-list ((type (eql 'required)) (lambda-list list))
  (if *lambda-list-typed-p*
      (values (mapcar 'first  lambda-list)
              (mapcar 'second lambda-list)
              (mapcar 'second lambda-list))
      (copy-list lambda-list)))

(defmethod compute-polymorphic-function-lambda-body
    ((type (eql 'required)) (untyped-lambda-list list) &optional invalidated-p)
  (let ((block-name (blockify-name *name*)))
    `((declare (optimize speed))
      (block ,block-name
        ,(cond (invalidated-p
                `(progn
                   (update-polymorphic-function-lambda (fdefinition ',*name*))
                   (funcall (fdefinition ',*name*) ,@untyped-lambda-list)))
               (t
                `(funcall
                  (the function
                       (locally
                           (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
                         (cond
                           ,@(loop
                               :for i :from 0
                               :for polymorph
                                 :in (polymorphic-function-polymorphs (fdefinition *name*))
                               :for static-dispatch-name
                                 := (polymorph-static-dispatch-name polymorph)
                               :for runtime-applicable-p-form
                                 := (polymorph-runtime-applicable-p-form polymorph)
                               :collect
                               `(,runtime-applicable-p-form #',static-dispatch-name))
                           (t
                            (return-from ,block-name
                              (funcall ,(polymorphic-function-default (fdefinition *name*))
                                       ',*name* (list ,@untyped-lambda-list)))))))
                  ,@untyped-lambda-list)))))))

(defmethod %sbcl-transform-arg-lvars-from-lambda-list-form ((type (eql 'required))
                                                            (untyped-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  `(list ,@(loop :for arg :in untyped-lambda-list
                 :collect `(cons ',arg ,arg))))

(defmethod %lambda-declarations ((type (eql 'required)) (typed-lambda-list list))
  (assert *lambda-list-typed-p*)
  `(declare ,@(mapcar (lambda (elt)
                        (if (type-specifier-p (second elt))
                            `(type ,(second elt) ,(first elt))
                            `(type ,(upgrade-extended-type (second elt)) ,(first elt))))
                      typed-lambda-list)))

(defmethod enhanced-lambda-declarations ((type (eql 'required))
                                         (type-list list)
                                         (param-list list)
                                         (arg-types list))
  `(declare ,@(mapcar (lambda (arg arg-type)
                        `(type ,arg-type ,arg))
                      param-list
                      arg-types)))

(defmethod %type-list-compatible-p ((type (eql 'required))
                                    (type-list list)
                                    (untyped-lambda-list list))
  (length= type-list untyped-lambda-list))

(defmethod compiler-applicable-p-lambda-body ((type (eql 'required))
                                              (type-list list))
  (let ((param-list (mapcar #'type->param type-list)))
    (with-gensyms (form form-type)
      `(lambda ,param-list
         (declare (optimize speed))
         (block nil
           (and ,@(loop :for param :in param-list
                        :for type  :in type-list
                        :collect `(let ((,form      (car ,param))
                                        (,form-type (cdr ,param)))
                                    (cond ((eq t ',type)
                                           t)
                                          ((eq t ,form-type)
                                           (signal 'form-type-failure
                                                   :form ,form))
                                          (t
                                           (subtypep ,form-type ',type)))))))))))

(defmethod runtime-applicable-p-form ((type (eql 'required))
                                      (untyped-lambda-list list)
                                      (type-list list))
  `(and ,@(loop :for param :in untyped-lambda-list
                :for type  :in type-list
                :collect `(typep ,param ',type))))

(defmethod %type-list-subtype-p ((type-1 (eql 'required))
                                 (type-2 (eql 'required))
                                 list-1
                                 list-2)
  (declare (optimize speed)
           (type list list-1 list-2))
  (and (length= list-1 list-2)
       (every #'subtypep list-1 list-2)))

(def-test type-list-specialized-required (:suite type-list-subtype-p)
  (5am:is-true  (type-list-subtype-p '(string string) '(string array)))
  (5am:is-false (type-list-subtype-p '(array string) '(string array)))
  (5am:is-false (type-list-subtype-p '(string string) '(string number)))
  (5am:is-false (type-list-subtype-p '(string string) '(string))))

(defmethod %type-list-causes-ambiguous-call-p
    ((type-1 (eql 'required)) (type-2 (eql 'required)) list-1 list-2)
  (declare (optimize speed)
           (type list list-1 list-2))
  (and (length= list-1 list-2)
       (every #'type= list-1 list-2)))

(def-test type-list-causes-ambiguous-call-required
    (:suite type-list-causes-ambiguous-call-p)
  (5am:is-true  (type-list-causes-ambiguous-call-p '(string array) '(string (array))))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string string) '(string)))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string string) '(string array))))
