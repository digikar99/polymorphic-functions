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
                                       ',*name* nil (list ,@untyped-lambda-list)))))))
                  ,@untyped-lambda-list)))))))

(defmethod %sbcl-transform-arg-lvars-from-lambda-list-form ((type (eql 'required))
                                                            (untyped-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  `(list ,@(loop :for arg :in untyped-lambda-list
                 :collect `(cons ',arg ,arg))))

(defmethod %type-list-compatible-p ((type (eql 'required))
                                    (type-list list)
                                    (untyped-lambda-list list))
  (length= type-list untyped-lambda-list))

;; FIXME: Rename TYPE-LIST-SUBTYPE-P to TYPE-LIST-MORE-SPECIFIC-P
(defmethod %type-list-subtype-p ((type-1 (eql 'required))
                                 (type-2 (eql 'required))
                                 list-1
                                 list-2)
  (declare (optimize speed)
           (type list list-1 list-2))
  (and (length= list-1 list-2)
       (loop :for type-1 :in list-1
             :for type-2 :in list-2
             ;; Return T the moment we find a SUBTYPEP with not TYPE=
             ;; The ones before this point should be TYPE=
             :do (cond ((type= type-1 type-2)
                        t)
                       ((subtypep type-1 type-2)
                        (return-from %type-list-subtype-p t))
                       (t
                        (return-from %type-list-subtype-p nil)))
             :finally (return t))))

(def-test type-list-subtype-required (:suite type-list-subtype-p)
  (5am:is-true  (type-list-subtype-p '(string string) '(string array)))
  (5am:is-false (type-list-subtype-p '(array string) '(string array)))
  (5am:is-true  (type-list-subtype-p '(string array) '(array string)))
  (5am:is-false (type-list-subtype-p '(string string) '(string number)))
  (5am:is-false (type-list-subtype-p '(string string) '(string)))
  (5am:is-false (type-list-subtype-p '((or string number) string) '((or string symbol) array))))

(defmethod %type-list-causes-ambiguous-call-p
    ((type-1 (eql 'required)) (type-2 (eql 'required)) list-1 list-2)
  (declare (optimize speed)
           (type list list-1 list-2))
  (and (length= list-1 list-2)
       (loop :for type-1 :in list-1
             :for type-2 :in list-2
             ;; Return T the moment we have a non-null intersection
             ;; without a definite direction of SUBTYPEP
             :do (cond ((type= nil `(and ,type-1 ,type-2))
                        (return-from %type-list-causes-ambiguous-call-p nil))
                       ((type= type-1 type-2)
                        t)
                       ((or (subtypep type-1 type-2)
                            (subtypep type-2 type-1))
                        (return-from %type-list-causes-ambiguous-call-p nil))
                       (t
                        (return-from %type-list-causes-ambiguous-call-p t)))
             :finally (return t))))

(def-test type-list-causes-ambiguous-call-required
    (:suite type-list-causes-ambiguous-call-p)
  (5am:is-true  (type-list-causes-ambiguous-call-p '(string array)  '(string (array))))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string string) '(string)))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string string) '(number array)))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string string) '(string array)))
  (5am:is-false (type-list-causes-ambiguous-call-p '(array string)  '(string array)))
  (5am:is-true  (type-list-causes-ambiguous-call-p '((or string number) string)
                                                   '((or string symbol) array)))
  )
