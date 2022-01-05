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

;; FIXME: Rename type-list-more-specific-p to TYPE-LIST-MORE-SPECIFIC-P
(defmethod %type-list-more-specific-p ((type-1 (eql 'required))
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
                        (return-from %type-list-more-specific-p t))
                       (t
                        (return-from %type-list-more-specific-p nil)))
             :finally (return t))))

(def-test type-list-subtype-required (:suite type-list-more-specific-p)
  (5am:is-true  (type-list-more-specific-p '(string string) '(string array)))
  (5am:is-false (type-list-more-specific-p '(array string) '(string array)))
  (5am:is-true  (type-list-more-specific-p '(string array) '(array string)))
  (5am:is-false (type-list-more-specific-p '(string string) '(string number)))
  (5am:is-false (type-list-more-specific-p '(string string) '(string)))
  (5am:is-false (type-list-more-specific-p '((or string number) string) '((or string symbol) array))))

(defmethod %type-list-intersection-null-p
    ((type-1 (eql 'required)) (type-2 (eql 'required)) list-1 list-2)
  (declare (optimize speed)
           (type list list-1 list-2))
  (or (/= (length list-1) (length list-2))
      (loop :for type-1 :in list-1
            :for type-2 :in list-2

            ;; Return T the moment we have a non-null intersection
            ;; without a definite direction of SUBTYPEP

            ;; While going from left to right,
            ;; because the CALLER has previously checked that
            ;; none of the two type-lists are more specific than the other,
            ;; it must mean that the first time the types are different,
            ;; their intersection be NIL; if not, there would be ambiguity

            :do (if (type= type-1 type-2)
                    t
                    (when (definitive-subtypep `(and ,type-1 ,type-2) nil)
                      (return-from %type-list-intersection-null-p t)))
            :finally (return nil))))

(def-test type-list-intersection-null-required
    (:suite type-list-intersection-null-p)
  (5am:is-false (type-list-intersection-null-p '(string) '(string)))
  (5am:is-true  (type-list-intersection-null-p '(string string) '(string)))
  (5am:is-true  (type-list-intersection-null-p '(string string) '(t)))
  (5am:is-true  (type-list-intersection-null-p '(string string) '(number array)))
  (5am:is-false (type-list-intersection-null-p '(string string) '(string array)))
  (5am:is-false (type-list-intersection-null-p '(array string)  '(string array)))
  (5am:is-false (type-list-intersection-null-p '((or string number) string)
                                               '((or string symbol) array)))
  (5am:is-true  (type-list-intersection-null-p '((or string number) string)
                                               '((or string symbol) number)))
  )
