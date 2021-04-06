(in-package :adhoc-polymorphic-functions)

(defmethod %lambda-list-type ((type (eql 'required)) (lambda-list list))
  (if *lambda-list-typed-p*
      (every (lambda (arg)
               (and (valid-parameter-name-p (first arg))
                    (type-specifier-p       (second arg))))
             lambda-list)
      (every 'valid-parameter-name-p lambda-list)))

(def-test type-identification (:suite lambda-list)
  (is (eq 'required (lambda-list-type '(a b))))
  (is-error (lambda-list-type '(a 5)))
  (is-error (lambda-list-type '(a b &rest))))

(defmethod %defun-lambda-list ((type (eql 'required)) (lambda-list list))
  (if *lambda-list-typed-p*
      (values (mapcar 'first  lambda-list)
              (mapcar 'second lambda-list)
              (mapcar 'second lambda-list))
      (copy-list lambda-list)))

(defmethod %defun-body ((type (eql 'required)) (defun-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  `(funcall (polymorph-lambda ,(retrieve-polymorph-form *name* type defun-lambda-list))
            ,@defun-lambda-list))


(defmethod %sbcl-transform-body-args ((type (eql 'required)) (typed-lambda-list list))
  (assert *lambda-list-typed-p*)
  (append (mapcar #'first typed-lambda-list)
          '(nil)))

(defmethod %lambda-declarations ((type (eql 'required)) (typed-lambda-list list))
  (assert *lambda-list-typed-p*)
  `(declare ,@(mapcar (lambda (elt)
                        `(type ,(second elt) ,(first elt)))
                      typed-lambda-list)))

(defmethod %type-list-compatible-p ((type (eql 'required))
                                    (type-list list)
                                    (untyped-lambda-list list))
  (length= type-list untyped-lambda-list))

(defmethod applicable-p-function ((type (eql 'required)) (type-list list))
  (let ((param-list (mapcar #'type->param type-list)))
    `(lambda ,param-list
       (declare (optimize speed))
       (if *compiler-macro-expanding-p*
           (and ,@(loop :for param :in param-list
                        :for type  :in type-list
                        :collect `(our-typep ,param ',type)))
           (and ,@(loop :for param :in param-list
                        :for type  :in type-list
                        :collect `(typep ,param ',type)))))))

(defmethod %type-list-subtype-p ((type-1 (eql 'required)) (type-2 (eql 'required))
                                 list-1 list-2)
  (declare (optimize speed)
           (type list list-1 list-2))
  (and (length= list-1 list-2)
       (every #'subtypep list-1 list-2)))

(def-test type-list-subtype-required (:suite type-list-subtype-p)
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
