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
              (mapcar 'second lambda-list))
      (copy-list lambda-list)))

(defmethod %defun-body ((type (eql 'required)) (defun-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  `(funcall (nth-value 1 (retrieve-polymorph ',*name* ,@defun-lambda-list))
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

(defmethod type-list-applicable-p ((type (eql 'required)) (arg-list list) (type-list list))
  (every 'our-typep arg-list type-list))
