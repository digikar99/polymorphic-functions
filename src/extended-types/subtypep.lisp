(in-package :polymorphic-functions.extended-types)

(pushnew 'subtypep *extended-type-specifiers*)

(defclass csubtypep (ctype)
  ((%type :initarg :type :reader csubtypep-type :type ctype::ctype)))

(defun csubtypep (type)
  (make-instance 'csubtypep :type type))

(defmethod cons-specifier-ctype ((head (eql 'subtypep)) rest env)
  (declare (ignorable env))
  (destructuring-bind (type) rest
    (csubtypep (specifier-ctype type env))))

(defmethod unparse ((ct csubtypep))
  `(subtypep ,(unparse (csubtypep-type ct))))

(defmethod upgraded-extended-type ((type-car (eql 'subtypep)))
  '(or list symbol))

(defmethod ctypep (object (ct csubtypep))
  (subctypep (csubtypep-type ct) (specifier-ctype object)))

(defmethod ctype:ctype= ((ct1 csubtypep) (ct2 csubtypep))
  (ctype:ctype= (csubtypep-type ct1)
                (csubtypep-type ct2)))

(defmethod subctypep ((ct1 csubtypep) (ct2 csubtypep))
  (subctypep (csubtypep-type ct1) (csubtypep-type ct2)))

(defmethod subctypep ((ct1 cmember) (ct2 csubtypep))
  (let ((specifier (csubtypep-type ct2)))
    (every (lambda (object)
             ;; Instead of converting to usual types, we instead convert
             ;; everything to CTYPE
             (subctypep specifier (specifier-ctype object)))
           (cmember-members ct1))))

(defmethod subctypep ((ct1 csubtypep) (ct2 (eql (ctype::top))))
  (values t t))

(defmethod subctypep ((ct1 csubtypep) ct2)
  (values nil nil))
