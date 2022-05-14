(in-package :polymorphic-functions.extended-types)

(pushnew 'type= *extended-type-specifiers*)

(defclass ctype= (ctype)
  ((%type :initarg :type :reader ctype=-type :type ctype::ctype)))

(defun ctype= (type)
  (make-instance 'ctype= :type type))

(defmethod cons-specifier-ctype ((head (eql 'type=)) rest env)
  (declare (ignorable env))
  (destructuring-bind (type) rest
    (ctype= (specifier-ctype type env))))

(defmethod unparse ((ct ctype=))
  `(type= ,(unparse (ctype=-type ct))))

(defmethod upgraded-extended-type ((type-car (eql 'type=)))
  '(or list symbol))

(defmethod ctypep (object (ct ctype=))
  (if (type-specifier-p object)
      (ctype:ctype= (ctype=-type ct) (specifier-ctype object))
      nil))

(defmethod ctype:ctype= ((ct1 ctype=) (ct2 ctype=))
  (ctype:ctype= (ctype=-type ct1)
                (ctype=-type ct2)))

(defmethod subctypep ((ct1 ctype=) (ct2 ctype=))
  (ctype:ctype= (ctype=-type ct1) (ctype=-type ct2)))

(defmethod conjoin/2 ((ct1 ctype=) (ct2 ctype=))
  (if (ctype:ctype= ct1 ct2)
      ct1
      (ctype:specifier-ctype nil)))

(defmethod subctypep ((ct1 ctype=) (ct2 ctype:disjunction))
  (if (null (ctype:junction-ctypes ct2))
      (values nil t)
      (error "Unhandled case!")))

(defmethod subctypep ((ct1 cmember) (ct2 ctype=))
  (let ((specifier (ctype=-type ct2)))
    (every (lambda (object)
             ;; Instead of converting to usual types, we instead convert
             ;; everything to CTYPE
             (if (type-specifier-p object)
                 (ctype:ctype= specifier (specifier-ctype object))
                 (return-from subctypep (values nil t))))
           (cmember-members ct1))))

(defmethod subctypep ((ct1 ctype=) (ct2 (eql (ctype::top))))
  (values t t))

(defmethod subctypep ((ct1 ctype=) ct2)
  (values nil nil))
