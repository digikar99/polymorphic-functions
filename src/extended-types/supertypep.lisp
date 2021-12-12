(in-package :polymorphic-functions.extended-types)

;; Suppose we have a polymorph for coercing to a type denoted by
;; type-specifier TS1, and another polymorph for coercing to a type
;; denoted by type-specifier TS2. Suppose TS2 is a subtype of TS1.

;; - Now, if asked to coerce to TS1, it is perfectly valid to coerce
;;   to TS2. In fact, the polymorph corresponding to TS2 can be called
;;   for any specifier that is a *supertype* of TS2. Similarly, the
;;   polymorph corresponding to TS1 can be called for any specifier
;;   that is a *supertype* of TS1.

;; - In addition, the set of type specifiers that are supertypes of
;;   TS1 is a subset of the set of type specifiers that are supertypes
;;   of TS2. In other words, (subtypep '(supertypep TS1) '(supertypep
;;   TS2)) is T, which means dispatching on the most specialized
;;   polymorph is the same as dispatching on the polymorph corresponding
;;   to TS1. And this is consistent with what @Commander_Trashdin just
;;   said.

;; However, the trouble of using SUPERTYPEP for TRIVIAL-COERCE:COERCE
;; include that coercions with (supertypep 'vector) and (supertypep 'list)
;; prove to be ambiguous if the argument (OUTPUT-TYPE-SPEC) is 'sequence.

(pushnew 'supertypep *extended-type-specifiers*)

(defclass csupertypep (ctype)
  ((%type :initarg :type :reader csupertypep-type :type ctype::ctype)))

(defun csupertypep (type)
  (make-instance 'csupertypep :type type))

(defmethod cons-specifier-ctype ((head (eql 'supertypep)) rest env)
  (declare (ignorable env))
  (destructuring-bind (type) rest
    (csupertypep (specifier-ctype type env))))

(defmethod unparse ((ct csupertypep))
  `(supertypep ,(unparse (csupertypep-type ct))))

(defmethod upgraded-extended-type ((type-car (eql 'supertypep)))
  '(or list symbol))

(defmethod ctypep (object (ct csupertypep))
  (if (type-specifier-p object)
      (subctypep (csupertypep-type ct) (specifier-ctype object))
      nil))

(defmethod ctype:ctype= ((ct1 csupertypep) (ct2 csupertypep))
  (ctype:ctype= (csupertypep-type ct1)
                (csupertypep-type ct2)))

(defmethod subctypep ((ct1 csupertypep) (ct2 csupertypep))
  (subctypep (csupertypep-type ct2) (csupertypep-type ct1)))

(defmethod conjoin/2 ((ct1 csupertypep) (ct2 csupertypep))
  (csupertypep (specifier-ctype
                `(or ,(unparse (csupertypep-type ct1))
                     ,(unparse (csupertypep-type ct2))))))

(defmethod subctypep ((ct1 csupertypep) (ct2 ctype:disjunction))
  (if (null (ctype:junction-ctypes ct2))
      (values nil t)
      (error "Unhandled case!")))

(defmethod subctypep ((ct1 cmember) (ct2 csupertypep))
  (let ((specifier (csupertypep-type ct2)))
    (every (lambda (object)
             ;; Instead of converting to usual types, we instead convert
             ;; everything to CTYPE
             (if (type-specifier-p object)
                 (subctypep specifier (specifier-ctype object))
                 (return-from subctypep (values nil t))))
           (cmember-members ct1))))

(defmethod subctypep ((ct1 csupertypep) (ct2 (eql (ctype::top))))
  (values t t))

(defmethod subctypep ((ct1 csupertypep) ct2)
  (values nil nil))
