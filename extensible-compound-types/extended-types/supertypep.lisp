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

(define-compound-type symbol-or-list-supertype-p (symbol-or-list type-specifier)
  "Denotes the set of type specifiers that are a SUPERTYPE of TYPE-SPECIFIER"
  (and (or (symbolp symbol-or-list)
           (listp   symbol-or-list))
       (type-specifier-p symbol-or-list)
       (supertypep symbol-or-list type-specifier)))

(deftype extensible-compound-types:supertypep (type-specifier)
  "Denotes the set of type specifiers that are a SUPERTYPE of TYPE-SPECIFIER"
  `(and (or symbol list)
        (symbol-or-list-supertype-p ,type-specifier)))

(defmethod %upgraded-cl-type ((name (eql 'symbol-or-list-supertype-p)) type &optional env)
  (declare (ignore env))
  (etypecase type
    (symbol 'symbol)
    (list   'list)))

(defmethod %subtypep ((n1 (eql 'symbol-or-list-supertype-p))
                      (n2 (eql 'symbol-or-list-supertype-p))
                      t1 t2 &optional env)
  (optima.extra:let-match (((list 'symbol-or-list-supertype-p ts1) t1)
                           ((list 'symbol-or-list-supertype-p ts2) t2))
    (multiple-value-bind (subtypep knownp) (subtypep ts2 ts1 env)
      (values subtypep knownp))))

(defmethod %subtypep ((n1 (eql 'member))
                      (n2 (eql 'symbol-or-list-supertype-p))
                      t1 t2 &optional env)
  (optima.extra:let-match (((list 'symbol-or-list-supertype-p specifier) t2)
                           ((list* 'member objects) t1))
    (values (every (lambda (object)
                     (if (type-specifier-p object)
                         (multiple-value-bind (supertypep knownp)
                             (supertypep object specifier env)
                           (if knownp
                               supertypep
                               (return-from %subtypep (values nil nil))))
                         (return-from %subtypep (values nil t))))
                   objects)
            t)))
