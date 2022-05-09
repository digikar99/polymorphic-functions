(in-package :polymorphic-functions.extended-types)

(define-compound-type symbol-or-list-subtype-p (symbol-or-list type-specifier)
  "Denotes the set of type specifiers that are a SUBTYPE of TYPE-SPECIFIER"
  (and (or (symbolp symbol-or-list)
           (listp   symbol-or-list))
       (type-specifier-p symbol-or-list)
       (subtypep symbol-or-list type-specifier)))

(deftype extensible-compound-types:subtypep (type-specifier)
  "Denotes the set of type specifiers that are a SUBTYPE of TYPE-SPECIFIER"
  `(and (or symbol list)
        (symbol-or-list-subtype-p ,type-specifier)))

(defmethod %upgraded-cl-type ((name (eql 'symbol-or-list-subtype-p)) type &optional env)
  (declare (ignore env))
  (etypecase type
    (symbol 'symbol)
    (list   'list)))

(defmethod %subtypep ((n1 (eql 'symbol-or-list-subtype-p))
                      (n2 (eql 'symbol-or-list-subtype-p))
                      t1 t2 &optional env)
  (optima.extra:let-match (((list 'symbol-or-list-subtype-p ts1) t1)
                           ((list 'symbol-or-list-subtype-p ts2) t2))
    (multiple-value-bind (subtypep knownp) (subtypep ts1 ts2 env)
      (values subtypep knownp))))

(defmethod %subtypep ((n1 (eql 'member))
                      (n2 (eql 'symbol-or-list-subtype-p))
                      t1 t2 &optional env)
  (optima.extra:let-match (((list 'symbol-or-list-subtype-p specifier) t2)
                           ((list* 'member objects) t1))
    (values (every (lambda (object)
                     (if (type-specifier-p object)
                         (multiple-value-bind (subtypep knownp)
                             (subtypep object specifier env)
                           (if knownp
                               subtypep
                               (return-from %subtypep (values nil nil))))
                         (return-from %subtypep (values nil t))))
                   objects)
            t)))
