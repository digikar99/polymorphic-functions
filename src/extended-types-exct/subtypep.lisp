(in-package :polymorphic-functions.extended-types)

(define-compound-type (extensible-compound-types:subtypep :specializing nil)
    (symbol-or-list type-specifier)
  "Denotes the set of type specifiers that are a SUBTYPE of TYPE-SPECIFIER"
  (and (or (symbolp symbol-or-list)
           (listp   symbol-or-list))
       (type-specifier-p symbol-or-list)
       (subtypep symbol-or-list type-specifier)))

(defmethod %upgraded-cl-type ((name (eql 'extensible-compound-types:subtypep)) type &optional env)
  (declare (ignore env))
  (etypecase type
    (symbol 'symbol)
    (list   'list)))

(defmethod %subtypep ((n1 (eql 'extensible-compound-types:subtypep))
                      (n2 (eql 'extensible-compound-types:subtypep))
                      t1 t2 &optional env)
  (optima.extra:let-match (((list 'extensible-compound-types:subtypep ts1) t1)
                           ((list 'extensible-compound-types:subtypep ts2) t2))
    (multiple-value-bind (subtypep knownp) (subtypep ts1 ts2 env)
      (values subtypep knownp))))

(defmethod %subtypep ((n1 (eql 'member))
                      (n2 (eql 'extensible-compound-types:subtypep))
                      t1 t2 &optional env)
  (optima.extra:let-match (((list 'extensible-compound-types:subtypep specifier) t2)
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
