(in-package :polymorphic-functions.extended-types)

(define-compound-type extensible-compound-types:type= (object type-specifier)
  "Denotes the set of type specifiers that are TYPE= to TYPE-SPECIFIER"
  (and (or (symbolp object)
           (listp   object))
       (type-specifier-p object)
       (type= object type-specifier)))

(defmethod %upgraded-cl-type ((name (eql 'extensible-compound-types:type=)) type &optional env)
  (declare (ignore env))
  (etypecase type
    (symbol 'symbol)
    (list   'list)))

(defmethod %subtypep ((n1 (eql 'extensible-compound-types:type=))
                      (n2 (eql 'extensible-compound-types:type=))
                      t1 t2 &optional env)
  (optima.extra:let-match (((list 'extensible-compound-types:type= ts1) t1)
                           ((list 'extensible-compound-types:type= ts2) t2))
    (multiple-value-bind (type= knownp) (type= ts1 ts2 env)
      (values type= knownp))))

(defmethod %subtypep ((n1 (eql 'member))
                      (n2 (eql 'extensible-compound-types:type=))
                      t1 t2 &optional env)
  (optima.extra:let-match (((list 'extensible-compound-types:type= specifier) t2)
                           ((list* 'member objects) t1))
    (values (every (lambda (object)
                     (if (type-specifier-p object)
                         (multiple-value-bind (type= knownp)
                             (type= specifier object env)
                           (if knownp
                               type=
                               (return-from %subtypep (values nil nil))))
                         (return-from %subtypep (values nil t))))
                   objects)
            t)))

(defmethod %subtypep ((n1 (eql 'extensible-compound-types:type=))
                              n2 t1 (t2 symbol) &optional env)
  (declare (ignore n1 n2 t1 t2 env))
  (values nil t))

(defmethod %subtypep (n1 (n2 (eql 'extensible-compound-types:type=))
                              (t1 symbol) t2 &optional env)
  (declare (ignore n1 n2 t1 t2 env))
  (values nil t))

(defmethod %intersect-type-p ((n1 (eql 'extensible-compound-types:type=))
                              (n2 (eql 'extensible-compound-types:type=))
                              t1 t2 &optional env)
  (optima.extra:let-match (((list 'extensible-compound-types:type= ts1) t1)
                           ((list 'extensible-compound-types:type= ts2) t2))
    (multiple-value-bind (type= knownp) (type= ts1 ts2 env)
      (values type= knownp))))

(defmethod %intersect-type-p ((n1 (eql 'extensible-compound-types:type=))
                              n2 t1 (t2 symbol) &optional env)
  (declare (ignore n1 n2 t1 t2 env))
  (values nil t))

(defmethod %intersect-type-p (n1 (n2 (eql 'extensible-compound-types:type=))
                              (t1 symbol) t2 &optional env)
  (declare (ignore n1 n2 t1 t2 env))
  (values nil t))
