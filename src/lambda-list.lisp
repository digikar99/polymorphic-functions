(in-package typed-dispatch)

(defun valid-parameter-name-p (name)
  (and (symbolp name)
       (not (eq t name))
       (not (eq nil name))))

(defun typed-lambda-list-p (list)
  "Returns four values:
  the first value indicates whether the given LIST is a TYPED-LAMBDA-LIST,
  the second is a UNTYPED-LAMBDA-LIST corresponding to the given LIST,
  the third is a LIST of TYPEs corresponding to the given LIST,
  the fourth is a List of PARAMETERs corresponding to the third value."
  (declare (type list list))
  (let ((typed-lambda-list-p   t)
        (untyped-lambda-list nil)
        (type-list           nil)
        (typed-param-list    nil))
    (macrolet ((update-with-and (obj)
                 `(if typed-lambda-list-p
                      (setq typed-lambda-list-p
                            (and typed-lambda-list-p ,obj))))
               (next (list)
                 `(setq ,list (cdr ,list))))
      ;; first process required args
      (loop :while (and typed-lambda-list-p list)
            :for param := (first list)
            :until (member param lambda-list-keywords)
            :do (update-with-and (listp param))
                (update-with-and (valid-parameter-name-p (first param)))
                (update-with-and (type-specifier-p (second param)))
                (update-with-and (null (cddr param))) ; max-length 2
                (when typed-lambda-list-p
                  (push (first param) untyped-lambda-list)
                  (push (first param) typed-param-list)
                  (push (second param) type-list))
                (next list))
      ;; optionally the &optional args
      (when (eq '&optional (first list)) 
        (push (first list) untyped-lambda-list)
        (next list)
        (loop :while (and typed-lambda-list-p list)
              :for param := (first list)
              :until (member param lambda-list-keywords)
              :do (update-with-and (listp param))
                  (update-with-and (listp (first param)))
                  (update-with-and (null (cdddr param))) ; max-length 3
                  (update-with-and (destructuring-bind ((name type)
                                                        &optional default-value given-p)
                                       param
                                     (and (valid-parameter-name-p name)
                                          (type-specifier-p type)
                                          ;; TODO: Signal a better error with typing
                                          (typep default-value type)
                                          (valid-parameter-name-p given-p))))
                  (when typed-lambda-list-p
                    (push (first (first param))
                          untyped-lambda-list)
                    (push (first (first param))
                          typed-param-list)
                    (push (second (first param))
                          type-list))
                  (next list)))
      (update-with-and (handler-case (progn
                                       (parse-ordinary-lambda-list list)
                                       t)
                         (program-error (c)
                           (declare (ignore c))
                           nil)))
      (when typed-lambda-list-p (setq untyped-lambda-list
                                      (append untyped-lambda-list
                                              list))))
    (check-type untyped-lambda-list untyped-lambda-list)
    (values typed-lambda-list-p
            untyped-lambda-list
            (nreverse type-list)
            (nreverse typed-param-list))))

(deftype typed-lambda-list ()
  "Examples:
  ((a integer) (b integer))
  ((a integer) &optional ((b integer) 0 b-supplied-p))"
  `(satisfies typed-lambda-list-p))

(defun untyped-lambda-list-p (list)
  "Returns two values: 
  the first value indicates if the given LIST is an UNTYPED-LAMBDA-LIST,
  if the first value is T, that is, if the LIST is an UNTYPED-LAMBDA-LIST, the second value is the list of PARAMETERs that will be considered for typed dispatch."
  (declare (type list list))
  (let ((untyped-lambda-list-p t)
        (typed-param-list    nil))
    (macrolet ((update-with-and (obj)
                 `(if untyped-lambda-list-p
                      (setq untyped-lambda-list-p
                            (and untyped-lambda-list-p ,obj))))
               (next (list)
                 `(setq ,list (cdr ,list))))
      ;; first process required args
      (loop :while (and untyped-lambda-list-p list)
            :for param := (first list)
            :until (member param lambda-list-keywords)
            :do (update-with-and (and (symbolp param)
                                      (not (eq t param))))
                (when untyped-lambda-list-p
                  (push param typed-param-list))
                (next list))
      ;; optionally the &optional args
      (when (eq '&optional (first list))
        (next list)
        (loop :while (and untyped-lambda-list-p list)
              :for param := (first list)
              :until (member param lambda-list-keywords)
              :do (update-with-and (valid-parameter-name-p param))
                  (when untyped-lambda-list-p
                    (push param typed-param-list))
                  (next list)))
      (update-with-and (handler-case (progn
                                       (parse-ordinary-lambda-list list)
                                       t)
                         (program-error (c)
                           (declare (ignore c))
                           nil))))
    (values untyped-lambda-list-p
            (nreverse typed-param-list))))

(deftype untyped-lambda-list ()
  "Examples:
  (a b)
  (a b &optional c)
Non-examples:
  ((a string))"
  `(satisfies untyped-lambda-list-p))

