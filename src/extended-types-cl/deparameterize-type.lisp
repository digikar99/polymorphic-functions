(in-package :polymorphic-functions)

(defvar *deparameterizer-alist* nil
  "(Re)Bound in PF-COMPILER-MACRO, modified by ENHANCED-LAMBDA-DECLARATIONS.")

;; FIXME: Clarify the above usage

(defun deparameterize-type (type-specifier)
  (etypecase type-specifier
    (atom (if (parametric-type-symbol-p type-specifier)
              (or (assoc-value *deparameterizer-alist* type-specifier)
                  t)
              type-specifier))
    (list (%deparameterize-type (car type-specifier) type-specifier))))

(defgeneric %deparameterize-type (type-specifier-car type-specifier &optional env)
  (:documentation "%DEPARAMETERIZE-TYPE is called when the argument to DEPARAMETERIZE-TYPE is a list."))

(defmethod %deparameterize-type (car-type-specifier type-specifier &optional env)
  (declare (ignore car-type-specifier env))
  (etypecase type-specifier
    (atom (if (parametric-type-symbol-p type-specifier)
              (or (assoc-value *deparameterizer-alist* type-specifier)
                  t)
              type-specifier))
    (list (traverse-tree type-specifier (lambda (node)
                                          (etypecase node
                                            (atom (if (parametric-type-symbol-p node)
                                                      (or (assoc-value *deparameterizer-alist* node)
                                                          'cl:*)
                                                      node))
                                            (list node)))))))

(defun simple-deparameterize-type (type-specifier &optional env)
  (declare (ignore env))
  `(,(first type-specifier)
    ,@(loop :for type :in (rest type-specifier)
            :collect (etypecase type
                       (atom (if (parametric-type-symbol-p type)
                                 (or (assoc-value *deparameterizer-alist* type)
                                     t)
                                 type))
                       (list (%deparameterize-type (car type) type))))))

(defmethod %deparameterize-type ((car (eql 'and)) type-specifier &optional env)
  (simple-deparameterize-type type-specifier env))

(defmethod %deparameterize-type ((car (eql 'or)) type-specifier &optional env)
  (simple-deparameterize-type type-specifier env))

(defmethod %deparameterize-type ((car (eql 'eql)) type-specifier &optional env)
  (simple-deparameterize-type type-specifier env))

(defmethod %deparameterize-type ((car (eql 'member)) type-specifier &optional env)
  (simple-deparameterize-type type-specifier env))

(defmethod %deparameterize-type ((car (eql 'values)) type-specifier &optional env)
  (simple-deparameterize-type type-specifier env))

(defmethod %deparameterize-type ((car (eql 'function)) fun-type-specifier &optional env)
  (declare (ignore env))
  (destructuring-bind (parameters return-type) (rest fun-type-specifier)
    `(function ,(cond ((and (atom parameters)
                            (eq 'cl:* parameters))
                       'cl:*)
                      ((listp parameters)
                       (loop :for typespec :in parameters
                             :with state := :required
                             :collect (let ((deparameterized-typespec
                                              (ecase state
                                                ((:required &optional &rest)
                                                 (etypecase typespec
                                                   (atom (if (parametric-type-symbol-p typespec)
                                                             (or (assoc-value
                                                                  *deparameterizer-alist*
                                                                  typespec)
                                                                 t)
                                                             typespec))
                                                   (list (%deparameterize-type (car typespec) typespec))))
                                                ((&key)
                                                 `(,(first typespec)
                                                   ,(let ((typespec (second typespec)))
                                                      (etypecase typespec
                                                        (atom (if (parametric-type-symbol-p typespec)
                                                                  (or (assoc-value
                                                                       *deparameterizer-alist*
                                                                       typespec)
                                                                      t)
                                                                  typespec))
                                                        (list
                                                         (%deparameterize-type (car typespec)
                                                                               typespec)))))))))
                                        (when (member typespec lambda-list-keywords)
                                          (setq state typespec))
                                        deparameterized-typespec)))
                      (t
                       (error "Unexpected parameters ~S" parameters)))
               ,(etypecase return-type
                  (atom (if (parametric-type-symbol-p return-type)
                            (or (assoc-value *deparameterizer-alist* return-type)
                                t)
                            return-type))
                  (list (%deparameterize-type (car return-type) return-type))))))

(let ((array-element-types (loop :for type
                                   :in '(single-float
                                         double-float
                                         (signed-byte 08)
                                         (signed-byte 16)
                                         (signed-byte 32)
                                         (signed-byte 64)
                                         (unsigned-byte 08)
                                         (unsigned-byte 16)
                                         (unsigned-byte 32)
                                         (unsigned-byte 64)
                                         fixnum)
                                 :if (not (type= t type))
                                   :collect (upgraded-array-element-type type))))

  (defun deparameterize-array-type (car type-specifier &optional env)
    (declare (ignore env))
    (if (not (parametric-type-specifier-p type-specifier))
        type-specifier
        (destructuring-bind (&optional (elt-type 'cl:*) (dim/rank 'cl:*))
            (rest type-specifier)
          (flet ((identity-or-* (object)
                   (if (parametric-type-symbol-p object)
                       'cl:*
                       object)))
            (let ((dim/rank (etypecase dim/rank
                              (list (mapcar #'identity-or-* dim/rank))
                              (atom (identity-or-* dim/rank)))))
              (if (parametric-type-symbol-p elt-type)
                  `(or ,@(loop :for type :in array-element-types
                               :collect `(,car ,type ,dim/rank)))
                  `(,car ,elt-type ,dim/rank)))))))

  (defmethod %deparameterize-type ((car (eql 'array)) type-specifier &optional env)
    (deparameterize-array-type car type-specifier env))

  (defmethod %deparameterize-type ((car (eql 'simple-array)) type-specifier &optional env)
    (deparameterize-array-type car type-specifier env)))
