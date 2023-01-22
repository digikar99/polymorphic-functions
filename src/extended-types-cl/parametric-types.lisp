(in-package :polymorphic-functions)

(defvar *parametric-type-symbol-predicates* ()
  "A type-specifier in the type-list of a polymorph qualifies as parametric-type-specifier
if there exists a symbol in the list, which when tested against the functions (predicates)
in this list, returns non-NIL for at least one predicate")

(defun parametric-type-symbol-p (atom)
  (and (symbolp atom)
       (some (lambda (fn)
               (funcall fn atom))
             *parametric-type-symbol-predicates*)))

(defun parametric-type-specifier-p (type-specifier)
  (and (or (proper-list-p type-specifier)
           (symbolp type-specifier))
       (flet ((%parametric-type-symbol-p (node)
                (etypecase node
                  (list node)
                  (atom (if (parametric-type-symbol-p node)
                            (return-from parametric-type-specifier-p t)
                            node)))))
         (traverse-tree type-specifier #'%parametric-type-symbol-p)
         nil)))

(deftype parametric-type-specifier ()
  `(satisfies parametric-type-specifier-p))

(defun parametric-type-specifiers (type-list)
  (declare (type type-list type-list))
  (let ((parametric-types nil)
        (list type-list))
    (loop :for elt := (first list)
          :while list     ; we don't want list to be empty
          :until (member elt '(&key &rest))
          :do (when (parametric-type-specifier-p elt)
                (push elt parametric-types))
              (setq list (rest list)))
    (when (and list
               (eq '&key (first list)))
      (loop :for param-type :in (rest list)
            :do (let ((type (optima:match param-type
                              ((list (list _ type))
                               type)
                              ((list (list _ type) _)
                               `(or ,type null))
                              ((list _ _)
                               t)
                              (_ t))))
                  (when (parametric-type-specifier-p type)
                    (push type parametric-types)))))
    parametric-types))

(defun parametric-type-specifiers-are-significant-p (type-list)
  "Returns T if there exist at least two parametric-type-specifiers in the type-list
which have type parameters that depend on each other."
  (declare (type type-list type-list))
  (let* ((parametric-types (parametric-type-specifiers type-list))
         (parametric-type-parameters
           (mapcar #'parametric-type-parameters parametric-types)))
    (loop :for (p1 . remaining) :on parametric-type-parameters
          :with intersect-p := nil
          :do (loop :for p2 :in remaining
                    :do (when (intersection p1 p2)
                          (return-from parametric-type-specifiers-are-significant-p t)))
          :finally (return-from parametric-type-specifiers-are-significant-p nil))))

(defun parametric-type-parameters (parametric-type-spec)
  (remove-duplicates
   (remove-if-not
    #'parametric-type-symbol-p
    (flatten (ensure-list parametric-type-spec)))))

(defvar *ocs-run-time-lambda-body-lambda-table* (make-hash-table))

(defun parametric-type-run-time-lambda-body (type-car type-cdr type-parameter)
  (assert (orthogonally-specializing-type-specifier-p
           (extensible-compound-types:typexpand
            (deparameterize-type `(,type-car ,@type-cdr)))))
  (let* ((type-spec  (extensible-compound-types:typexpand `(,type-car ,@type-cdr)))
         (class-name (second type-spec)))
    (unless (nth-value 1 (gethash class-name *ocs-run-time-lambda-body-lambda-table*))
      (setf (gethash class-name *ocs-run-time-lambda-body-lambda-table*)
            (compile
             nil
             (let* ((ocs (extensible-compound-types.impl::class-specializer class-name))
                    (slots (extensible-compound-types.impl::ocs-slots ocs))
                    (arg-list (extensible-compound-types.impl::ocs-arg-list ocs)))
               (with-gensyms (p o)
                 `(cl:lambda (,p ,@arg-list)
                    (cond
                      ,@(loop :for slot :in slots
                              :nconcing
                              (destructuring-bind
                                  (name &key accessor nested)
                                  slot
                                (if nested
                                    `(((and (atom ,name)
                                            (equal ,name ,p))
                                       ',accessor)
                                      ((and (consp ,name)
                                            (position ,p (flatten ,name)))
                                       `(cl:lambda (,',o)
                                          ,(let ((,name (flatten ,name)))
                                             `(nth ,(position ,p ,name)
                                                   (flatten (,',accessor ,',o)))))))
                                    `(((and (atom ,name)
                                            (equal ,name ,p))
                                       ',accessor))))))))))))
    (apply (gethash class-name *ocs-run-time-lambda-body-lambda-table*)
           type-parameter
           (nthcdr 2 type-spec))))

(defun parametric-type-compile-time-lambda-body (type-car type-cdr type-parameter)
  (assert (orthogonally-specializing-type-specifier-p
           (extensible-compound-types:typexpand
            (deparameterize-type `(,type-car ,@type-cdr)))))
  (with-gensyms (object-type p a)
    (let* ((type-pattern (extensible-compound-types:typexpand `(,type-car ,@type-cdr))))
      `(cl:lambda (,object-type)
         (let ((,object-type (flatten (extensible-compound-types:typexpand ,object-type))))
           (if (not (eq (second ,object-type)
                        ',(second type-pattern)))
               nil
               (loop :for ,a :in (rest ,object-type)
                     :for ,p :in ',(flatten (rest type-pattern))
                     :do (when (eq ,p ',type-parameter)
                           (return ,a)))))))))

(defun type-parameters-from-parametric-type (parametric-type-spec)
  "Returns a list oF TYPE-PARAMETERS"
  (etypecase parametric-type-spec
    (atom
     (if (parametric-type-symbol-p parametric-type-spec)
         (list (make-type-parameter :name parametric-type-spec
                                    :run-time-deparameterizer-lambda-body
                                    `(cl:lambda (o) (class-name (class-of o)))
                                    :compile-time-deparameterizer-lambda
                                    (lambda (form-type) form-type)
                                    :compile-time-deparameterizer-lambda-body
                                    `(cl:lambda (form-type) form-type)))
         nil))
    (list
     (let* ((type-car (car parametric-type-spec))
            (type-parameter-names (parametric-type-parameters parametric-type-spec)))
       (mapcar (lambda (type-parameter-name)
                 (let ((compiler-body (parametric-type-compile-time-lambda-body
                                       type-car
                                       (cdr parametric-type-spec)
                                       type-parameter-name)))
                   (make-type-parameter :name type-parameter-name
                                        :run-time-deparameterizer-lambda-body
                                        (parametric-type-run-time-lambda-body
                                         type-car
                                         (cdr parametric-type-spec)
                                         type-parameter-name)
                                        :compile-time-deparameterizer-lambda-body
                                        compiler-body
                                        :compile-time-deparameterizer-lambda
                                        (compile nil compiler-body))))
               type-parameter-names)))))
