(in-package :polymorphic-functions)

(defvar *parametric-type-symbol-predicates* ()
  "If the type-specifier in the type-list of a polymorph is a list, then each symbol
in the list is tested against the functions (predicates) in this list. The
type-specifier aka list qualifies as a parametric-type-specifier if at least one
predicate returns non-NIL for at least one symbol.")

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

(defun parametric-type-parameters (parametric-type-spec)
  (remove-duplicates
   (remove-if-not
    #'parametric-type-symbol-p
    (flatten (ensure-list parametric-type-spec)))))

(defun ensure-type-form (type form)
  (if (parametric-type-specifier-p type)
      (let ((type (traverse-tree type
                                 (lambda (node)
                                   (etypecase node
                                     (atom node)
                                     (list
                                      ;; QUOTE cannot be used in a parametric-type expression
                                      (if (eq 'quote (first node))
                                          node
                                          (cons 'list
                                                (loop :for item :in node
                                                      :collect
                                                      (etypecase item
                                                        (atom
                                                         (if (parametric-type-symbol-p item)
                                                             item
                                                             (list 'quote item)))
                                                        (list item)))))))))))
        (with-gensyms (form-value)
          `(let ((,form-value ,form))
             (assert (typep ,form-value ,type)
                     ()
                     'type-error
                     :expected-type ,type
                     :datum ,form-value)
             ,form-value)))
      ;; If we sometime decide to emit the LET form in both cases (since THE has
      ;; no guarantee, apparantly), do handle the VALUES types correctly!
      `(the ,type ,form)))

;; TODO: Documentation

(defgeneric parametric-type-run-time-lambda-body (type-car type-cdr type-parameter))
(defgeneric parametric-type-compile-time-lambda  (type-car type-cdr type-parameter))

(defun type-parameters-from-parametric-type (parametric-type-spec)
  "Returns a list oF TYPE-PARAMETERS"
  (etypecase parametric-type-spec
    (atom
     (if (parametric-type-symbol-p parametric-type-spec)
         (list (make-type-parameter :name parametric-type-spec
                                    :run-time-deparameterizer-lambda-body
                                    `(lambda (o) (type-of o))
                                    :compile-time-deparameterizer-lambda
                                    (lambda (form-type) form-type)
                                    :compile-time-deparameterizer-lambda-body
                                    `(lambda (form-type) form-type)))
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

(defmethod parametric-type-run-time-lambda-body
    ((type-car (eql 'array)) type-cdr parameter)
  (destructuring-bind (&optional (elt-type 'cl:*) (rank/dimensions 'cl:*)) type-cdr
    (declare (ignore rank/dimensions))
    (cond ((eq parameter elt-type)
           `(lambda (array) (array-element-type array)))
          (t
           (error "This case has not been handled!")))))

(defmethod parametric-type-compile-time-lambda-body
    ((type-car (eql 'array)) type-cdr parameter)
  (destructuring-bind (&optional (elt-type 'cl:*) (rank/dimensions 'cl:*)) type-cdr
    (declare (ignore rank/dimensions))
    (cond ((eq parameter elt-type)
           `(lambda (type)
              (if (and (listp type)
                       (or (eq 'array (first type))
                           (eq 'simple-array (first type)))
                       (not (eq 'cl:* (second type))))
                  (second type)
                  nil)))
          (t
           (error "This case has not been handled!")))))

(defmethod upgrade-parametric-type ((type-car (eql 'array)) type-cdr)
  'cl:array)
