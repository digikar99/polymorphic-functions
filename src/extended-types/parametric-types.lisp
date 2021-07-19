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
  `(satisfiers parametric-type-specifier-p))

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

(defgeneric parametric-type-run-time-lambda-body (type-car type-cdr type-parameter)
  (:documentation
   "Users are expected to specialize on the TYPE-CAR using an (EQL symbol) specializer.
TYPE-CAR and TYPE-CDR together make up the parametric-type, while TYPE-PARAMETER
is one of the type parameter in the parametric-type.

The methods implemented should return a one-argument lambda-*expression* (not function).
The expression will be compiled to a function and called with the appropriate *object*
at run-time. The function should return the value of the TYPE-PARAMTER corresponding
to the *object* and the parametric type."))

(defgeneric parametric-type-compile-time-lambda  (type-car type-cdr type-parameter)
  (:documentation
   "Users are expected to specialize on the TYPE-CAR using an (EQL symbol) specializer.
TYPE-CAR and TYPE-CDR together make up the parametric-type, while TYPE-PARAMETER
is one of the type parameter in the parametric-type.

The methods implemented should return a one-argument lambda-*expression* (not function).
The expression will be compiled to a function and called with the appropriate
*form-type* at run-time. The function should return the value of the TYPE-PARAMTER
corresponding to the *form-type* and the parametric type."))

(defun deparameterize-type (type-specifier)
  (etypecase type-specifier
    (atom (if (parametric-type-symbol-p type-specifier)
              t
              type-specifier))
    (list (traverse-tree type-specifier (lambda (node)
                                          (etypecase node
                                            (atom (if (parametric-type-symbol-p node)
                                                      'cl:*
                                                      node))
                                            (list node)))))))

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
    (cond ((eq parameter elt-type)
           `(lambda (array) (array-element-type array)))
          ((symbolp rank/dimensions)
           (cond ((eq parameter rank/dimensions)
                  `(lambda (array) (array-dimensions array)))
                 (t
                  (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                         parameter (cons type-car type-cdr)))))
          (t
           (loop :for s :in rank/dimensions
                 :for i :from 0
                 :do (cond ((eq parameter s)
                            (return-from parametric-type-run-time-lambda-body
                              `(lambda (array) (array-dimension array ,i))))
                           (t
                            (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                                   parameter (cons type-car type-cdr)))))))))

(defmethod parametric-type-compile-time-lambda-body
    ((type-car (eql 'array)) type-cdr parameter)
  (destructuring-bind (&optional (elt-type 'cl:*) (rank/dimensions 'cl:*)) type-cdr
    `(lambda (type)
       (if (and (listp type)
                (or (eq 'array (first type))
                    (eq 'simple-array (first type)))
                (not (eq 'cl:* (second type))))
           ,(cond ((eq parameter elt-type)
                   `(second type))
                  ((symbolp rank/dimensions)
                   `(third type))
                  (t
                   (block loop-block
                     (loop :for s :in rank/dimensions
                           :for i :from 0
                           :do (cond ((eq parameter s)
                                      (return-from loop-block
                                        `(nth ,i (third type))))
                                     (t
                                      (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                                             parameter (cons type-car type-cdr))))))))))))
