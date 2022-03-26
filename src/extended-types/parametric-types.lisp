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

(defun parametric-type-parameters (parametric-type-spec)
  (remove-duplicates
   (remove-if-not
    #'parametric-type-symbol-p
    (flatten (ensure-list parametric-type-spec)))))

;; TODO: Documentation

(defgeneric parametric-type-run-time-lambda-body (type-car type-cdr type-parameter)
  (:documentation
   "Users are expected to specialize on the TYPE-CAR using an (EQL symbol) specializer.
TYPE-CAR and TYPE-CDR together make up the parametric-type, while TYPE-PARAMETER
is one of the type parameter in the parametric-type.

The methods implemented should return a one-argument lambda-*expression* (not function).
The expression will be compiled to a function and called with the appropriate *object*
at run-time. The function should return the value of the TYPE-PARAMETER corresponding
to the *object* and the parametric type."))

(defgeneric parametric-type-compile-time-lambda  (type-car type-cdr type-parameter)
  (:documentation
   "Users are expected to specialize on the TYPE-CAR using an (EQL symbol) specializer.
TYPE-CAR and TYPE-CDR together make up the parametric-type, while TYPE-PARAMETER
is one of the type parameter in the parametric-type.

The methods implemented should return a one-argument lambda-*expression* (not function).
The expression will be compiled to a function and called with the appropriate
*form-type* at run-time. The function should return the value of the TYPE-PARAMETER
corresponding to the *form-type* and the parametric type."))


(defun type-parameters-from-parametric-type (parametric-type-spec)
  "Returns a list oF TYPE-PARAMETERS"
  (etypecase parametric-type-spec
    (atom
     (if (parametric-type-symbol-p parametric-type-spec)
         (list (make-type-parameter :name parametric-type-spec
                                    :run-time-deparameterizer-lambda-body
                                    `(cl:lambda (o) (type-of o))
                                    :compile-time-deparameterizer-lambda
                                    (cl:lambda (form-type) form-type)
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

(defmethod parametric-type-run-time-lambda-body
    ((type-car (eql 'array)) type-cdr parameter)
  (destructuring-bind (&optional (elt-type 'cl:*) (rank/dimensions 'cl:*)) type-cdr
    (cond ((eq parameter elt-type)
           `(cl:lambda (array) (array-element-type array)))
          ((symbolp rank/dimensions)
           (cond ((eq parameter rank/dimensions)
                  `(cl:lambda (array) (array-dimensions array)))
                 (t
                  (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                         parameter (cons type-car type-cdr)))))
          (t
           (loop :for s :in rank/dimensions
                 :for i :from 0
                 :do (cond ((eq parameter s)
                            (return-from parametric-type-run-time-lambda-body
                              `(cl:lambda (array) (array-dimension array ,i))))
                           (t
                            (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                                   parameter (cons type-car type-cdr)))))))))

(defmethod parametric-type-compile-time-lambda-body
    ((type-car (eql 'array)) type-cdr parameter)
  (destructuring-bind (&optional (elt-type 'cl:*) (rank/dimensions 'cl:*)) type-cdr
    `(cl:lambda (type)
       ,(cond ((eq parameter elt-type)
               `(if (and (listp type)
                         (or (eq 'array (first type))
                             (eq 'simple-array (first type)))
                         (not (eq 'cl:* (second type)))
                         (nthcdr 1 type))
                    (second type)
                    nil))
              (t
               `(if (and (listp type)
                         (or (eq 'array (first type))
                             (eq 'simple-array (first type)))
                         (not (eq 'cl:* (third type)))
                         (nthcdr 2 type))
                    ,(if (symbolp rank/dimensions)
                         `(cond ((eq 'cl:* (third type))
                                 nil)
                                ((listp (third type))
                                 (length (third type)))
                                (t
                                 (third type)))
                         (block loop-block
                           (loop :for s :in rank/dimensions
                                 :for i :from 0
                                 :do (cond ((eq parameter s)
                                            (return-from loop-block
                                              `(if (and (listp (third type))
                                                        (eq 'cl:* (nth ,i (third type))))
                                                   nil
                                                   (nth ,i (third type)))))
                                           (t
                                            (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                                                   parameter (cons type-car type-cdr)))))))
                    nil))))))
