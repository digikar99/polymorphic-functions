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
*form-type* at compile-time. The function should return the value of the TYPE-PARAMETER
corresponding to the parametric type in the *form-type*.

If the *form-type* does not match the parametric-type, then NIL may be returned."))


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

(defun parametric-type-run-time-lambda-body-for-array
    (type-car type-cdr parameter)
  `(cl:lambda (array)
     ,(optima:match type-cdr
        ((list* (eql parameter) _)
         `(array-element-type array))
        ((list _ (eql parameter))
         `(array-rank array))
        ((optima:guard (list _ dimensions)
                       (position parameter dimensions))
         `(array-dimension array ,(position parameter dimensions)))
        (otherwise
         (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                parameter (cons type-car type-cdr))))))

(defun parametric-type-compile-time-lambda-body-for-array
    (type-car type-cdr parameter)
  `(cl:lambda (type)
     (optima:match type
       ((optima:guard (list* ft-type-car _)
                      (subtypep ft-type-car ',type-car))
        ,(optima:match type-cdr
           ((list* (eql parameter) _)
            `(let ((elt-type (array-type-element-type type)))
               (if (eq elt-type 'cl:*)
                   nil
                   elt-type)))
           ((list _ (eql parameter))
            `(let ((rank (array-type-rank type)))
               (if (eq rank 'cl:*)
                   nil
                   rank)))
           ((optima:guard (list _ dimensions)
                          (position parameter dimensions))
            (let ((pos (position parameter dimensions)))
              `(let ((dimension (nth ,pos (array-type-dimensions type))))
                 (if (eq dimension 'cl:*)
                     nil
                     dimension))))
           (otherwise
            (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                   parameter (cons type-car type-cdr)))))
       (otherwise nil))))

(defmethod parametric-type-run-time-lambda-body
    ((type-car (eql 'array)) type-cdr parameter)
  (parametric-type-run-time-lambda-body-for-array 'array type-cdr parameter))

(defmethod parametric-type-run-time-lambda-body
    ((type-car (eql 'simple-array)) type-cdr parameter)
  (parametric-type-run-time-lambda-body-for-array 'simple-array type-cdr parameter))

(defmethod parametric-type-compile-time-lambda-body
    ((type-car (eql 'array)) type-cdr parameter)
  (parametric-type-compile-time-lambda-body-for-array 'array type-cdr parameter))

(defmethod parametric-type-compile-time-lambda-body
    ((type-car (eql 'simple-array)) type-cdr parameter)
 (parametric-type-compile-time-lambda-body-for-array 'simple-array type-cdr parameter))
