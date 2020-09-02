(in-package typed-dispatch)

(defparameter *typed-function-table* (make-hash-table :test 'eq))

(defstruct (typed-function)
  "  HASH-TABLE maps a TYPE-LIST to a pair of two elements: the first element consists of the (lambda) function body while the second consists of the compiled lambda function itself. 
  A TYPE-LIST is simply a LIST of TYPEs."
  ;; TODO: Check if a symbol / list denotes a type
  (name (error "NAME must be supplied.") :type symbol)
  (lambda-list (error "LAMBDA-LIST must be supplied.") :type list)
  (hash-table (make-hash-table :test 'equalp) :type hash-table))

(defun register-typed-function-with-name (name lambda-list)
  (declare (type symbol        name)
           (type list   lambda-list))
  (when (gethash name *typed-function-table*)
    (warn "Redefining typed-function ~D ..." name))
  (setf (gethash name *typed-function-table*)
        (make-typed-function :name name
                             :lambda-list lambda-list)))

(defun retrieve-typed-function-with-name (name)
  (gethash name *typed-function-table*))

(defun register-typed-function (name type-list function-body function)
  (declare (type symbol        name)
           (type list     type-list)
           (type function  function))
  (let ((table (typed-function-hash-table (gethash name *typed-function-table*))))
    ;; We need the FUNCTION-BODY due to compiler macros, and "objects of type FUNCTION can't be dumped into fasl files.
    (setf (gethash type-list table) (cons function-body function))))

(defun retrieve-typed-function (name type-list)
  "If successful, returns 2 values: the first object is the function body, while the second is the function itself."
  (declare (type symbol      name)
           (type list   type-list))
  (let* ((typed-function-hash-table (typed-function-hash-table
                                     (gethash name
                                             *typed-function-table*)))
         (type-lists                (hash-table-keys typed-function-hash-table))
         (supplied-type-list        type-list)
         (applicable-function-type-lists
           (loop :for expected-type-list :in type-lists
                 :if (every (lambda (supplied-type expected-type)
                              (subtypep supplied-type expected-type))
                            supplied-type-list
                            expected-type-list)
                   :collect expected-type-list)))
    (case (length applicable-function-type-lists)
      (1 (destructuring-bind (body &rest function)
             (gethash (first applicable-function-type-lists) typed-function-hash-table)
           (values body function)))
      (0 (error "No applicable TYPED-FUNCTION discovered for TYPE-LIST ~D.~%Available TYPE-LISTs include:~%~{~D~^  ~%~}"
                supplied-type-list
                type-lists))
      (t (error "Multiple applicable TYPED-FUNCTIONs discovered for TYPE-LIST ~D:~%~{~D~^  ~%~}"
                supplied-type-list
                applicable-function-type-lists)))))

