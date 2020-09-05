(in-package typed-dispatch)

(defun type-list-p (list)
  (every (lambda (obj)
           (or (type-specifier-p obj)
               (eq '&optional obj)))
         list))

(deftype type-list () `(satisfies type-list-p))

;; equalp: need to allow for symbols and (setf symbol) "function-name" more strictly
(defparameter *typed-function-table* (make-hash-table :test 'equalp)) 

(defstruct (typed-function)
  (type-list nil :type (proper-list type-specifier))
  (body)
  ;; We need the FUNCTION-BODY due to compiler macros, and "objects of type FUNCTION can't be dumped into fasl files.
  (function)
  (compiler-macro))

(defun typed-function (typed-function)
  "Returns the NAMED-LAMBDA associated with TYPED-FUNCTION"
  (declare (type typed-function typed-function)
           (optimize speed))
  (typed-function-function typed-function))

(defstruct typed-function-wrapper
  "HASH-TABLE maps a TYPE-LIST to a TYPED-FUNCTION. A TYPE-LIST is simply a LIST of TYPEs."
  ;; TODO: Check if a symbol / list denotes a type
  (name (error "NAME must be supplied.") :type function-name)
  (lambda-list (error "LAMBDA-LIST must be supplied.") :type list)
  (hash-table (make-hash-table :test 'equalp) :type hash-table))

(defun register-typed-function-wrapper (name lambda-list)
  (declare (type function-name name)
           (type list   lambda-list))
  (when (gethash name *typed-function-table*)
    (warn "Redefining typed-function ~D ..." name))
  (setf (gethash name *typed-function-table*)
        (make-typed-function-wrapper :name name
                                     :lambda-list lambda-list)))

(defun retrieve-typed-function-wrapper (name)
  (gethash name *typed-function-table*))

(defun register-typed-function (name type-list function-body function)
  (declare (type function-name name)
           (type function  function)
           (type type-list type-list))
  (let ((table (typed-function-wrapper-hash-table (gethash name *typed-function-table*))))
    (setf (gethash type-list table) (cons function-body function))))

(defun retrieve-typed-function (name type-list)
  "If successful, returns 2 values: the first object is the function body, while the second is the function itself."
  (declare (type function-name name)
           (type type-list type-list))
  (let* ((typed-function-wrapper-hash-table (typed-function-wrapper-hash-table
                                             (gethash name
                                                      *typed-function-table*)))
         (type-lists                (hash-table-keys typed-function-wrapper-hash-table))
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
             (gethash (first applicable-function-type-lists) typed-function-wrapper-hash-table)
           (values body function)))
      (0 (error "No applicable TYPED-FUNCTION discovered for TYPE-LIST ~D.~%Available TYPE-LISTs include:~%~{~D~^    ~%~}"
                supplied-type-list
                type-lists))
      (t (error "Multiple applicable TYPED-FUNCTIONs discovered for TYPE-LIST ~D:~%~{~D~^    ~%~}"
                supplied-type-list
                applicable-function-type-lists)))))

