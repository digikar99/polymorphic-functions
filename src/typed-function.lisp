(in-package typed-dispatch)

(defun type-list-p (list)
  (every (lambda (obj)
           (or (eq '&optional obj)
               (type-specifier-p obj)))
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
    (multiple-value-bind (typed-function exists)
        (gethash type-list table)
      (if exists
          (setf (typed-function-function typed-function) function
                (typed-function-body     typed-function) function-body)
          (setf (gethash type-list table)
                (make-typed-function :type-list type-list
                                     :body function-body
                                     :function function))))))

(defun compute-applicable-function-type-lists (supplied-type-list expected-type-lists)
  (loop :for expected-type-list :in expected-type-lists
        :if (let ((supplied-type-list supplied-type-list))
              (loop :for idx :from 0
                    :with supplied-type-list-length := (length supplied-type-list)
                    :for expected-type :in expected-type-list
                    :with list-valid-p := t
                    :while list-valid-p
                    :with optional-args-p := nil
                    :do ;; (print (list expected-type supplied-type-list
                        ;;              supplied-type-list-length
                        ;;              idx))
                        (setq list-valid-p
                              (and list-valid-p
                                   (cond ((eq expected-type '&optional)
                                          (setq optional-args-p t)
                                          t)
                                         ((and (first supplied-type-list)
                                               (subtypep (first supplied-type-list)
                                                         expected-type))
                                          t)
                                         ((and (first supplied-type-list)
                                               (not (subtypep (first supplied-type-list)
                                                              expected-type)))
                                          nil)
                                         ((and (>= idx supplied-type-list-length)
                                               optional-args-p)
                                          t)
                                         (t
                                          nil))))
                        (unless (eq expected-type '&optional)
                          (setq supplied-type-list (rest supplied-type-list)))
                    :finally (return list-valid-p)))
          :collect expected-type-list))

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
           (compute-applicable-function-type-lists supplied-type-list type-lists)))
    (case (length applicable-function-type-lists)
      (1 (with-slots (body function)
             (gethash (first applicable-function-type-lists) typed-function-wrapper-hash-table)
           (values body function)))
      (0 (error "~%No applicable TYPED-FUNCTION discovered for TYPE-LIST ~D.~%Available TYPE-LISTs include:~%   ~{~D~^~%   ~}"
                supplied-type-list
                type-lists))
      (t (error "Multiple applicable TYPED-FUNCTIONs discovered for TYPE-LIST ~D:~%~{~D~^    ~%~}"
                supplied-type-list
                applicable-function-type-lists)))))

(defun register-typed-function-compiler-macro (name type-list function)
  (declare (type function-name name)
           (type type-list type-list)
           (type function function))
  (let ((table (typed-function-wrapper-hash-table (gethash name *typed-function-table*))))
    (multiple-value-bind (typed-function exists)
        (gethash type-list table)
      (if exists
          (setf (typed-function-compiler-macro typed-function) function)
          (setf (gethash type-list table)
                (make-typed-function :type-list type-list
                                     :compiler-macro function))))))

(defun retrieve-typed-function-compiler-macro (name type-list)
  (declare (type function-name name)
           (type type-list type-list))
  (let* ((typed-function-wrapper-hash-table (typed-function-wrapper-hash-table
                                             (gethash name
                                                      *typed-function-table*)))
         (type-lists                (hash-table-keys typed-function-wrapper-hash-table))
         (supplied-type-list        type-list)
         (applicable-function-type-lists
           (compute-applicable-function-type-lists supplied-type-list type-lists)))
    (case (length applicable-function-type-lists)
      (1 (typed-function-compiler-macro
          (gethash (first applicable-function-type-lists)
                   typed-function-wrapper-hash-table)))
      (0 (error "~%No applicable TYPED-FUNCTION discovered for TYPE-LIST ~D.~%Available TYPE-LISTs include:~%   ~{~D~^~%   ~}"
                supplied-type-list
                type-lists))
      (t (error "Multiple applicable TYPED-FUNCTIONs discovered for TYPE-LIST ~D:~%~{~D~^    ~%~}"
                supplied-type-list
                applicable-function-type-lists)))))

