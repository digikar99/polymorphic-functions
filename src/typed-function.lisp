(in-package typed-dispatch)

(defun type-list-p (list)
  ;; TODO: what parameter-names are valid?
  (let ((valid-p t))
    (loop :for elt := (first list)
          :while (and list valid-p) ; we don't want list to be empty
          :until (eq '&key elt)
          :do (setq valid-p
                    (and valid-p
                         (cond ((eq '&optional elt)
                                t)
                               ((type-specifier-p elt)
                                t)
                               (t
                                nil))))
              (setq list (rest list)))
    (cond ((and valid-p list (eq '&key (first list)) (oddp (length list)))
           (loop :initially (setq list (rest list))
                 :while list
                 :for param := (first  list)
                 :for type  := (second list)
                 :do (setq valid-p (type-specifier-p type))
                 (setq list (cddr list))))
          ((and valid-p list)
           (setq valid-p nil)))
    valid-p))

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

(defun compute-applicable-function-type-lists (supplied-arg-list expected-type-lists)
  ;; TODO: Prepare (regression) tests for this function
  (loop :for expected-type-list :in expected-type-lists
        :if (let ((supplied-arg-list supplied-arg-list)
                  (expected-type-list expected-type-list)
                  (list-valid-p       t))
              (loop :for idx :from 0
                    :with supplied-arg-list-length := (length supplied-arg-list)
                    :while expected-type-list
                    :for expected-type := (first expected-type-list)
                    :until (eq expected-type '&key)
                    :while list-valid-p
                    :with optional-args-p := nil
                    :do ;; (print (list expected-type supplied-arg-list
                        ;;              supplied-arg-list-length
                        ;;              idx))
                        (setq list-valid-p
                              (and list-valid-p
                                   (cond ((and (eq '&optional expected-type)
                                               (eq '&optional (first supplied-arg-list)))
                                          (setq optional-args-p t)
                                          t)
                                         ((and (first supplied-arg-list)
                                               (typep (first supplied-arg-list)
                                                      expected-type))
                                          t)
                                         ((and (first supplied-arg-list)
                                               (not (typep (first supplied-arg-list)
                                                           expected-type)))
                                          nil)
                                         ((and (>= idx supplied-arg-list-length)
                                               optional-args-p)
                                          t)
                                         (t
                                          nil))))
                        (setq supplied-arg-list (rest supplied-arg-list))
                        (setq expected-type-list (rest expected-type-list)))
              (when (and list-valid-p
                         (eq '&key (first expected-type-list))
                         (eq '&key (first supplied-arg-list)))
                (loop :initially (setq expected-type-list (rest expected-type-list))
                                 (setq supplied-arg-list (rest supplied-arg-list))
                      :while supplied-arg-list
                      :for key  := (first supplied-arg-list)
                      :for supplied-arg := (second supplied-arg-list)
                      :do (setq list-valid-p
                                (and list-valid-p
                                     (typep supplied-arg (getf expected-type-list key))))
                          (setq supplied-arg-list (cddr supplied-arg-list))))
              list-valid-p)
          :collect expected-type-list))

(defun retrieve-typed-function (name &rest arg-list)
  "If successful, returns 3 values:
  the first object is the function body,
  the second is the function itself,
  the third is the type list corresponding to the typed function that will be used for dispatch"
  (declare (type function-name name))
  (let* ((typed-function-wrapper-hash-table (typed-function-wrapper-hash-table
                                             (gethash name
                                                      *typed-function-table*)))
         (type-lists                (hash-table-keys typed-function-wrapper-hash-table))
         (supplied-arg-list        arg-list)
         (applicable-function-type-lists
           (compute-applicable-function-type-lists supplied-arg-list type-lists)))
    (case (length applicable-function-type-lists)
      (1 (with-slots (body function)
             (gethash (first applicable-function-type-lists) typed-function-wrapper-hash-table)
           (values body function (first applicable-function-type-lists))))
      (0 (error "~%No applicable TYPED-FUNCTION discovered for ARG-LIST ~S.~%Available TYPE-LISTs include:~%   ~{~S~^~%   ~}"
                supplied-arg-list
                type-lists))
      (t (error "Multiple applicable TYPED-FUNCTIONs discovered for ARG-LIST ~S:~%~{~S~^    ~%~}"
                supplied-arg-list
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
      (0 (error "~%No applicable TYPED-FUNCTION discovered for TYPE-LIST ~S.~%Available TYPE-LISTs include:~%   ~{~S~^~%   ~}"
                supplied-type-list
                type-lists))
      (t (error "Multiple applicable TYPED-FUNCTIONs discovered for TYPE-LIST ~S:~%~{~S~^    ~%~}"
                supplied-type-list
                applicable-function-type-lists)))))

