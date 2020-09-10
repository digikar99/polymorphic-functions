(in-package typed-dispatch)

(define-condition malformed-lambda-list (error)
  ((lambda-list :reader lambda-list
                :initarg :lambda-list)))

(define-condition malformed-untyped-lambda-list (malformed-lambda-list)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "Expected LAMBDA-LIST ~D to be an ~D; but ~D returned NIL as the first value"
                     (lambda-list condition)
                     'UNTYPED-LAMBDA-LIST
                     'UNTYPED-LAMBDA-LIST-P))))

(define-condition malformed-typed-lambda-list (malformed-lambda-list)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "Expected LAMBDA-LIST ~D to be a ~D; but ~D returned NIL as the first value"
                     (lambda-list condition)
                     'TYPED-LAMBDA-LIST
                     'TYPED-LAMBDA-LIST-P))))

(defun remove-untyped-args (lambda-list &key typed)
  "Examples
  (remove-untyped-args '(a b c &key d))      ;=> (A B C)
  (remove-untyped-args '(a b c &optional d)) ;=> (A B C D)
  (remove-untyped-args '((a string) &optional ((d integer)) &rest args) :typed t)
    ;=> (A D), (integer string)

If TYPED is non-NIL, the second return value is a TYPE-LIST corresponding to the first return value.

Signals a MALFORMED-(UN)TYPED-LAMBDA-LIST error if the parsing fails."
  (if typed
      (multiple-value-bind (valid-p untyped-lambda-list type-list typed-param-list)
          (typed-lambda-list-p lambda-list)
        (declare (type boolean valid-p)
                 (type untyped-lambda-list untyped-lambda-list)
                 (ignore untyped-lambda-list)
                 (type type-list type-list)
                 (type list typed-param-list))
        (if valid-p
            (values typed-param-list type-list)
            (error 'malformed-typed-lambda-list
                   :lambda-list lambda-list)))
      (multiple-value-bind (valid-p typed-param-list)
          (untyped-lambda-list-p lambda-list)
        (if valid-p
            typed-param-list
            (error 'malformed-untyped-lambda-list
                   :lambda-list lambda-list))))) 

(defun parse-lambda-list (lambda-list &key typed)
  "Converst LAMBDA-LIST to a form suitable for passing to FUNCALL or APPLY."
  ;; TODO: Handle the case of parsed-args better
  (multiple-value-bind (ordinary-args optional-args rest keyword-args)
      (parse-ordinary-lambda-list lambda-list :normalize-optional nil
                                              :normalize-keyword nil
                                              :allow-specializers typed)
    (declare (ignore rest))
    (append (if typed
                (mapcar #'first ordinary-args)
                ordinary-args)
            (if typed
                (mapcar (compose #'first #'first) optional-args)
                (mapcar #'first optional-args)) 
            (flatten (mapcar (lambda (keyword)
                               (let ((keyword (etypecase keyword
                                                (symbol keyword)
                                                (list   (caar keyword)))))
                                 (list (intern (symbol-name keyword) :keyword)
                                       keyword)))
                             keyword-args)))))

(defun process-untyped-lambda-list (list)
  "Assuming LIST is an UNTYPED-LAMBDA-LIST, it returns the third return value of UNTYPED-LAMBDA-LIST-P."
  (nth-value 2 (untyped-lambda-list-p list)))

(defun process-typed-lambda-list (list)
  (nth-value 4 (typed-lambda-list-p list)))
