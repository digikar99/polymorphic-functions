(in-package :typed-functions)

(define-condition no-applicable-typed-function (error)
  ((arg-list :initarg :arg-list
             :initform (error "ARG-LIST not specified")
             :reader arg-list)
   (type-lists :initarg :type-lists
               :initform (error "TYPE-LISTS not specified")
               :reader type-lists))
  (:report (lambda (condition stream)
             (format stream
                     "~%No applicable TYPED-FUNCTION discovered for ARG-LIST ~S.~%Available TYPE-LISTs include:~%   ~{~S~^~%   ~}"
                     (arg-list condition)
                     (type-lists condition)))))

(define-condition form-type-failure (condition)
  ((form :initarg :form
         :initform (error "FORM not specified")
         :reader form))
  (:report (lambda (condition stream)
             (format stream "~%Type of ~%  ~S~%could not be determined" (form condition)))))
