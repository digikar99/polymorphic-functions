(in-package :adhoc-polymorphic-functions)

(define-condition no-applicable-polymorph (error)
  ((arg-list :initarg :arg-list
             :initform (error "ARG-LIST not specified")
             :reader arg-list)
   (type-lists :initarg :type-lists
               :initform (error "TYPE-LISTS not specified")
               :reader type-lists))
  (:report (lambda (condition stream)
             (format stream
                     "~%No applicable POLYMORPH discovered for ARG-LIST ~S.~%Available TYPE-LISTs include:~%   ~{~S~^~%   ~}"
                     (arg-list condition)
                     (type-lists condition)))))

(define-condition form-type-failure (condition)
  ((form :initarg :form
         :initform (error "FORM not specified")
         :reader form))
  (:report (lambda (condition stream)
             (format stream "~%Type of ~%  ~S~%could not be determined" (form condition)))))

(define-condition polymorph-body-has-free-variables (condition)
  ((name :initarg :name
         :initform (error "NAME not specified")
         :reader name)
   (type-list :initarg :type-list
              :initform (error "TYPE-LIST not specified")
              :reader type-list))
  (:report (lambda (condition stream)
             (format stream "~&~S with TYPE-LIST ~S cannot be inlined due to free-variables"
                     (name condition)
                     (type-list condition)))))

(define-condition recursive-expansion-is-possibly-infinite (condition)
  ((form :initarg :form
         :initform (error "FORM not specified")
         :reader form))
  (:report (lambda (condition stream)
             (format stream "~&Inlining ~S results in (potentially infinite) recursive expansion"
                     (form condition)))))
