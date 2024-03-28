(in-package :polymorphic-functions)

(define-condition no-applicable-polymorph/compiler-note
    (no-applicable-polymorph compiler-macro-notes:note)
  ())


(define-condition defpolymorph-note (compiler-macro-notes:note)
  ((datum :initarg :datum :reader condition-datum))
  (:report (lambda (c s) (write-string (condition-datum c) s))))

(define-condition form-type-failure (compiler-macro-notes:optimization-failure-note)
  ((form :initarg :form
         :initform (error "FORM not specified")
         :reader form))
  (:report (lambda (condition stream)
             (format stream "Type of~%  ~S~%could not be determined" (form condition)))))

(define-condition polymorph-has-no-inline-lambda-body
    (compiler-macro-notes:optimization-failure-note)
  ((name :initarg :name
         :initform (error "NAME not specified")
         :reader name)
   (type-list :initarg :type-list
              :initform (error "TYPE-LIST not specified")
              :reader type-list))
  (:report (lambda (condition stream)
             (format stream "~S with TYPE-LIST ~%  ~S~%has no stored INLINE-LAMBDA-BODY"
                     (name condition)
                     (type-list condition)))))

(define-condition suboptimal-polymorph-note
    (compiler-macro-notes:optimization-failure-note)
  ((type-list :initarg :type-list :reader type-list))
  (:report (lambda (condition stream)
             (format stream "POLYMORPH with TYPE-LIST~%  ~S~%was used for optimizing, but it is possibly suboptimal~%Better POLYMORPHs should exist"
                     (type-list condition)))))

(define-condition more-optimal-polymorph-inapplicable
    (suboptimal-polymorph-note)
  ((more-optimal-type-list :initarg :more-optimal-type-list
                           :initform (error "MORE-OPTIMAL-TYPE-LIST not specified")
                           :reader more-optimal-type-list))
  (:report (lambda (condition stream)
             (format stream "More optimal POLYMORPH with TYPE-LIST~%  ~S~%was found to be inapplicable"
                     (more-optimal-type-list condition)))))

(define-condition compile-time-return-type-mismatch
    (compiler-macro-notes:optimization-failure-note)
  ((derived :initarg :derived)
   (declared :initarg :declared)
   (form :initarg :form))
  (:report (lambda (condition stream)
             (with-slots (derived declared form) condition
               (format stream "The declared return type~%  ~S~%does not match the derived return type~%  ~S~%of form~%  ~S"
                       declared derived form)))))

