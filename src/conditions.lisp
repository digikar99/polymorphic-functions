(in-package :polymorphic-functions)

(define-condition lambda-list-has-changed (error)
  ((name :initarg :name
         :reader name
         :initform (error "NAME must be supplied!"))
   (new-lambda-list :initarg :new-lambda-list
                    :reader new-lambda-list
                    :initform (error "NEW LAMBDA LIST must be supplied!")))
  (:report (lambda (condition stream)
             (let* ((name (name condition))
                    (apf  (fdefinition name)))
               (format stream "New lambda list~%  ~S~%does not match the old lambda list~%  ~S
of the ADHOC-POLYMORPHIC-FUNCTION ~S with TYPE-LISTS:~%~{~^    ~S~%~}

Do you want to delete these POLYMORPHs to associate a new ones?"
                       (new-lambda-list condition)
                       (polymorphic-function-lambda-list apf)
                       name
                       (polymorphic-function-type-lists  apf))))))

(define-condition not-a-ahp (error)
  ((name :initarg :name
         :reader name
         :initform (error "NAME must be supplied!")))
  (:report (lambda (condition stream)
             (let* ((name (name condition)))
               (format stream "There already exists a FUNCTION ~S associated with NAME ~S.~%Do you want to delete the existing FUNCTION and associate a new~%POLYMORPHIC-FUNCTION with NAME ~S?"
                       (fdefinition name) name name)))))

(define-condition no-applicable-polymorph ()
  ((name :initarg :name
         :initform (error "NAME not specified")
         :reader name)
   (args :initarg :args
         :initform (error "ARGS not specified")
         :reader args)
   (arg-types :initarg :arg-types
              :reader arg-types)
   (effective-type-lists :initarg :effective-type-lists
                         :initform (error "EFFECTIVE-TYPE-LISTS not specified")
                         :reader effective-type-lists))
  (:report (lambda (condition s)
             (pprint-logical-block (s nil)
               (format s "No applicable POLYMORPH discovered for polymorphic-function~%  ~S~%"
                       (name condition))
               (format s "and ARGS:~%~%")
               ;; It is possible that argss could be circular (?)
               ;; Or that they contain circular structures (?)
               (pprint-logical-block (s nil :per-line-prefix "  ")
                 (format s "~S" (args condition)))
               (format s "~%~%derived to be of TYPES:~%~%")
               (pprint-logical-block (s nil :per-line-prefix "  ")
                 (format s "~S" (if (slot-boundp condition 'arg-types)
                                    (arg-types condition)
                                    (mapcar #'type-of (args condition)))))
               ;; So, we only "improve" the printing for effective-type-lists
               (let ((*print-circle* nil)
                     (type-lists (effective-type-lists condition)))
                 (format s
                         "~%~%Available Effective-Type-Lists include:~%~{~^~%  ~S~}"
                         (subseq type-lists
                                 0 (if *print-length*
                                       (min *print-length* (length type-lists))
                                       nil)))
                 (when (and *print-length*
                            (nthcdr *print-length* type-lists))
                   (format s "~%  ...")))))))

(define-condition no-applicable-polymorph/error
    (no-applicable-polymorph error)
  ())

(define-condition no-applicable-polymorph/compiler-note
    (no-applicable-polymorph compiler-macro-notes:note)
  ())

(defun no-applicable-polymorph (name env args &optional arg-types)
  (declare (ignore env))
  (if *compiler-macro-expanding-p*
      (signal 'no-applicable-polymorph/compiler-note
              :name name
              :args args
              :arg-types arg-types
              :effective-type-lists
              (polymorphic-function-effective-type-lists (fdefinition name)))
      (error 'no-applicable-polymorph/error
             :name name
             :args args
             :effective-type-lists
             (polymorphic-function-effective-type-lists (fdefinition name)))))

(define-condition defpolymorph-note (compiler-macro-notes:note)
  ((datum :initarg :datum :reader condition-datum))
  (:report (lambda (c s) (write-string (condition-datum c) s))))

(defun note-null-env (form datum &rest arguments)
  (let ((*print-pretty* t))
    (format *error-output* "~%Inlining~%")
    (pprint-logical-block (*error-output* nil :per-line-prefix "  ")
      (format *error-output* "~S" form))
    (format *error-output* "~&in null environment is not without warnings:~%")
    (pprint-logical-block (*error-output* nil :per-line-prefix "  ")
      (format *error-output* "~A"
              (handler-case (apply #'signal datum arguments)
                (condition (c) c))))))

(defun note-no-inline (form datum &rest arguments)
  (let ((*print-pretty* t))
    (format *error-output* "Will not inline~%~A~%because ~A"
            (with-output-to-string (*error-output*)
              (pprint-logical-block (*error-output* nil :per-line-prefix "  ")
                (format *error-output* "~S" form)))
            (if (string= "" datum)
                ""
                (format nil "~&~A"
                        (handler-case (apply #'signal datum arguments)
                          (condition (c) c)))))))

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

;; TODO: Add a NOT-THE-MOST-SPECIALIZED-POLYMORPH condition
