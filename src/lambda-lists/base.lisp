(in-package #:polymorphic-functions)

;; In this file, our main functions/macros are
;; - DEFINE-LAMBDA-LIST-HELPER
;; - LAMBDA-LIST-TYPE
;; - COMPUTE-POLYMORPHIC-FUNCTION-LAMBDA-BODY
;; - SBCL-TRANSFORM-ARGS-FROM-LAMBDA-LIST-FORM
;; - TYPE-LIST-COMPATIBLE-P
;; - type-list-more-specific-p
;; - TYPE-LIST-CAUSES-AMBIGUOUS-CALL-P

;; THE BASICS ==================================================================

(5am:def-suite lambda-list :in :polymorphic-functions)

(defun valid-parameter-name-p (name)
  (and (symbolp name)
       (not (constantp name))
       (not (member name lambda-list-keywords))))

(defun potential-type-of-lambda-list (lambda-list)
  ;; "potential" because it does not check the symbols
  (declare (type list lambda-list))
  (the lambda-list-type
       (let ((intersection (intersection lambda-list lambda-list-keywords)))
         ;; premature optimization and over-abstraction :/
         (cond ((null intersection) 'required)
               ((and (car intersection) (null (cdr intersection)) ; length is 1
                     (member '&optional intersection))
                'required-optional)
               ((and (car intersection) (null (cddr intersection)) ; length is at most 2
                     (or (set-equal intersection '(&key))
                         (set-equal intersection '(&key &rest))))
                'required-key)
               ((member '&rest intersection) ; don't check the lengths
                'rest)
               (t
                (error "Neither of ~A types" +lambda-list-types+))))))

(defvar *potential-type*)
(setf (documentation '*potential-type* 'variable)
      "POTENTIAL-TYPE of the LAMBDA-LIST of the typed function being compiled.
Bound inside the functions defined by POLYMORPHS::DEFINE-LAMBDA-LIST-HELPER")

(defvar *lambda-list*)
(setf (documentation '*lambda-list* 'variable)
      "LAMBDA-LIST of the typed function being compiled. Bound inside the functions
defined by POLYMORPHS::DEFINE-LAMBDA-LIST-HELPER")

(defvar *lambda-list-typed-p*)
(setf (documentation '*lambda-list-typed-p* 'variable)
      "Is T if the *LAMBDA-LIST* being processed is to be treated as if it had type
specifiers. Bound inside the functions defined by POLYMORPHS::DEFINE-LAMBDA-LIST-HELPER")

(defmacro define-lambda-list-helper ((outer-name outer-documentation)
                                     (inner-name inner-documentation)
                                     &body action-form)
  "ACTION-FORM should be defined in terms of *POTENTIAL-TYPE* and *LAMBDA-LIST* variables."
  `(progn
     (defun ,outer-name (lambda-list &key typed)
       ,outer-documentation
       (declare (type list lambda-list))
       (let ((*potential-type*      (potential-type-of-lambda-list lambda-list))
             (*lambda-list*         lambda-list)
             (*lambda-list-typed-p* typed))
         (if (%lambda-list-type *potential-type* lambda-list)
             (progn ,@action-form)
             (error "LAMBDA-LIST ~S is neither of ~%  ~S" lambda-list +lambda-list-types+))))
     (defgeneric ,inner-name (potential-lambda-list-type lambda-list)
       (:documentation ,inner-documentation))
     ;; For better error reporting
     (defmethod ,inner-name ((type t) (lambda-list t))
       (assert (typep type 'lambda-list-type)
               ()
               "Expected POTENTIAL-LAMBDA-LIST-TYPE to be one of ~%  ~S~%but is ~S"
               +lambda-list-types+ type)
       (assert (typep lambda-list 'list)
               ()
               "Expected LAMBDA-LIST to be a LIST but is ~S"
               lambda-list)
       (error "No potential type found for LAMBDA-LIST ~S from amongst ~%  ~S"
              lambda-list +lambda-list-types+))))

;; LAMBDA-LIST-TYPE ============================================================

(define-lambda-list-helper
    (lambda-list-type  #.+lambda-list-type-doc+)
    (%lambda-list-type "Checks whether LAMBDA-LIST is of type POTENTIAL-LAMBDA-LIST-TYPE")
  *potential-type*)

;; COMPUTE-POLYMORPHIC-FUNCTION-LAMBDA-BODY ====================================

(defgeneric compute-polymorphic-function-lambda-body
    (lambda-list-type effective-untyped-lambda-list declaration &optional invalidated-p)
  (:documentation #.+compute-polymorphic-function-lambda-body-doc+))

;; SBCL-TRANSFORM-ARG-LVARS-FROM-LAMBDA-LIST-FORM ==============================

(define-lambda-list-helper
    (sbcl-transform-arg-lvars-from-lambda-list-form
     #.+sbcl-transform-arg-lvars-from-lambda-list-form+)
    (%sbcl-transform-arg-lvars-from-lambda-list-form
     #.+sbcl-transform-arg-lvars-from-lambda-list-form+)
  (progn
    (assert (untyped-lambda-list-p *lambda-list*))
    (%sbcl-transform-arg-lvars-from-lambda-list-form *potential-type* *lambda-list*)))

;; TYPE-LIST-COMPATIBLE-P ======================================================

(defun type-list-compatible-p (lambda-list-type type-list effective-untyped-lambda-list)
  "Returns T if the given TYPE-LIST is compatible with the given UNTYPED-LAMBDA-LIST."
  (declare (type type-list type-list))
  (let ((*lambda-list-typed-p* nil)
        (*potential-type* lambda-list-type))
    (%type-list-compatible-p *potential-type* type-list effective-untyped-lambda-list)))

(defgeneric %type-list-compatible-p
    (potential-lambda-list-type type-list untyped-lambda-list))

(defmethod %type-list-compatible-p ((type t)
                                    (type-list t)
                                    (untyped-lambda-list t))
  (assert (typep type 'lambda-list-type) nil
          "Expected LAMBDA-LIST-TYPE to be one of ~%  ~a~%but is ~a"
          +lambda-list-types+ type)
  (assert (typep type-list 'type-list) nil
          "Expected TYPE-LIST to be a TYPE-LIST but is ~a" type-list)
  (assert (typep untyped-lambda-list 'untyped-lambda-list) nil
          "Expected ~A to be a UNTYPED-LAMBDA-LIST" untyped-lambda-list)
  (error "This code shouldn't have reached here; perhaps file a bug report!"))

(def-test type-list-compatible-p (:suite lambda-list)
  (5am:is-true  (type-list-compatible-p 'required '(string string) '(c d)))
  (5am:is-false (type-list-compatible-p 'required '(string) '(c d)))
  (5am:is-true  (type-list-compatible-p 'required-optional
                                        '(string number &optional t) '(c d &optional e)))
  (5am:is-false (type-list-compatible-p 'required-optional
                                        '(number) '(c d &optional d)))
  (5am:is-false (type-list-compatible-p 'required-key
                                        '(string &key (:d number))
                                        '(c &rest args &key (d nil dp) (e nil ep))))
  (5am:is-true  (type-list-compatible-p 'required-key
                                        '(string &key (:d number) (:e string))
                                        '(c &rest args &key (d nil dp) (e nil ep))))
  (5am:is-false (type-list-compatible-p 'required-key
                                        '(string &key (:d number))
                                        '(c &rest args &key (d nil dp) (e nil ep))))
  (5am:is-true  (type-list-compatible-p 'rest
                                        '(string) '(c &rest e)))
  (5am:is-true  (type-list-compatible-p 'rest
                                        '(number string) '(c &rest e)))
  (5am:is-false (type-list-compatible-p 'rest
                                        '(&rest) '(c &rest e))))

(defvar *name*)
(defvar *environment*)


;; TYPE-LIST-MORE-SPECIFIC-P =========================================================

(defun type-list-more-specific-p (type-list-1 type-list-2)
  #.+type-list-more-specific-p+
  (declare (type type-list type-list-1 type-list-2))
  (let ((*lambda-list-typed-p* nil))
    (if (equal type-list-1 type-list-2)
        t
        (%type-list-more-specific-p (potential-type-of-lambda-list type-list-1)
                                    (potential-type-of-lambda-list type-list-2)
                                    type-list-1
                                    type-list-2))))

(defgeneric %type-list-more-specific-p (type-1 type-2 type-list-1 type-list-2)
  (:documentation #.+type-list-more-specific-p+))

(5am:def-suite type-list-more-specific-p :in lambda-list)

;; TYPE-LIST-INTERSECTION-NULL-P ===============================================

(defun intersection-null-p (env &rest types)
  (subtypep `(and ,@types) nil env))

(defun definitive-intersection-null-p (type1 type2 &optional env)
  (multiple-value-bind (intersection-null-p knownp)
      (intersection-null-p env type1 type2)
    (cond ((not knownp)
           (cerror "Retry"
                   "Please use PELTADOT:DEFINE-SUBTYPEP-LAMBDA and
PELTADOT:DEFINE-INTERSECT-TYPE-P-LAMBDA to define the intersection
of the following types:~%  ~S~%  ~S"
                   type1 type2)
           (definitive-intersection-null-p type1 type2 env))
          (t
           intersection-null-p))))

(defun type-list-intersection-null-p (type-list-1 type-list-2)
  #.+type-list-intersection-null-p+
  (declare (type type-list type-list-1 type-list-2))
  (let ((*lambda-list-typed-p* nil))
    (%type-list-intersection-null-p (potential-type-of-lambda-list type-list-1)
                                    (potential-type-of-lambda-list type-list-2)
                                    type-list-1
                                    type-list-2)))

(defgeneric %type-list-intersection-null-p
    (type-1 type-2 type-list-1 type-list-2)
  (:documentation #.+type-list-intersection-null-p+))

(5am:def-suite type-list-intersection-null-p :in lambda-list)

;; FTYPE-FOR-STATIC-DISPATCH ================================================

(defun ftype-for-static-dispatch (static-dispatch-name effective-type-list return-type env)
  (declare (ignorable env))
  `(ftype (function ,(let ((type-list (loop :with state := :required
                                            :for type-spec :in effective-type-list
                                            :with processed-type-spec := nil
                                            :do (setq processed-type-spec
                                                      (if (member type-spec lambda-list-keywords)
                                                          (setq state type-spec)
                                                          (ecase state
                                                            ((:required &optional)
                                                             type-spec)
                                                            (&key
                                                             `(,(first type-spec)
                                                               ,(second type-spec))))))
                                            :collect processed-type-spec)))
                       (if (eq '&rest (lastcar effective-type-list))
                           (append type-list '(t))
                           type-list))
                    ,return-type)
          ,static-dispatch-name))

(defun ftype-proclaimation
    (static-dispatch-name effective-type-list return-type env)
  (let* ((ftype (ftype-for-static-dispatch
                 static-dispatch-name effective-type-list return-type env))
         (proclaimation
           `(proclaim ',ftype)))
    (if optim-debug
        proclaimation
        `(handler-bind ((warning #'muffle-warning))
           ,proclaimation))))
