(in-package adhoc-polymorphic-functions)

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
    (cond ((and valid-p list (eq '&key (first list)))
           (loop :for (param type) :in (rest list)
                 :do (setq valid-p (type-specifier-p type))))
          ((and valid-p list)
           (setq valid-p nil)))
    valid-p))

(def-test type-list (:suite :adhoc-polymorphic-functions)
  (5am:is-true (type-list-p '()))
  (5am:is-true (type-list-p '(number string)))
  (5am:is-true (type-list-p '(&optional)))
  (5am:is-true (type-list-p '(&key)))
  (5am:is-true (type-list-p '(number &optional string)))
  (5am:is-true (type-list-p '(number &key (:a string)))))

(deftype type-list () `(satisfies type-list-p))

(defclass polymorph ()
  ;; TODO: Put these to use
  (#+sbcl (sb-pcl::source :initarg sb-pcl::source)
   #+sbcl (sb-pcl::plist :initarg sb-pcl::plist)
   (documentation :type (or null string) :initarg :documentation)
   (type-list :initarg :type-list
              :reader polymorph-type-list)
   (applicable-p-function :initarg :applicable-p-function
                          :reader polymorph-applicable-p-function
                          :type function)
   ;; We need the LAMBDA-BODY due to compiler macros,
   ;; and "objects of type FUNCTION can't be dumped into fasl files."
   (lambda-body :initarg :lambda-body :accessor polymorph-lambda-body)
   (lambda      :initarg :lambda      :accessor polymorph-lambda)
   (compiler-macro-lambda :initarg :compiler-macro-lambda
                          :initform nil
                          :accessor polymorph-compiler-macro-lambda)))

(defun polymorph (polymorph)
  "Returns the LAMBDA associated with POLYMORPH"
  (declare (type polymorph polymorph)
           (optimize speed))
  (polymorph-lambda polymorph))

(declaim (notinline polymorph-applicable-p))
(defun polymorph-applicable-p (polymorph args)
  (declare (type polymorph polymorph)
           (type list args)
           (optimize speed))
  (apply (the function (polymorph-applicable-p-function polymorph)) args))

(defclass polymorphic-function ()
  ((name        :initarg :name
                :initform (error "NAME must be supplied.")
                :reader polymorphic-function-name)
   (lambda-list :initarg :lambda-list :type list
                :initform (error "LAMBDA-LIST must be supplied.")
                :reader polymorphic-function-lambda-list)
   (type-lists  :initform nil :type list
                :accessor polymorphic-function-type-lists)
   (lambda-list-type :type lambda-list-type
                     :initarg :lambda-list-type
                     :initform (error "LAMBDA-LIST-TYPE must be supplied.")
                     :reader polymorphic-function-lambda-list-type)
   (hash-table  :initform (make-hash-table :test 'equalp) :type hash-table
                :reader polymorphic-function-hash-table)
   (documentation :initarg :documentation
                  :type (or string null))
   #+sbcl (sb-pcl::source :initarg sb-pcl::source)
   #+sbcl (%lock
           :initform (sb-thread:make-mutex :name "GF lock")
           :reader sb-pcl::gf-lock))
  (:documentation
   "HASH-TABLE maps a TYPE-LIST to a POLYMORPH. A TYPE-LIST is simply a LIST of TYPEs.")
  ;; TODO: Check if a symbol / list denotes a type
  (:metaclass closer-mop:funcallable-standard-class))

(defvar *name*)
(setf (documentation '*name* 'variable)
      "NAME of the typed function being compiled. Bound inside DEFINE-POLYMORPH")

(defvar *environment*)
(setf (documentation '*environment* 'variable)
      "Bound inside the DEFINE-COMPILER-MACRO defined in DEFINE-POLYMORPH for
use by functions like TYPE-LIST-APPLICABLE-P")

(defun register-polymorphic-function (name untyped-lambda-list &key overwrite)
  (declare (type function-name       name)
           (type untyped-lambda-list untyped-lambda-list))
  (unless overwrite
    (when-let (pf (and (fboundp name) (fdefinition name)))
      (if (typep pf 'polymorphic-function)
          (cerror "Yes, delete existing POLYMORPHs and associate new ones"
                  "There already exists a POLYMORPHIC-FUNCTION associated with NAME ~S corresponding~%to the following TYPE-LISTS~%~{~^    ~S~%~}Do you want to delete these POLYMORPHs and associate a new~%POLYMORPHIC-FUNCTION with NAME ~S?"
                  name (polymorphic-function-type-lists pf) name)
          (cerror (format nil
                          "Yes, delete existing FUNCTION associated with ~S and associate a new POLYMORPHIC-FUNCTION" name)
                  "There already exists a FUNCTION associated with NAME ~S.~%Do you want to delete the existing FUNCTION and associate a new~%POLYMORPHIC-FUNCTION with NAME ~S?"
                  name name))))
  (let ((*name* name))
    (multiple-value-bind (body-form lambda-list) (defun-body untyped-lambda-list)
      (let ((pf (make-instance 'polymorphic-function
                               :name name
                               :lambda-list untyped-lambda-list
                               :lambda-list-type (lambda-list-type untyped-lambda-list))))
        (closer-mop:set-funcallable-instance-function
         pf
         (compile nil
                  `(lambda ,lambda-list ,body-form)))
        (setf (fdefinition name) pf)))))

(defun register-polymorph (name type-list lambda-body lambda)
  ;; TODO: Get rid of APPLICABLE-P-FUNCTION: construct it from type-list
  (declare (type function-name name)
           (type function      lambda)
           (type type-list     type-list)
           (type list          lambda-body))
  (let* ((pf                (fdefinition name))
         (table             (polymorphic-function-hash-table pf))
         (lambda-list-type  (polymorphic-function-lambda-list-type pf)))
    (with-slots (type-lists) pf
      ;; FIXME: Use a type-list equality check, not EQUALP
      (unless (member type-list type-lists :test 'equalp)
        (setf type-lists (cons type-list type-lists))))
    (multiple-value-bind (polymorph exists)
        (gethash type-list table)
      (if exists
          (setf (polymorph-lambda      polymorph) lambda
                (polymorph-lambda-body polymorph) lambda-body)
          (setf (gethash type-list table)
                (make-instance 'polymorph
                               :type-list type-list
                               :applicable-p-function
                               (compile nil (applicable-p-function lambda-list-type
                                                                   type-list))
                               :lambda-body lambda-body
                               :lambda      lambda))))))

(defun retrieve-polymorph (name &rest arg-list)
  "If successful, returns 3 values:
  the first object is the function body,
  the second is the function itself,
  the third is the type list corresponding to the polymorph that will be used for dispatch"
  (declare (type function-name name))
  (let* ((polymorphic-function (fdefinition name))
         (pf-hash-table   (polymorphic-function-hash-table polymorphic-function)))
    (loop :for polymorph :being the hash-values :of pf-hash-table
          :do (when (apply (polymorph-applicable-p-function polymorph)
                           arg-list)
                (return-from retrieve-polymorph
                  (values (polymorph-lambda-body polymorph)
                          (the function (polymorph-lambda  polymorph))
                          (polymorph-type-list polymorph)))))
    (error 'no-applicable-polymorph
           :arg-list arg-list
           :type-lists (polymorphic-function-type-lists polymorphic-function))))

(defun remove-polymorph (name type-list)
  (let ((pf (fdefinition name)))
    (when pf
      (remhash type-list
               (polymorphic-function-hash-table pf))
      (removef (polymorphic-function-type-lists pf) type-list :test 'equalp))))

(define-compiler-macro retrieve-polymorph (&whole form name &rest arg-list
                                                  &environment env)
  (cond ((= 3 (policy-quality 'debug env))
         form)
        ((and (listp name)
              (eq 'quote (first name))
              (null (third name)))
         (when (eq 'funcall (first form))
           (setq form (rest form)))
         (if (eq 'retrieve-polymorph (first form))
             (let ((gensyms (make-gensym-list (length arg-list))))
               `(block retrieve-polymorph
                  ;; - TYPE_LISTS and PF-HASH-TABLE will change after the
                  ;;   "call to RETRIEVE-POLYMORPH" is compiled
                  ;; - While FUNCTION-WRAPPER can be precompiled, completely dumping it requires
                  ;;   knowledge of PF-HASH-TABLE that would only be available after
                  ;;   the call to RETRIEVE-POLYMORPH is compiled. (Am I missing something?)
                  ;;   An interested person may look up this link for MAKE-LOAD-FORM
                  ;;   https://blog.cneufeld.ca/2014/03/the-less-familiar-parts-of-lisp-for-beginners-make-load-form/
                  (let*
                      ((polymorphic-function (fdefinition ,name))
                       (pf-hash-table        (polymorphic-function-hash-table
                                              polymorphic-function))
                       ,@(mapcar #'list gensyms arg-list))
                    (declare (optimize speed)
                             (type hash-table           pf-hash-table)
                             (type polymorphic-function polymorphic-function))
                    (loop :for polymorph :being the hash-values :of pf-hash-table
                          :do (when (funcall (the function
                                                  (polymorph-applicable-p-function polymorph))
                                             ,@gensyms)
                                (return-from retrieve-polymorph
                                  (values (polymorph-lambda-body polymorph)
                                          (polymorph-lambda      polymorph)))))
                    (error 'no-applicable-polymorph
                           :arg-list (list ,@gensyms)
                           :type-lists (polymorphic-function-type-lists polymorphic-function)))))
             form))
        (t form)))

(defun register-polymorph-compiler-macro (name type-list lambda)
  (declare (type function-name name)
           (type type-list type-list)
           (type function lambda))
  (let* ((pf               (fdefinition name))
         (table            (polymorphic-function-hash-table pf))
         (lambda-list-type (polymorphic-function-lambda-list-type pf)))
    (multiple-value-bind (polymorph exists)
        (gethash type-list table)
      (if exists
          (setf (polymorph-compiler-macro-lambda polymorph) lambda)
          ;; Need below instead of error-ing to define the compiler macro simultaneously
          (setf (gethash type-list table)
                (make-instance 'polymorph
                               :type-list type-list
                               :applicable-p-function
                               (compile nil (applicable-p-function lambda-list-type
                                                                   type-list))
                               :compiler-macro-lambda lambda))))))

(defun retrieve-polymorph-compiler-macro (name &rest arg-list)
  ;; TODO: Update this function
  (declare (type function-name name))
  (let* ((polymorphic-function (fdefinition name))
         (pf-hash-table        (polymorphic-function-hash-table polymorphic-function))
         (type-lists           (polymorphic-function-type-lists polymorphic-function))
         (applicable-function-type-lists
           (loop :for polymorph :being the hash-values :of pf-hash-table
                 :if (polymorph-applicable-p polymorph arg-list)
                   :collect (polymorph-type-list polymorph))))
    (case (length applicable-function-type-lists)
      (1 (polymorph-compiler-macro-lambda
          (gethash (first applicable-function-type-lists) pf-hash-table)))
      (0 (error 'no-applicable-polymorph :arg-list arg-list :type-lists type-lists))
      (t (error "Multiple applicable POLYMORPHs discovered for ARG-LIST ~S:~%~{~S~^    ~%~}"
                arg-list
                applicable-function-type-lists)))))

(defun find-polymorphs (name &optional (type-list nil type-list-p))
  "Returns two values:
If a POLYMORPHIC-FUNCTION by NAME does not exist, returns NIL NIL.
If it exists, the second value is T and the first value is a possibly empty
  list of POLYMORPHs associated with NAME."
  (declare (type function-name name))
  (let* ((polymorphic-function   (and (fboundp name) (fdefinition name)))
         (pf-hash-table     (when polymorphic-function
                                   (polymorphic-function-hash-table polymorphic-function))))
    (cond ((null polymorphic-function)
           (values nil nil))
          ((null type-list-p)
           (values (hash-table-values pf-hash-table) t))
          (t
           ;; FIXME: Use a type-list equality check, not EQUALP
           (loop :for polymorph :being the hash-values :of pf-hash-table
                 :do (when (equalp type-list
                                   (polymorph-type-list polymorph))
                       (return-from find-polymorphs
                         (values (list polymorph) t))))
           (values nil t)))))
