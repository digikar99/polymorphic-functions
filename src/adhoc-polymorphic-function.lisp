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

(defclass adhoc-polymorphic-function ()
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
   (polymorphs  :initform nil
                :accessor polymorphic-function-polymorphs)
   (documentation :initarg :documentation
                  :type (or string null))
   #+sbcl (sb-pcl::source :initarg sb-pcl::source)
   #+sbcl (%lock
           :initform (sb-thread:make-mutex :name "GF lock")
           :reader sb-pcl::gf-lock))
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
    (when-let (apf (and (fboundp name) (fdefinition name)))
      (if (typep apf 'adhoc-polymorphic-function)
          (cerror "Yes, delete existing POLYMORPHs and associate new ones"
                  "There already exists a ADHOC-POLYMORPHIC-FUNCTION associated with NAME ~S corresponding~%to the following TYPE-LISTS~%~{~^    ~S~%~}Do you want to delete these POLYMORPHs and associate a new~%POLYMORPHIC-FUNCTION with NAME ~S?"
                  name (polymorphic-function-type-lists apf) name)
          (cerror (format nil
                          "Yes, delete existing FUNCTION associated with ~S and associate a new ADHOC-POLYMORPHIC-FUNCTION" name)
                  "There already exists a FUNCTION associated with NAME ~S.~%Do you want to delete the existing FUNCTION and associate a new~%POLYMORPHIC-FUNCTION with NAME ~S?"
                  name name))))
  (let ((*name* name))
    (multiple-value-bind (body-form lambda-list) (defun-body untyped-lambda-list)
      (let ((apf (make-instance 'adhoc-polymorphic-function
                               :name name
                               :lambda-list untyped-lambda-list
                               :lambda-list-type (lambda-list-type untyped-lambda-list))))
        (closer-mop:set-funcallable-instance-function
         apf
         (compile nil
                  `(lambda ,lambda-list ,body-form)))
        (setf (fdefinition name) apf)))))

(defun register-polymorph (name type-list lambda-body lambda)
  ;; TODO: Get rid of APPLICABLE-P-FUNCTION: construct it from type-list
  (declare (type function-name name)
           (type function      lambda)
           (type type-list     type-list)
           (type list          lambda-body))
  (let* ((apf               (fdefinition name))
         (lambda-list-type  (polymorphic-function-lambda-list-type apf)))
    (with-slots (type-lists polymorphs) apf
      ;; FIXME: Use a type-list equality check, not EQUALP
      (unless (member type-list type-lists :test 'equalp)
        (setf type-lists (cons type-list type-lists)))
      (if-let (polymorph (find type-list polymorphs :test #'equalp :key #'polymorph-type-list))
        (setf (polymorph-lambda      polymorph) lambda
              (polymorph-lambda-body polymorph) lambda-body)
        (push (make-instance 'polymorph
                             :type-list type-list
                             :applicable-p-function
                             (compile nil (applicable-p-function lambda-list-type
                                                                 type-list))
                             :lambda-body lambda-body
                             :lambda      lambda)
              polymorphs)))))

(defun retrieve-polymorph (name &rest arg-list)
  "If successful, returns 3 values:
  the first object is the function body,
  the second is the function itself,
  the third is the type list corresponding to the polymorph that will be used for dispatch"
  (declare (type function-name name))
  (let* ((ahp        (fdefinition name))
         (polymorphs (polymorphic-function-polymorphs ahp)))
    (loop :for polymorph :in polymorphs
          :do (when (apply (polymorph-applicable-p-function polymorph)
                           arg-list)
                (return-from retrieve-polymorph
                  (values (polymorph-lambda-body polymorph)
                          (the function (polymorph-lambda  polymorph))
                          (polymorph-type-list polymorph)))))
    (error 'no-applicable-polymorph
           :arg-list arg-list
           :type-lists (polymorphic-function-type-lists ahp))))

(defun remove-polymorph (name type-list)
  (let ((apf (fdefinition name)))
    (when apf
      (removef (polymorphic-function-polymorphs apf) type-list 
               :test #'equalp :key #'polymorph-type-list)
      (removef (polymorphic-function-type-lists apf) type-list :test 'equalp))))

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
                      ((apf        (fdefinition ,name))
                       (polymorphs (polymorphic-function-polymorphs apf))
                       ,@(mapcar #'list gensyms arg-list))
                    (declare (optimize speed)
                             (type list                       polymorphs)
                             (type adhoc-polymorphic-function apf))
                    (loop :for polymorph :in polymorphs
                          :do (when (funcall (the function
                                                  (polymorph-applicable-p-function polymorph))
                                             ,@gensyms)
                                (return-from retrieve-polymorph
                                  (values (polymorph-lambda-body polymorph)
                                          (polymorph-lambda      polymorph)))))
                    (error 'no-applicable-polymorph
                           :arg-list (list ,@gensyms)
                           :type-lists (polymorphic-function-type-lists apf)))))
             form))
        (t form)))

(defun register-polymorph-compiler-macro (name type-list lambda)
  (declare (type function-name name)
           (type type-list type-list)
           (type function lambda))
  (let* ((apf              (fdefinition name))
         (lambda-list-type (polymorphic-function-lambda-list-type apf)))
    (with-slots (polymorphs) apf
      (if-let (polymorph (find type-list polymorphs :test #'equalp
                                                    :key #'polymorph-type-list))
        (setf (polymorph-compiler-macro-lambda polymorph) lambda)
        ;; Need below instead of error-ing to define the compiler macro simultaneously
        (push (make-instance 'polymorph
                             :type-list type-list
                             :applicable-p-function
                             (compile nil (applicable-p-function lambda-list-type
                                                                 type-list))
                             :compiler-macro-lambda lambda)
              polymorphs)))))

(defun retrieve-polymorph-compiler-macro (name &rest arg-list)
  (declare (type function-name name))
  (let* ((apf        (fdefinition name))
         (polymorphs (polymorphic-function-polymorphs apf))
         (type-lists (polymorphic-function-type-lists apf))
         (applicable-polymorphs
           (loop :for polymorph :in polymorphs
                 :if (polymorph-applicable-p polymorph arg-list)
                   :collect polymorph)))
    (case (length applicable-polymorphs)
      (1 (polymorph-compiler-macro-lambda (first applicable-polymorphs)))
      (0 (error 'no-applicable-polymorph :arg-list arg-list :type-lists type-lists))
      (t (error "Multiple applicable POLYMORPHs discovered for ARG-LIST ~S:~%~{~S~^    ~%~}"
                arg-list
                (mapcar #'polymorph-type-list applicable-polymorphs))))))

(defun find-polymorphs (name &optional (type-list nil type-list-p))
  "Returns two values:
If a ADHOC-POLYMORPHIC-FUNCTION by NAME does not exist, returns NIL NIL.
If it exists, the second value is T and the first value is a possibly empty
  list of POLYMORPHs associated with NAME."
  (declare (type function-name name))
  (let* ((apf        (and (fboundp name) (fdefinition name)))
         (polymorphs (when (typep apf 'adhoc-polymorphic-function)
                       (polymorphic-function-polymorphs apf))))
    (cond ((null apf)
           (values nil nil))
          ((null type-list-p)
           (values (copy-list polymorphs) t))
          (t
           ;; FIXME: Use a type-list equality check, not EQUALP
           (loop :for polymorph :in polymorphs
                 :do (when (equalp type-list
                                   (polymorph-type-list polymorph))
                       (return-from find-polymorphs
                         (values (list polymorph) t))))
           (values nil t)))))
