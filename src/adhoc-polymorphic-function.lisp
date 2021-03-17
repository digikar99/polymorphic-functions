(in-package adhoc-polymorphic-functions)

(defun type-list-p (list)
  ;; TODO: what parameter-names are valid?
  (let ((valid-p t))
    (loop :for elt := (first list)
          :while (and list valid-p) ; we don't want list to be empty
          :until (member elt '(&key &rest))
          :do (setq valid-p
                    (and valid-p
                         (cond ((eq '&optional elt)
                                t)
                               ((type-specifier-p elt)
                                t)
                               (t
                                nil))))
              (setq list (rest list)))
    (when valid-p
      (cond ((eq '&key (first list))
             (when list
               (loop :for (param type) :in (rest list)
                     :do (setq valid-p (type-specifier-p type)))))
            ((eq '&rest (first list))
             (unless (null (rest list))
               (setq valid-p nil)))
            (list
             (setq valid-p nil))))
    valid-p))

(def-test type-list (:suite :adhoc-polymorphic-functions)
  (5am:is-true (type-list-p '()))
  (5am:is-true (type-list-p '(number string)))
  (5am:is-true (type-list-p '(number string &rest)))
  (5am:is-true (type-list-p '(&optional)))
  (5am:is-true (type-list-p '(&key)))
  (5am:is-true (type-list-p '(&rest)))
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
   (name        :initarg :name
                :initform (error "NAME must be supplied!"))
   (compiler-macro-lambda :initarg :compiler-macro-lambda
                          :initform nil
                          :accessor polymorph-compiler-macro-lambda))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((o polymorph) stream)
  (print-unreadable-object (o stream :type t)
    (with-slots (name type-list) o
      (format stream "~S ~S" name type-list))))

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

(defmethod print-object ((o adhoc-polymorphic-function) stream)
  (print-unreadable-object (o stream :type t)
    (with-slots (name type-lists) o
      (format stream "~S (~S)" name (length type-lists)))))

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
  (unless overwrite ; so, OVERWRITE is NIL
    (when-let (apf (and (fboundp name) (fdefinition name)))
      (if (typep apf 'adhoc-polymorphic-function)
          (if (equalp untyped-lambda-list
                      (polymorphic-function-lambda-list apf))
              (return-from register-polymorphic-function name)
              (cerror "Yes, delete existing POLYMORPHs to associate new ones"
                      'lambda-list-has-changed
                      :name name
                      :new-lambda-list untyped-lambda-list))
          (cerror (format nil
                          "Yes, delete existing FUNCTION associated with ~S and associate an ADHOC-POLYMORPHIC-FUNCTION" name)
                  'not-a-ahp :name name))))
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

(defun register-polymorph (name type-list lambda-body lambda lambda-list-type)
  ;; TODO: Get rid of APPLICABLE-P-FUNCTION: construct it from type-list
  (declare (type function-name name)
           (type function      lambda)
           (type type-list     type-list)
           (type list          lambda-body))
  (let* ((apf                        (fdefinition name))
         (apf-lambda-list-type       (polymorphic-function-lambda-list-type apf))
         (untyped-lambda-list        (polymorphic-function-lambda-list apf)))
    (when (eq apf-lambda-list-type 'rest)
      ;; required-optional can simply be split up into multiple required or required-key
      (assert (member lambda-list-type '(rest required required-key))
              nil
              "&OPTIONAL keyword is not allowed for LAMBDA-LIST~%  ~S~%of the ADHOC-POLYMORPHIC-FUNCTION associated with ~S"
              untyped-lambda-list name))
    (assert (type-list-compatible-p type-list untyped-lambda-list)
            nil
            "TYPE-LIST ~S is not compatible with the LAMBDA-LIST ~S of the POLYMORPHs associated with ~S"
            type-list untyped-lambda-list name)
    (ensure-non-intersecting-type-lists name type-list)
    (with-slots (type-lists polymorphs) apf
      ;; FIXME: Use a type-list equality check, not EQUALP
      (unless (member type-list type-lists :test 'equalp)
        (setf type-lists (cons type-list type-lists)))
      (if-let (polymorph (find type-list polymorphs :test #'equalp :key #'polymorph-type-list))
        (setf (polymorph-lambda      polymorph) lambda
              (polymorph-lambda-body polymorph) lambda-body)
        (push (make-instance 'polymorph
                             :name name
                             :type-list type-list
                             :applicable-p-function
                             (compile nil (applicable-p-function lambda-list-type
                                                                 type-list))
                             :lambda-body lambda-body
                             :lambda      lambda)
              polymorphs))
      (let ((polymorph (find type-list polymorphs :test #'equalp :key #'polymorph-type-list)))
        (closer-mop:set-funcallable-instance-function polymorph lambda))
      name)))

(defun retrieve-polymorph (name &rest arg-list)
  "If successful, returns 3 values:
  the first object is the function body,
  the second is the function itself,
  the third is the type list corresponding to the polymorph that will be used for dispatch"
  (declare (type function-name name))
  (let* ((apf                  (fdefinition name))
         (polymorphs           (polymorphic-function-polymorphs apf))
         (apf-lambda-list-type (polymorphic-function-lambda-list-type apf)))
    (loop :for polymorph :in polymorphs
          :do (when (if (eq 'rest apf-lambda-list-type)
                        (ignore-errors
                         (apply
                          (the function
                               (polymorph-applicable-p-function polymorph))
                          arg-list))
                        (apply
                         (the function
                              (polymorph-applicable-p-function polymorph))
                         arg-list))
                (return-from retrieve-polymorph
                  (values (polymorph-lambda-body polymorph)
                          (the function (polymorph-lambda  polymorph))))))
    (error 'no-applicable-polymorph
           :arg-list arg-list
           :type-lists (polymorphic-function-type-lists apf))))

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
                      ((apf                  (fdefinition ,name))
                       (polymorphs           (polymorphic-function-polymorphs apf))
                       (apf-lambda-list-type (polymorphic-function-lambda-list-type apf))
                       ,@(mapcar #'list gensyms arg-list))
                    (declare (optimize speed)
                             (type list                       polymorphs)
                             (type adhoc-polymorphic-function apf))
                    (loop :for polymorph :in polymorphs
                          :do (when (if (and (eq 'rest apf-lambda-list-type)
                                             (listp ,(lastcar gensyms)))
                                        (ignore-errors
                                         (apply
                                          (the function
                                               (polymorph-applicable-p-function polymorph))
                                          ,@gensyms))
                                        (funcall
                                         (the function
                                              (polymorph-applicable-p-function polymorph))
                                         ,@gensyms))
                                (return-from retrieve-polymorph
                                  (values (polymorph-lambda-body polymorph)
                                          (polymorph-lambda      polymorph)))))
                    (error 'no-applicable-polymorph
                           :arg-list (list ,@(if *compiler-macro-expanding-p*
                                                 arg-list
                                                 gensyms))
                           :type-lists (polymorphic-function-type-lists apf)))))
             form))
        (t form)))

(defun register-polymorph-compiler-macro (name type-list lambda)
  (declare (type function-name name)
           (type type-list type-list)
           (type function lambda))

  (let* ((apf              (fdefinition name))
         (lambda-list      (polymorphic-function-lambda-list apf))
         (lambda-list-type (polymorphic-function-lambda-list-type apf))
         (type-list   (let ((key-position (position '&key type-list)))
                        (if key-position
                            (append (subseq type-list 0 key-position)
                                    '(&key)
                                    (sort (subseq type-list (1+ key-position))
                                          #'string< :key #'first))
                            ;; This sorting allows things to work even though
                            ;; we use EQUALP instead of TYPE-LIST-=
                            type-list))))
    (if (eq lambda-list-type 'rest)
        ;; required-optional can simply be split up into multiple required or required-key
        (assert (not (member '&optional type-list))
                nil
                "&OPTIONAL keyword is not allowed for LAMBDA-LIST~%  ~S~%of the ADHOC-POLYMORPHIC-FUNCTION associated with ~S"
                lambda-list name)
        (assert (type-list-compatible-p type-list lambda-list)
                nil
                "TYPE-LIST ~S is not compatible with the LAMBDA-LIST ~S of the POLYMORPHs associated with ~S"
                type-list lambda-list name))
    (ensure-non-intersecting-type-lists name type-list)

    (with-slots (polymorphs) apf
      (if-let (polymorph (find type-list polymorphs :test #'equalp
                                                    :key #'polymorph-type-list))
        (setf (polymorph-compiler-macro-lambda polymorph) lambda)
        ;; Need below instead of error-ing to define the compiler macro simultaneously
        (push (make-instance 'polymorph
                             :name name
                             :type-list type-list
                             :applicable-p-function
                             (compile nil (applicable-p-function lambda-list-type
                                                                 type-list))
                             :compiler-macro-lambda lambda)
              polymorphs)))))

(defun retrieve-polymorph-compiler-macro (name &rest arg-list)
  (declare (type function-name name))
  (let* ((apf                  (fdefinition name))
         (polymorphs           (polymorphic-function-polymorphs apf))
         (type-lists           (polymorphic-function-type-lists apf))
         (apf-lambda-list-type (polymorphic-function-lambda-list-type apf))
         (applicable-polymorphs
           (loop :for polymorph :in polymorphs
                 :if (if (eq 'rest apf-lambda-list-type)
                         (ignore-errors
                          (apply
                           (the function
                                (polymorph-applicable-p-function polymorph))
                           arg-list))
                         (apply
                          (the function
                               (polymorph-applicable-p-function polymorph))
                          arg-list))
                   :collect polymorph)))
    (case (length applicable-polymorphs)
      (1 (polymorph-compiler-macro-lambda (first applicable-polymorphs)))
      (0 (error 'no-applicable-polymorph :arg-list arg-list :type-lists type-lists))
      (t (error "Multiple applicable POLYMORPHs discovered for ARG-LIST ~S:~%~{~S~^    ~%~}"
                arg-list
                (mapcar #'polymorph-type-list applicable-polymorphs))))))

(defun find-polymorph (name type-list)
  "Returns two values:
If a ADHOC-POLYMORPHIC-FUNCTION by NAME does not exist, returns NIL NIL.
If it exists, the second value is T and the first value is a possibly empty
  list of POLYMORPHs associated with NAME."
  (declare (type function-name name))
  (let* ((apf        (and (fboundp name) (fdefinition name)))
         (polymorphs (when (typep apf 'adhoc-polymorphic-function)
                       (polymorphic-function-polymorphs apf))))
    (cond ((null (typep apf 'adhoc-polymorphic-function))
           (values nil nil))
          (t
           ;; FIXME: Use a type-list equality check, not EQUALP
           (loop :for polymorph :in polymorphs
                 :do (when (equalp type-list
                                   (polymorph-type-list polymorph))
                       (return-from find-polymorph
                         (values polymorph t))))
           (values nil t)))))
