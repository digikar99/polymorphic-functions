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

(defstruct polymorph
  (documentation nil :type (or null string))
  (name (error "NAME must be supplied!"))
  (return-type)
  (type-list nil)
  (lambda-list-type nil)
  (effective-type-list nil)
  (applicable-p-function)
  (inline-lambda-body)
  ;; We need the FUNCTION-BODY due to compiler macros,
  ;; and "objects of type FUNCTION can't be dumped into fasl files."
  (lambda)
  (compiler-macro-lambda))

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
    (with-slots (name polymorphs) o
      (format stream "~S (~S)" name (length polymorphs)))))

(defun polymorphic-function-type-lists (polymorphic-function)
  (mapcar #'polymorph-type-list (polymorphic-function-polymorphs polymorphic-function)))

(defun polymorphic-function-effective-type-lists (polymorphic-function)
  (mapcar #'polymorph-effective-type-list
          (polymorphic-function-polymorphs polymorphic-function)))

(defvar *name*)
(setf (documentation '*name* 'variable)
      "NAME of the typed function being compiled. Bound inside DEFINE-POLYMORPH")

(defvar *environment*)
(setf (documentation '*environment* 'variable)
      "Bound inside the DEFINE-COMPILER-MACRO defined in DEFINE-POLYMORPH for
use by functions like TYPE-LIST-APPLICABLE-P")

(defun register-polymorphic-function (name untyped-lambda-list documentation &key overwrite)
  (declare (type function-name       name)
           (type (or null string)    documentation)
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
    (multiple-value-bind (body-forms lambda-list) (defun-body untyped-lambda-list)
      (let ((apf (make-instance 'adhoc-polymorphic-function
                                :name name
                                :documentation documentation
                                :lambda-list untyped-lambda-list
                                :lambda-list-type (lambda-list-type untyped-lambda-list))))

        (closer-mop:set-funcallable-instance-function
         apf #+sbcl (compile nil `(sb-int:named-lambda (adhoc-polymorphic-function ,name)
                                    ,lambda-list ,@body-forms))
             ;; FIXME: https://github.com/Clozure/ccl/issues/361
             #+ccl (eval `(ccl:nfunction (adhoc-polymorphic-function ,name)
                                         (lambda ,lambda-list ,@body-forms)))
             #-(or ccl sbcl) (compile nil `(lambda ,lambda-list ,@body-forms)))
        (setf (fdefinition name) apf)
        (setf (documentation name 'function) documentation)))))

(defun register-polymorph (name type-list effective-type-list
                           return-type inline-lambda-body lambda
                           lambda-list-type)
  (declare (type function-name  name)
           (type function       lambda)
           (type type-list      type-list)
           (type type-list      effective-type-list)
           (type type-specifier return-type)
           (type list           inline-lambda-body))
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
    (ensure-unambiguous-call name type-list effective-type-list)
    (let* ((polymorphs (polymorphic-function-polymorphs apf))
           (polymorph  (find type-list polymorphs :test #'equalp :key #'polymorph-type-list)))
      ;; FIXME: Use a type-list equality check, not EQUALP
      ;; FIXME: A trivial fix for &key args is to sort them lexically
      (setf (polymorphic-function-polymorphs apf)
            (cons (make-polymorph :name name
                                  :type-list        type-list
                                  :return-type      return-type
                                  :lambda-list-type lambda-list-type
                                  :effective-type-list effective-type-list
                                  :applicable-p-function
                                  (compile nil (applicable-p-function lambda-list-type
                                                                      effective-type-list))
                                  :inline-lambda-body inline-lambda-body
                                  :lambda             lambda
                                  :compiler-macro-lambda
                                  (when polymorph
                                    (polymorph-compiler-macro-lambda polymorph)))
                  (remove type-list polymorphs :test #'equalp :key #'polymorph-type-list)))
      name)))

;; TODO: Implement caching for retrieving polymorphs

(defun retrieve-polymorph (name &rest arg-list)
  (declare (type function-name name))
  (assert *compiler-macro-expanding-p*)
  ;; This function is used by the main compiler macro of the polymorphic-function
  ;; The RETRIEVE-POLYMORPH-FORM below is a complementary to this function.
  (let* ((apf                   (fdefinition name))
         (polymorphs            (polymorphic-function-polymorphs apf))
         (lambda-list-type      (polymorphic-function-lambda-list-type apf))
         (applicable-polymorph  nil)
         (applicable-polymorphs nil))
    (loop :for polymorph :in polymorphs
          :do (when (if (eq 'rest lambda-list-type)
                        (ignore-errors
                         (apply
                          (the function
                               (polymorph-applicable-p-function polymorph))
                          arg-list))
                        (apply
                         (the function
                              (polymorph-applicable-p-function polymorph))
                         arg-list))
                (cond (applicable-polymorphs (push polymorph applicable-polymorphs))
                      (applicable-polymorph
                       (push applicable-polymorph applicable-polymorphs)
                       (push polymorph applicable-polymorphs)
                       (setq applicable-polymorph nil))
                      (t (setq applicable-polymorph polymorph)))))
    ;; TODO: Optimize this; look at specialization-store?
    (or applicable-polymorph
        (most-specialized-polymorph applicable-polymorphs)
        (error 'no-applicable-polymorph
               :arg-list arg-list
               :effective-type-lists
               (polymorphic-function-effective-type-lists (fdefinition name))))))

(defun retrieve-polymorph-form (name lambda-list-type arg-list)
  ;; This is called either while the main polymorphic-function itself is being compiled,
  ;; or while the compiler-macro associated with it is being compiled.
  (assert (every #'symbolp arg-list))
  ;; Introduce gensyms the day this asserition fails: is this premature-optimization?
  (let* ((apf-form         (if *compiler-macro-expanding-p*
                               ;; FDEFINITION can be incredibly slower
                               ;; than FUNCTION on SBCL
                               `(function ,name)
                               `(fdefinition ',name)))
         (funcall-form     `(funcall (the function
                                          (polymorph-applicable-p-function polymorph))
                                     ,@arg-list))
         (apply-form       `(apply (the function
                                        (polymorph-applicable-p-function polymorph))
                                   ,@arg-list))
         (applicable-p-form (if *compiler-macro-expanding-p*
                                (if (eq 'rest lambda-list-type)
                                    `(ignore-errors
                                      (ecase (polymorph-lambda-list-type polymorph)
                                        ((required required-optional) ,funcall-form)
                                        ((rest required-key) ,apply-form)))
                                    funcall-form)
                                (ecase lambda-list-type
                                  (rest `(ignore-errors ,apply-form))
                                  (required-key apply-form)
                                  ((required required-optional) funcall-form))))
         (no-polymorph-arg-list (case lambda-list-type
                                  (required-key `(nconc (list ,@(butlast arg-list))
                                                        ,(lastcar arg-list)))
                                  (t `(list ,@arg-list)))))
    ;; - TYPE_LISTS and PF-HASH-TABLE will change after the
    ;;   "call to RETRIEVE-POLYMORPH" is compiled
    ;; - While FUNCTION-WRAPPER can be precompiled, completely dumping it requires
    ;;   knowledge of PF-HASH-TABLE that would only be available after
    ;;   the call to RETRIEVE-POLYMORPH is compiled. (Am I missing something?)
    ;;   An interested person may look up this link for MAKE-LOAD-FORM
    ;;   https://blog.cneufeld.ca/2014/03/the-less-familiar-parts-of-lisp-for-beginners-make-load-form/
    `(block retrieve-polymorph
       ;; Separating out applicable-p-function into a separate list,
       ;; in fact, decreases performance on sbcl
       ;; TODO: Optimize this; look at specialization-store?
       (let ((applicable-polymorph nil)
             (applicable-polymorphs nil))
         (declare (optimize speed))
         (loop :for polymorph :in (polymorphic-function-polymorphs ,apf-form)
               :do (when ,applicable-p-form
                     (cond
                       (applicable-polymorphs (push polymorph applicable-polymorphs))
                       (applicable-polymorph
                        (push applicable-polymorph applicable-polymorphs)
                        (push polymorph applicable-polymorphs)
                        (setq applicable-polymorph nil))
                       (t (setq applicable-polymorph polymorph)))))
         (cond (applicable-polymorph applicable-polymorph)
               (applicable-polymorphs
                (most-specialized-polymorph applicable-polymorphs))
               (t
                (error 'no-applicable-polymorph
                       :arg-list ,no-polymorph-arg-list
                       :effective-type-lists
                       (polymorphic-function-effective-type-lists ,apf-form))))))))

#+sbcl
(defun most-specialized-applicable-transform-p (name node type-list)
  (let ((applicable-polymorphs
          (remove-if-not (lambda (polymorph)
                           (sb-c::valid-fun-use
                            node
                            (sb-kernel:specifier-type
                             (list 'function
                                   (let ((type-list (polymorph-type-list polymorph)))
                                     ;; FIXME: Better integration of &rest types
                                     (if (eq '&rest (lastcar type-list))
                                         (butlast type-list)
                                         type-list))
                                   '*))))
                         (polymorphic-function-polymorphs
                          (fdefinition name)))))
    (equalp type-list
            (polymorph-type-list (most-specialized-polymorph applicable-polymorphs)))))

(defun most-specialized-polymorph (polymorphs)
  (declare (optimize debug))
  (cond ((null polymorphs) nil)
        ((null (rest polymorphs)) (first polymorphs))
        (t
         (let ((ms-polymorph (most-specialized-polymorph (rest polymorphs))))
           (if (type-list-subtype-p (polymorph-type-list ms-polymorph)
                                    (polymorph-type-list (first polymorphs)))
               ms-polymorph
               (first polymorphs))))))

(defun remove-polymorph (name type-list)
  (let ((apf (fdefinition name)))
    (when apf
      (removef (polymorphic-function-polymorphs apf) type-list
               :test #'equalp :key #'polymorph-type-list))))

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
    ;; FIXME: How should we account for EFFECTIVE-TYPE-LIST here?
    (ensure-unambiguous-call name type-list type-list)
    (with-slots (polymorphs) apf
      (if-let (polymorph (find type-list polymorphs :test #'equalp
                                                    :key #'polymorph-type-list))
        (setf (polymorph-compiler-macro-lambda polymorph) lambda)
        ;; Need below instead of error-ing to define the compiler macro simultaneously
        (push (make-polymorph :name name
                              :type-list type-list
                              :lambda-list-type lambda-list-type
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
