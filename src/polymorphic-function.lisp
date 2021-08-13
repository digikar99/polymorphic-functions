(in-package polymorphic-functions)

(defun blockify-name (name)
  (etypecase name
    (symbol name)
    (list
     (assert (and (eq 'setf (first name))
                  (second name)
                  (null (nthcdr 2 name))))
     (second name))))

(defun type-list-p (list)
  ;; TODO: what parameter-names are valid?
  (let ((valid-p t))
    (loop :for elt := (first list)
          :while (and list valid-p)   ; we don't want list to be empty
          :until (member elt '(&key &rest))
          :do (setq valid-p
                    (and valid-p
                         (cond ((eq '&optional elt)
                                t)
                               ((member elt lambda-list-keywords)
                                nil)
                               (t
                                t))))
              (setq list (rest list)))
    (when valid-p
      (cond ((eq '&key (first list))
             (when list
               (loop :for param-type :in (rest list)
                     :do (setq valid-p (and (listp param-type)
                                            (cdr param-type)
                                            (null (cddr param-type)))))))
            ((eq '&rest (first list))
             (unless (null (rest list))
               (setq valid-p nil)))
            (list
             (setq valid-p nil))))
    valid-p))

(def-test type-list (:suite :polymorphic-functions)
  (5am:is-true (type-list-p '()))
  (5am:is-true (type-list-p '(number string)))
  (5am:is-true (type-list-p '(number string &rest)))
  (5am:is-true (type-list-p '(&optional)))
  (5am:is-true (type-list-p '(&key)))
  (5am:is-true (type-list-p '(&rest)))
  (5am:is-true (type-list-p '(number &optional string)))
  (5am:is-true (type-list-p '(number &key (:a string)))))

(defun extended-type-list-p (list)
  ;; TODO: what parameter-names are valid?
  (and (type-list-p list)
       (let ((state :required)
             (extended-p nil))
         (loop :for elt :in list
               :until extended-p
               :do (setq extended-p
                         (ecase state
                           ((:required &optional) (extended-type-specifier-p elt))
                           (&key (extended-type-specifier-p (second elt)))))
                   (when (member elt lambda-list-keywords)
                     (setq state elt))
               :finally (return extended-p)))))

(deftype type-list () `(satisfies type-list-p))

(defstruct polymorph
  "
- If RUNTIME-APPLICABLE-P-FORM returns true when evaluated inside the lexical environment
of the polymorphic-function, then the dispatch is done on LAMBDA. The prioritization
is done by ADD-OR-UPDATE-POLYMORPH so that a more specialized polymorph is checked
for compatibility before a less specialized polymorph.
- The APF-COMPILER-MACRO calls the COMPILER-APPLICABLE-P-LAMBDA with the FORM-TYPEs
of the arguments derived at compile time. The compiler macro dispatches on the polymorph
at compile time if the COMPILER-APPLICABLE-P-LAMBDA returns true.
  "
  (documentation nil :type (or null string))
  (name (error "NAME must be supplied!"))
  (return-type)
  (type-list nil)
  (lambda-list-type nil)
  (effective-type-list nil)
  (compiler-applicable-p-lambda)
  (runtime-applicable-p-form)
  (inline-p)
  (inline-lambda-body)
  (static-dispatch-name)
  (compiler-macro-lambda)
  (parameters))

(defmethod print-object ((o polymorph) stream)
  (print-unreadable-object (o stream :type t)
    (with-slots (name type-list) o
      (format stream "~S ~S" name type-list))))

(defclass polymorphic-function ()
  ((name        :initarg :name
                :initform (error "NAME must be supplied.")
                :reader polymorphic-function-name)
   (lambda-list :initarg :lambda-list :type list
                :initform (error "LAMBDA-LIST must be supplied.")
                :reader polymorphic-function-lambda-list)
   (effective-lambda-list :initarg :effective-lambda-list :type list
                          :initform (error "EFFECTIVE-LAMBDA-LIST must be supplied.")
                          :reader polymorphic-function-effective-lambda-list)
   (lambda-list-type :type lambda-list-type
                     :initarg :lambda-list-type
                     :initform (error "LAMBDA-LIST-TYPE must be supplied.")
                     :reader polymorphic-function-lambda-list-type)
   (default     :initarg :default
                :initform (error ":DEFAULT must be supplied")
                :reader polymorphic-function-default
                :type function)
   (polymorphs  :initform nil
                :accessor polymorphic-function-polymorphs)
   (documentation :initarg :documentation
                  :type (or string null))
   (invalidated-p :accessor polymorphic-function-invalidated-p
                  :initform nil)
   ;; Using SOURCE doesn't simply seem to be a matter of calling (SB-C:SOURCE-LOCATION)
   #+sbcl (sb-pcl::source :initarg sb-pcl::source)
   #+sbcl (%lock
           :initform (sb-thread:make-mutex :name "GF lock")
           :reader sb-pcl::gf-lock))
  ;; TODO: Check if a symbol / list denotes a type
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((o polymorphic-function) stream)
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

(defun register-polymorphic-function (name untyped-lambda-list documentation default
                                      &key overwrite)
  (declare (type function-name       name)
           (type function            default)
           (type (or null string)    documentation)
           (type untyped-lambda-list untyped-lambda-list))
  (unless overwrite                     ; so, OVERWRITE is NIL
    (when-let (apf (and (fboundp name) (fdefinition name)))
      (if (typep apf 'polymorphic-function)
          (if (equalp untyped-lambda-list
                      (let ((lambda-list (polymorphic-function-lambda-list apf)))
                        (if-let ((key-pos  (position '&key lambda-list))
                                 (rest-pos (position '&rest lambda-list)))
                          (append (subseq lambda-list 0 rest-pos)
                                  (list '&key)
                                  (mapcar #'first (subseq lambda-list (1+ key-pos))))
                          lambda-list)))
              (return-from register-polymorphic-function name)
              (cerror "Yes, delete existing POLYMORPHs to associate new ones"
                      'lambda-list-has-changed
                      :name name
                      :new-lambda-list untyped-lambda-list))
          (cerror (format nil
                          "Yes, delete existing FUNCTION associated with ~S and associate an POLYMORPHIC-FUNCTION" name)
                  'not-a-ahp :name name))))
  (let* ((*name* name)
         (effective-lambda-list
           (polymorphic-function-make-effective-lambda-list untyped-lambda-list)))
    ;; We do not call UPDATE-POLYMORPHIC-FUNCTION-LAMBDA because
    ;; the first call is an exception: the LAMBDA-LIST received here should be used
    ;; to construct apf
    (let ((apf (make-instance 'polymorphic-function
                              :name name
                              :documentation documentation
                              :lambda-list untyped-lambda-list
                              :effective-lambda-list effective-lambda-list
                              :default default
                              :lambda-list-type (lambda-list-type untyped-lambda-list))))
      (invalidate-polymorphic-function-lambda apf)
      (setf (fdefinition name) apf)
      (setf (documentation name 'function) documentation)
      #+ccl (setf (ccl:arglist name) effective-lambda-list)
      apf)))

(defun update-polymorphic-function-lambda (polymorphic-function &optional invalidate)
  (when (and invalidate (polymorphic-function-invalidated-p polymorphic-function))
    (return-from update-polymorphic-function-lambda polymorphic-function))
  (let* ((apf polymorphic-function)
         (*name*                (polymorphic-function-name apf))
         (effective-lambda-list (polymorphic-function-effective-lambda-list apf))
         (lambda-list-type      (polymorphic-function-lambda-list-type apf))
         (lambda-body (if invalidate
                          (compute-polymorphic-function-lambda-body lambda-list-type
                                                                    effective-lambda-list
                                                                    t)
                          (compute-polymorphic-function-lambda-body lambda-list-type
                                                                    effective-lambda-list))))
    (closer-mop:set-funcallable-instance-function
     ;; A potentially relevant issue: https://github.com/Clozure/ccl/issues/361
     ;; FIXME: Should we COMPILE this?
     apf (eval `(list-named-lambda (polymorphic-function ,*name*)
                    ,(symbol-package (if (atom *name*) *name* (second *name*)))
                    ,effective-lambda-list
                  ,@lambda-body)))
    (setf (polymorphic-function-invalidated-p apf) invalidate)
    apf))

(defun invalidate-polymorphic-function-lambda (polymorphic-function)
  (update-polymorphic-function-lambda polymorphic-function t))

(defun add-or-update-polymorph (polymorphic-function polymorph)
  (declare (type polymorphic-function polymorphic-function)
           (type polymorph polymorph))
  (let* ((apf   polymorphic-function)
         (p-new polymorph)
         (polymorphs (polymorphic-function-polymorphs apf))
         (type-list  (polymorph-type-list p-new))
         (p-old (find type-list polymorphs :test #'equalp
                                           :key #'polymorph-type-list))
         (p-pos (when p-old (position p-old polymorphs))))
    ;; FIXME: Use a type-list equality check, not EQUALP
    ;; FIXME: A trivial fix for &key args is to sort them lexically
    (cond ((and p-old (numberp p-pos))
           (flet ((merge-slot (slot-name)
                    (setf (slot-value p-new slot-name)
                          (or (slot-value p-new slot-name)
                              (slot-value p-old slot-name)))))
             (merge-slot 'inline-p)
             (merge-slot 'return-type)
             (merge-slot 'effective-type-list)
             (merge-slot 'compiler-applicable-p-lambda)
             (merge-slot 'runtime-applicable-p-form)
             (merge-slot 'inline-lambda-body)
             (merge-slot 'compiler-macro-lambda))
           (setf (nth p-pos polymorphs) p-new)
           (setf (polymorphic-function-polymorphs apf) polymorphs) ; do we need this?
           t)
          (t
           (labels ((add-polymorph (polymorph polymorphs)
                      (cond ((null polymorphs)
                             (list polymorph))
                            ((type-list-subtype-p
                              (polymorph-type-list polymorph)
                              (polymorph-type-list (first polymorphs)))
                             (cons polymorph polymorphs))
                            (t
                             (setf (cdr polymorphs)
                                   (add-polymorph polymorph (rest polymorphs)))
                             polymorphs))))
             (setf (polymorphic-function-polymorphs apf)
                   (add-polymorph p-new polymorphs)))
           nil))))

(defun register-polymorph (name inline-p typed-lambda-list type-list effective-type-list
                           return-type inline-lambda-body static-dispatch-name
                           lambda-list-type runtime-applicable-p-form
                           compiler-applicable-p-lambda)
  (declare (type function-name  name)
           (type (member t nil :maybe) inline-p)
           (type typed-lambda-list typed-lambda-list)
           (type function-name  static-dispatch-name)
           (type type-list      type-list)
           (type type-list      effective-type-list)
           (type list           inline-lambda-body))
  (let* ((apf                        (fdefinition name))
         (apf-lambda-list-type       (polymorphic-function-lambda-list-type apf))
         (untyped-lambda-list        (polymorphic-function-effective-lambda-list apf)))
    (when (eq apf-lambda-list-type 'rest)
      ;; required-optional can simply be split up into multiple required or required-key
      (assert (member lambda-list-type '(rest required required-key))
              nil
              "&OPTIONAL keyword is not allowed for LAMBDA-LIST~%  ~S~%of the POLYMORPHIC-FUNCTION associated with ~S"
              untyped-lambda-list name))
    (assert (type-list-compatible-p apf-lambda-list-type type-list untyped-lambda-list)
            nil
            "TYPE-LIST ~S is not compatible with the LAMBDA-LIST ~S of the POLYMORPHs associated with ~S"
            type-list untyped-lambda-list name)
    (ensure-unambiguous-call name type-list effective-type-list)
    (let ((polymorph (make-polymorph :name name
                                     :inline-p         inline-p
                                     :type-list        type-list
                                     :return-type      return-type
                                     :lambda-list-type lambda-list-type
                                     :effective-type-list effective-type-list
                                     :compiler-applicable-p-lambda
                                     compiler-applicable-p-lambda
                                     :runtime-applicable-p-form
                                     runtime-applicable-p-form
                                     :inline-lambda-body inline-lambda-body
                                     :static-dispatch-name static-dispatch-name
                                     :compiler-macro-lambda nil
                                     :parameters
                                     (make-polymorph-parameters-from-lambda-lists
                                      untyped-lambda-list typed-lambda-list))))
      (add-or-update-polymorph apf polymorph)
      (invalidate-polymorphic-function-lambda apf)
      polymorph)))

(defvar *compiler-macro-expanding-p* nil
  "Bound to T inside the DEFINE-COMPILER-MACRO defined in DEFINE-POLYMORPH")

(defun compiler-retrieve-polymorph (name &rest arg-types-alist)
  (declare (type function-name name))
  (assert *compiler-macro-expanding-p*)
  ;; This function is used by the main compiler macro of the polymorphic-function
  ;; The RETRIEVE-POLYMORPH-FORM below is a complementary to this function.
  (let* ((apf        (fdefinition name))
         (polymorphs (polymorphic-function-polymorphs apf))
         (num-args   (length arg-types-alist)))
    (declare (optimize debug))
    (loop :for polymorph :in polymorphs
          :for lambda-list-type := (polymorph-lambda-list-type polymorph)
          :for type-list := (polymorph-type-list polymorph)
          :for app-p-lambda := (polymorph-compiler-applicable-p-lambda polymorph)
          :do (when (block app-p-lambda
                      (case lambda-list-type
                        (required
                         (if (= num-args (length type-list))
                             (apply app-p-lambda arg-types-alist)
                             nil))
                        (required-optional
                         (if (<= (position '&optional type-list)
                                 num-args
                                 (1- (length type-list)))
                             (apply app-p-lambda arg-types-alist)
                             nil))
                        (required-key
                         (let ((key-pos (position '&key type-list)))
                           (if (<= key-pos
                                   num-args
                                   (+ key-pos (* 2 (- (length type-list) key-pos 1))))
                               (apply app-p-lambda
                                      (loop :for (arg . arg-type) :in arg-types-alist
                                            :for idx :from 0
                                            :with keyword-start := key-pos
                                            :if (and (>= idx keyword-start)
                                                     (evenp (- idx keyword-start)))
                                              :collect (if (and (listp arg-type)
                                                                ;; FIXME: Use CTYPE
                                                                (member (first arg-type)
                                                                        '(eql member))
                                                                (null (cddr arg-type)))
                                                           (second arg-type)
                                                           (return-from app-p-lambda nil))
                                            :else
                                              :collect (cons arg arg-type)))
                               nil)))
                        (rest
                         (if (<= (position '&rest type-list)
                                 num-args)
                             (apply app-p-lambda arg-types-alist)
                             nil))))
                (return-from compiler-retrieve-polymorph polymorph)))))

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
         (lambda-list      (polymorphic-function-effective-lambda-list apf))
         (lambda-list-type (polymorphic-function-lambda-list-type apf))
         (polymorph-lambda-list-type
           (eswitch ((intersection type-list lambda-list-keywords) :test #'equal)
             ('(&rest)     'rest)
             ('(&key)      'required-key)
             ('(&optional) 'required-optional)
             ('()          'required)))
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
                "&OPTIONAL keyword is not allowed for LAMBDA-LIST~%  ~S~%of the POLYMORPHIC-FUNCTION associated with ~S"
                lambda-list name)
        (assert (type-list-compatible-p lambda-list-type type-list lambda-list)
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
        (add-or-update-polymorph
         apf
         (make-polymorph :name name
                         :type-list type-list
                         :lambda-list-type polymorph-lambda-list-type
                         :compiler-macro-lambda lambda))))))

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
                                (polymorph-compiler-applicable-p-lambda polymorph))
                           arg-list))
                         (apply
                          (the function
                               (polymorph-compiler-applicable-p-lambda polymorph))
                          arg-list))
                   :collect polymorph)))
    (case (length applicable-polymorphs)
      (1 (polymorph-compiler-macro-lambda (first applicable-polymorphs)))
      (0 (error 'no-applicable-polymorph/error
                :arg-list arg-list :type-lists type-lists))
      (t (error "Multiple applicable POLYMORPHs discovered for ARG-LIST ~S:~%~{~S~^    ~%~}"
                arg-list
                (mapcar #'polymorph-type-list applicable-polymorphs))))))

(defun find-polymorph (name type-list)
  "Returns two values:
If a POLYMORPHIC-FUNCTION by NAME does not exist, returns NIL NIL.
If it exists, the second value is T and the first value is a possibly empty
  list of POLYMORPHs associated with NAME."
  (declare (type function-name name))
  (let* ((apf        (and (fboundp name) (fdefinition name)))
         (polymorphs (when (typep apf 'polymorphic-function)
                       (polymorphic-function-polymorphs apf))))
    (cond ((null (typep apf 'polymorphic-function))
           (values nil nil))
          (t
           ;; FIXME: Use a type-list equality check, not EQUALP
           (loop :for polymorph :in polymorphs
                 :do (when (equalp type-list
                                   (polymorph-type-list polymorph))
                       (return-from find-polymorph
                         (values polymorph t))))
           (values nil t)))))

(defun polymorph-apropos-list-type (type &key (name nil namep)
                                           (package nil packagep))
  (assert (not (and namep packagep))
          ()
          "NAME and PACKAGE must not be supplied together!")
  (flet ((apropos-pf (name)
           (let* ((apf        (and (fboundp name) (fdefinition name)))
                  (polymorphs (when (typep apf 'polymorphic-function)
                                (polymorphic-function-polymorphs apf))))
             (cond ((null (typep apf 'polymorphic-function))
                    (values nil nil))
                   (t
                    ;; FIXME: Use a type-list equality check, not EQUALP
                    (values
                     (loop :for polymorph :in polymorphs
                           :when (accepts-argument-of-type-p
                                  (polymorph-parameters polymorph)
                                  type)
                             :collect polymorph)
                     t))))))
    (cond (namep
           (apropos-pf name))
          (packagep
           (let ((names
                   (let (l)
                     (do-symbols (s package)
                       (when (and (fboundp s)
                                  (typep (fdefinition s) 'polymorphic-function))
                         (push s l)))
                     l)))
             (mappend #'apropos-pf names)))
          (t
           (let ((names
                   (let (l)
                     (do-all-symbols (s)
                       (when (and (fboundp s)
                                  (typep (fdefinition s) 'polymorphic-function))
                         (push s l)))
                     l)))
             (mappend #'apropos-pf names))))))

(define-declaration type-like (vars env)
  ;; FIXME: Consequences of emitting CL:TYPE declaration are undefined
  (destructuring-bind (original &rest similar) vars
    (values :variable
            (loop :with type
                    := (rest (assoc 'cl:type
                                    (nth-value 2 (variable-information original env))))
                  :for var :in similar
                  :collect `(,var cl:type ,type)))))

(define-declaration inline-pf (vars env)
  (values :function
          (loop :for var :in vars
                :collect `(,var inline-pf inline-pf))))

(define-declaration notinline-pf (vars env)
  (values :function
          (loop :for var :in vars
                :collect `(,var inline-pf notinline-pf))))

(define-declaration pf-defined-before-use (args)
  (declare (ignore args))
  (values :declare
          (cons 'pf-defined-before-use t)))

(define-declaration not-pf-defined-before-use (args)
  (declare (ignore args))
  (values :declare
          (cons 'pf-defined-before-use nil)))
