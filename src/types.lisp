(in-package #:polymorphic-functions)

(5am:in-suite :polymorphic-functions)

(defstruct polymorph-parameters
  required
  optional
  rest
  keyword
  min-args
  max-args
  validator-form)

(defmethod print-object ((o polymorph-parameters) s)
  (with-slots (required optional rest keyword) o
    (format s "#<POLYMORPH-PARAMETERS")
    (when required
      (dolist (p required)
        (format s " ~A" (pp-local-name p))))
    (when optional
      (write-string "&OPTIONAL" s)
      (dolist (p required)
        (format s " ~A" (pp-local-name p))))
    (when rest
      (write-string " &REST" s)
      (format s " ~A" (pp-local-name required)))
    (when keyword
      (write-string " &KEY" s)
      (dolist (p keyword)
        (format s " ~A" (pp-local-name p))))
    (write-char #\> s)))

(defstruct (polymorph-parameter (:conc-name pp-))
  "
LOCAL-NAME : Name inside the body of the polymorph
FORM-IN-PF : The form which yields the parameter's value inside the lexical
  environment of the polymorphic-function

Note: Only LOCAL-NAME and FORM-IN-PF are relevant for &REST parameter
"
  local-name
  form-in-pf
  value-type
  default-value-form
  supplied-p-name
  value-effective-type)

(defstruct polymorph
  "
- If RUNTIME-APPLICABLE-P-FORM returns true when evaluated inside the lexical environment
of the polymorphic-function, then the dispatch is done on LAMBDA. The prioritization
is done by ADD-OR-UPDATE-POLYMORPH so that a more specialized polymorph is checked
for compatibility before a less specialized polymorph.
- The PF-COMPILER-MACRO calls the COMPILER-APPLICABLE-P-LAMBDA with the FORM-TYPEs
of the arguments derived at compile time. The compiler macro dispatches on the polymorph
at compile time if the COMPILER-APPLICABLE-P-LAMBDA returns true.

- If this POLYMORPH is used for INLINE-ing or STATIC-DISPATCH and if MORE-OPTIMAL-TYPE-LIST
or SUBOPTIMAL-NOTE is non-NIL, then emits a OPTIMIZATION-FAILURE-NOTE
  "
  (documentation nil :type (or null string))
  (name (error "NAME must be supplied!"))
  (source)
  (return-type)
  (type-list nil)
  (lambda-list-type nil)
  (typed-lambda-list nil)
  (effective-type-list nil)
  (more-optimal-type-list nil)
  (suboptimal-note nil)
  (compiler-applicable-p-lambda)
  (runtime-applicable-p-form)
  (inline-p)
  (inline-lambda-body)
  (static-dispatch-name)
  (compiler-macro-lambda)
  (compiler-macro-source)
  (parameters (error "POLYMORPH-PARAMETERS must be supplied") :type polymorph-parameters))

(defmethod print-object ((o polymorph) stream)
  (print-unreadable-object (o stream :type t)
    (with-slots (name type-list) o
      (format stream "~S ~S" name type-list))))

(with-eval-always
  (defun sbcl-version-spec-list (&optional version)
    (let ((spec-list (uiop:split-string (or version (lisp-implementation-version))
                                        :separator '(#\.))))
      (cond ((nth 2 spec-list) ; there are atleast three elements
             (handler-case (loop :for elt :in spec-list
                                 :collect (parse-integer elt :junk-allowed t))
               (parse-error () `(0 0 0))))
            (t
             `(0 0 0)))))

  (defun sbcl-version-spec-integer (&optional version)
    (destructuring-bind (major-1 major-2 minor &rest sub-minor)
        (sbcl-version-spec-list version)
      (declare (ignore sub-minor))
      (+ (* 12 (+ (* 10 major-1) major-2))
         minor))))

(define-constant +optimize-speed-or-compilation-speed+
    ;; optimize for compilation-speed for SBCL>2.2.3 else for speed
    (if (and (string= "SBCL" (lisp-implementation-type))
             (let ((spec-integer (sbcl-version-spec-integer (lisp-implementation-version))))
               (if (zerop spec-integer)
                   (progn
                     (warn "Detected implementation to be SBCL but could not parse version ~A as X.Y.Z~%Optimizing polymorphic-functions for compilation-speed." (lisp-implementation-version))
                     t)
                   (< (sbcl-version-spec-integer "2.2.3")
                      spec-integer))))
        `(optimize compilation-speed)
        `(optimize speed))
  :test #'equal)

(defclass polymorphic-function ()
  ((name        :initarg :name
                :initform (error "NAME must be supplied.")
                :reader polymorphic-function-name)
   (source :initarg :source :reader polymorphic-function-source)
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
   (dispatch-declaration :initarg :dispatch-declaration
                         :initform +optimize-speed-or-compilation-speed+
                         :accessor polymorphic-function-dispatch-declaration)
   (default     :initarg :default
                :initform (error ":DEFAULT must be supplied")
                :reader polymorphic-function-default
                :type function)
   (polymorphs  :initform nil
                :accessor polymorphic-function-polymorphs)
   (documentation :initarg :documentation
                  :type (or string null)
                  :accessor polymorphic-function-documentation)
   (invalidated-p :accessor polymorphic-function-invalidated-p
                  :initform nil)
   #+sbcl (%lock
           :initform (sb-thread:make-mutex :name "GF lock")
           :reader sb-pcl::gf-lock))
  ;; TODO: Check if a symbol / list denotes a type
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((o polymorphic-function) stream)
  (print-unreadable-object (o stream :type t)
    (with-slots (name polymorphs) o
      (format stream "~S (~S)" name (length polymorphs)))))

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

(def-test type-list ()
  (5am:is-true (type-list-p '()))
  (5am:is-true (type-list-p '(number string)))
  (5am:is-true (type-list-p '(number string &rest)))
  (5am:is-true (type-list-p '(&optional)))
  (5am:is-true (type-list-p '(&key)))
  (5am:is-true (type-list-p '(&rest)))
  (5am:is-true (type-list-p '(number &optional string)))
  (5am:is-true (type-list-p '(number &key (:a string)))))

(deftype type-list () `(satisfies type-list-p))

(defun type-list-order-keywords (type-list)
  (let ((key-position (position '&key type-list)))
    (if key-position
        (append (subseq type-list 0 (1+ key-position))
                (sort (copy-list (subseq type-list (1+ key-position))) #'string< :key #'first))
        type-list)))

(define-constant +lambda-list-types+
    (list 'required
          'required-optional
          'required-key
          'rest)
  :test #'equalp)

(defun lambda-list-type-p (object)
  "Checks whhether the OBJECT is in +LAMBDA-LIST-TYPES+"
  (member object +lambda-list-types+))

(deftype lambda-list-type () `(satisfies lambda-list-type-p))

(defun untyped-lambda-list-p (lambda-list)
  (ignore-errors (lambda-list-type lambda-list)))
(defun typed-lambda-list-p (lambda-list)
  (ignore-errors (lambda-list-type lambda-list :typed t)))
(deftype untyped-lambda-list ()
  "Examples:
  (a b)
  (a b &optional c)
Non-examples:
  ((a string))"
  `(satisfies untyped-lambda-list-p))
(deftype typed-lambda-list ()
  "Examples:
  ((a integer) (b integer))
  ((a integer) &optional ((b integer) 0 b-supplied-p))"
  `(satisfies typed-lambda-list-p))
