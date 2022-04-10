(in-package polymorphic-functions)

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
  (source)
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
  (compiler-macro-source)
  (parameters (error "POLYMORPH-PARAMETERS must be supplied") :type polymorph-parameters))

(defmethod print-object ((o polymorph) stream)
  (print-unreadable-object (o stream :type t)
    (with-slots (name type-list) o
      (format stream "~S ~S" name type-list))))

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
               :do (if (member elt lambda-list-keywords)
                       (setq state elt)
                       (setq extended-p
                             (ecase state
                               ((:required &optional) (extended-type-specifier-p elt))
                               (&key (extended-type-specifier-p (second elt))))))
               :finally (return extended-p)))))

(deftype type-list () `(satisfies type-list-p))

(defun type-list-order-keywords (type-list)
  (let ((key-position (position '&key type-list)))
    (if key-position
        (append (subseq type-list 0 (1+ key-position))
                (sort (copy-list (subseq type-list (1+ key-position))) #'string< :key #'first))
        type-list)))

