(in-package polymorphic-functions)

(defun blockify-name (name)
  (etypecase name
    (symbol name)
    (list
     (assert (and (eq 'setf (first name))
                  (second name)
                  (null (nthcdr 2 name))))
     (second name))))

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
                                      &key overwrite source)
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
              (progn
                (setf (documentation name 'cl:function) documentation)
                (return-from register-polymorphic-function name))
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
                              :source source
                              :documentation documentation
                              :lambda-list untyped-lambda-list
                              :effective-lambda-list effective-lambda-list
                              :default default
                              :lambda-list-type (lambda-list-type untyped-lambda-list))))
      (invalidate-polymorphic-function-lambda apf)
      (setf (fdefinition name) apf)
      (setf (documentation name 'cl:function) documentation)
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
  "Returns T if the POLYMOMRPH with identical effective-type-list existed, otherwise returns NIL."
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
           (unless (slot-value p-new 'compiler-macro-lambda)
             (setf (slot-value p-new 'compiler-macro-lambda)
                   (slot-value p-old 'compiler-macro-lambda))
             (setf (slot-value p-new 'compiler-macro-source)
                   (slot-value p-old 'compiler-macro-source)))
           ;; We replace p-old with p-new in the list POLYMORPHS
           ;; In doing so, the only thing we might need to preserve is the COMPILER-MACRO-LAMBDA
           (setf (nth p-pos polymorphs) p-new)
           (setf (polymorphic-function-polymorphs apf) polymorphs) ; do we need this?
           (equalp (slot-value p-new 'effective-type-list)
                   (slot-value p-old 'effective-type-list)))
          (t
           (labels ((add-polymorph (polymorph polymorphs)
                      (cond ((null polymorphs)
                             (list polymorph))
                            ((type-list-more-specific-p
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
                           compiler-applicable-p-lambda &optional source-location)
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
            "TYPE-LIST~%  ~S~%is not compatible with the LAMBDA-LIST~%  ~S~%of the POLYMORPHs associated with ~S"
            type-list untyped-lambda-list name)
    (ensure-unambiguous-call name type-list effective-type-list)
    (let ((polymorph (make-polymorph :name             name
                                     :source           source-location
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
      (unless (add-or-update-polymorph apf polymorph)
        (invalidate-polymorphic-function-lambda apf))
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

(defun register-polymorph-compiler-macro (name type-list lambda &optional source)
  (declare (type function-name name)
           (type type-list type-list)
           (type function lambda))
  ;; TODO: Comment why this became impossible
  (assert (find-polymorph name type-list)
          ()
          "Illegal to have a POLYMORPH-COMPILER-MACRO without a corresponding POLYMORPH")
  (let* ((apf              (fdefinition name))
         (lambda-list      (polymorphic-function-effective-lambda-list apf))
         (lambda-list-type (polymorphic-function-lambda-list-type apf))
         (type-list   (type-list-order-keywords type-list)))
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
    (let ((polymorph (find type-list (polymorphic-function-polymorphs apf)
                           :test #'equalp
                           :key #'polymorph-type-list)))
      (setf (polymorph-compiler-macro-lambda polymorph) lambda)
      (setf (polymorph-compiler-macro-source polymorph) source))))

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
                       (polymorphic-function-polymorphs apf)))
         (type-list  (type-list-order-keywords type-list)))
    (cond ((null (typep apf 'polymorphic-function))
           (values nil nil))
          (t
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

#-extensible-compound-types
(define-declaration type-like (vars env)
  ;; FIXME: Consequences of emitting CL:TYPE declaration are undefined
  (destructuring-bind (original &rest similar) vars
    (values :variable
            (loop :with type
                    := (rest (assoc 'cl:type
                                    (nth-value 2 (variable-information original env))))
                  :for var :in similar
                  :collect `(,var cl:type ,type)))))

#+extensible-compound-types
(define-declaration type-like (vars env)
  ;; FIXME: Consequences of emitting CL:TYPE declaration are undefined
  (destructuring-bind (original &rest similar) vars
    (values :variable
            (loop :with type
                    := (rest (assoc 'extensible-compound-types:extype
                                    (nth-value 2 (variable-information original env))))
                  :for var :in similar
                  :collect `(,var extensible-compound-types:extype ,type)
                  :collect `(,var cl:type ,(upgraded-cl-type type env))))))

(define-declaration inline-pf (vars env)
  (values :function
          (loop :for var :in vars
                :collect `(,var inline-pf inline-pf))))

(define-declaration notinline-pf (vars env)
  (values :function
          (loop :for var :in vars
                :collect `(,var inline-pf notinline-pf))))

(define-declaration pf-defined-before-use (args)
  ;; (declare (ignore args))
  (values :declare
          (cons 'pf-defined-before-use t)))

(define-declaration not-pf-defined-before-use (args)
  ;; (declare (ignore args))
  (values :declare
          (cons 'pf-defined-before-use nil)))
