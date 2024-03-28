(in-package #:polymorphic-functions)

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

(defun register-polymorphic-function
    (name untyped-lambda-list documentation default
     &key overwrite source declaration)
  (declare (type function-name       name)
           (type function            default)
           (type (or null string)    documentation)
           (type untyped-lambda-list untyped-lambda-list))
  (unless overwrite                     ; so, OVERWRITE is NIL
    (when-let (apf (and (fboundp name) (fdefinition name)))
      (if (typep apf 'polymorphic-function)
          (if (let ((old-lambda-list (polymorphic-function-lambda-list apf))
                    (new-lambda-list untyped-lambda-list))
                (let ((optional-pos-old (position '&optional old-lambda-list))
                      (optional-pos-new (position '&optional new-lambda-list))
                      (key-pos-old (position '&key old-lambda-list))
                      (key-pos-new (position '&key new-lambda-list))
                      (rest-pos-old (position '&rest old-lambda-list))
                      (rest-pos-new (position '&rest new-lambda-list)))
                  ;; lambda-lists are compatible and don't need to be changed
                  (and (eql optional-pos-old  optional-pos-new)
                       (eql key-pos-old  key-pos-new)
                       (eql rest-pos-old rest-pos-new)
                       (if (and key-pos-old key-pos-new)
                           (equal (subseq new-lambda-list key-pos-new)
                                  (subseq old-lambda-list key-pos-old))
                           t))))
              (progn
                (setf (polymorphic-function-documentation apf) documentation)
                (update-polymorphic-function-documentation name)
                (setf (polymorphic-function-dispatch-declaration apf) declaration)
                (invalidate-polymorphic-function-lambda apf)
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
                              :dispatch-declaration declaration
                              :source source
                              :documentation documentation
                              :lambda-list untyped-lambda-list
                              :effective-lambda-list effective-lambda-list
                              :default default
                              :lambda-list-type (lambda-list-type untyped-lambda-list))))
      (invalidate-polymorphic-function-lambda apf)
      (setf (fdefinition name) apf)
      (update-polymorphic-function-documentation name)
      #+ccl (setf (ccl:arglist name) effective-lambda-list)
      apf)))

(defun update-polymorphic-function-documentation (name)
  (setf (documentation name 'cl:function)
        (let* ((pf         (fdefinition name))
               (pf-doc     (polymorphic-function-documentation pf))
               (polymorphs (polymorphic-function-polymorphs pf)))
          (when (or pf-doc polymorphs)
            (with-output-to-string (*standard-output*)
              (pprint-logical-block (nil nil)
                (when pf-doc
                  (write-string pf-doc)
                  (pprint-newline :mandatory))
                (when polymorphs
                  (write-string "Polymorphs:")
                  (pprint-indent :block 2)
                  (pprint-newline :mandatory)
                  (loop :for polymorph :in polymorphs
                        :do (with-slots (effective-type-list documentation) polymorph
                              (write (cons name effective-type-list))
                              (when documentation
                                (pprint-newline :mandatory)
                                (write-string "  Documentation:")
                                (pprint-newline :mandatory)
                                (pprint-logical-block (nil nil :per-line-prefix "    ")
                                  (write-string documentation)))
                              (pprint-newline :mandatory)))
                  (pprint-indent :block -2))))))))

(defun update-polymorphic-function-lambda (polymorphic-function &optional invalidate)
  (when (and invalidate (polymorphic-function-invalidated-p polymorphic-function))
    (return-from update-polymorphic-function-lambda polymorphic-function))
  (let* ((apf polymorphic-function)
         (*name*                (polymorphic-function-name apf))
         (effective-lambda-list (polymorphic-function-effective-lambda-list apf))
         (lambda-list-type      (polymorphic-function-lambda-list-type apf))
         (declaration           (polymorphic-function-dispatch-declaration apf))
         (lambda-body (if invalidate
                          (compute-polymorphic-function-lambda-body lambda-list-type
                                                                    effective-lambda-list
                                                                    declaration
                                                                    t)
                          (compute-polymorphic-function-lambda-body lambda-list-type
                                                                    effective-lambda-list
                                                                    declaration)))
         ;; FIXME: Should we COMPILE this?
         (function    (eval `(list-named-lambda (polymorphic-function ,*name*)
                                 ,(symbol-package (if (atom *name*) *name* (second *name*)))
                                 ,effective-lambda-list
                               ,@lambda-body))))
    (closer-mop:set-funcallable-instance-function apf function)
    ;; Relevant issue: https://github.com/Clozure/ccl/issues/361
    #+ccl (ccl::lfun-bits apf
                          (logior (ccl::lfun-bits apf)
                                  (logand (ccl::lfun-bits function) (1- (expt 2 16)))))
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

(defun register-polymorph (name inline-p documentation typed-lambda-list type-list
                           effective-type-list more-optimal-type-list suboptimal-note
                           return-type inline-lambda-body static-dispatch-name
                           lambda-list-type runtime-applicable-p-form
                           compiler-applicable-p-lambda &optional source-location)
  (declare (type function-name  name)
           (type (member t nil :maybe) inline-p)
           (type (or null string) documentation)
           (type typed-lambda-list typed-lambda-list)
           (type function-name  static-dispatch-name)
           (type type-list      type-list)
           (type type-list      effective-type-list)
           (type type-list      more-optimal-type-list)
           (type symbol         suboptimal-note)
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
                                     :documentation    documentation
                                     :source           source-location
                                     :inline-p         inline-p
                                     :type-list        type-list
                                     :return-type      return-type
                                     :typed-lambda-list typed-lambda-list
                                     :lambda-list-type lambda-list-type
                                     :effective-type-list effective-type-list
                                     :more-optimal-type-list more-optimal-type-list
                                     :suboptimal-note  suboptimal-note
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
      (update-polymorphic-function-documentation name)
      polymorph)))

(defun remove-polymorph (name type-list)
  (let ((apf (fdefinition name)))
    (when apf
      (removef (polymorphic-function-polymorphs apf) type-list
               :test #'equalp :key #'polymorph-type-list))))

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
                 :do (when (and (type-list-more-specific-p type-list
                                                           (polymorph-type-list polymorph))
                                (type-list-more-specific-p (polymorph-type-list polymorph)
                                                           type-list))
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

(defvar *compiler-macro-expanding-p* nil
  "Bound to T inside the DEFINE-COMPILER-MACRO defined in DEFINE-POLYMORPH")
