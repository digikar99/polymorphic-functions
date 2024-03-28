(in-package #:polymorphic-functions)

(defun recursive-function-p (name body)
  (flet ((%recursive-p (node)
           (if (listp node)
               (if (eq name (first node))
                   (return-from recursive-function-p t)
                   node)
               nil)))
    (traverse-tree body #'%recursive-p)
    nil))

;;; - run-time correctness requires
;;;   - DEFINE-POLYMORPH-FUNCTION -> DEFUN
;;;   - DEFPOLYMORPH
;;; - compile-time correctness requires
;;;   - DEFINE-POLYMORPH-FUNCTION -> DEFINE-COMPILER-MACRO
;;;   - GET-TYPE-LIST
;;;   - DEFPOLYMORPH-COMPILER-MACRO

(defmacro define-polymorphic-function (name untyped-lambda-list
                                       &key overwrite
                                         (documentation nil docp)
                                         (default '(function no-applicable-polymorph))
                                         (dispatch-declaration
                                          ''#.+optimize-speed-or-compilation-speed+)
                                       &environment env)
  "Define a function named NAME that can then be used for DEFPOLYMORPH
for specializing on various argument types.

If OVERWRITE is T, all the existing polymorphs associated with NAME are deleted,
and new polymorphs will be ready to be installed.
If OVERWRITE is NIL, a continuable error is raised if the LAMBDA-LIST has changed.

DEFAULT should be a FUNCTION that can be called with two arguments at run-time
and compile-time in case no polymorph is applicable.
- the first of these arguments is the NAME, while
- the second argument is the argument list with which the polymorphic-function
  was called or compiled.
At compile-time *COMPILER-MACRO-EXPANDING-P* is bound to non-NIL."
  (declare (type function-name       name)
           (type untyped-lambda-list untyped-lambda-list))
  (when (and docp (symbolp documentation) (constantp documentation))
    (setq documentation (constant-form-value documentation env)))
  (when docp (check-type documentation string))
  (let* ((*name*        name)
         (*environment*  env)
         (untyped-lambda-list (normalize-untyped-lambda-list untyped-lambda-list))
         (untyped-lambda-list (if (member '&key untyped-lambda-list)
                                  (let* ((key-position (position '&key untyped-lambda-list)))
                                    (append (subseq untyped-lambda-list 0 key-position)
                                            '(&key)
                                            (sort (subseq untyped-lambda-list (1+ key-position))
                                                  #'string<
                                                  :key (lambda (param)
                                                         (if (and (listp param)
                                                                  (null (cddr param)))
                                                             (car param)
                                                             param)))))
                                  untyped-lambda-list)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,(when overwrite
            `(undefine-polymorphic-function ',name))
         (register-polymorphic-function ',name ',untyped-lambda-list ,documentation
                                        ,default
                                        :source #+sbcl (sb-c:source-location) #-sbcl nil
                                        :declaration ,dispatch-declaration)
         #+sbcl (sb-c:defknown ,name * * nil :overwrite-fndb-silently t))
       (fdefinition ',name))))

(defun extract-declarations (body &key documentation)
  "Returns two values: DECLARATIONS and remaining BODY
If DOCUMENTATION is non-NIL, returns three values: DECLARATIONS and remaining BODY and DOC-STRING"
  (labels ((%extract-declarations (body)
             (cond ((null body)
                    (values `(declare) nil))
                   ((and (listp (car body))
                         (eq 'declare (caar body)))
                    (multiple-value-bind (declarations rest-body)
                        (%extract-declarations (rest body))
                      (values (nconc (cons 'declare (cdar body))
                                     (rest declarations))
                              rest-body)))
                   (t
                    (values `(declare) body)))))
    (multiple-value-bind (declarations rest-body)
        (if (and documentation
                 (stringp (first body)))
            (%extract-declarations (rest body))
            (%extract-declarations body))
      (if (and documentation
               (stringp (first body)))
          (values declarations rest-body (first body))
          (values declarations rest-body)))))

(defun ensure-unambiguous-call (name type-list effective-type-list)
  (declare (optimize debug))
  (loop :for polymorph :in (polymorphic-function-polymorphs (fdefinition name))
        :for existing-type-list := (polymorph-type-list polymorph)
        :for existing-effective-type-list := (polymorph-effective-type-list polymorph)
        :for new-specific-p
          := (type-list-more-specific-p effective-type-list existing-effective-type-list)
        :for existing-specific-p
          := (type-list-more-specific-p existing-effective-type-list effective-type-list)
        :do (when (or (and new-specific-p
                           existing-specific-p
                           (not (equalp type-list existing-type-list)))
                      (and (not new-specific-p)
                           (not existing-specific-p)
                           (not (type-list-intersection-null-p effective-type-list
                                                               existing-effective-type-list))))
              (cerror "Undefine existing polymorph"
                      "The given TYPE-LIST ~%  ~S~%effectively~%  ~S~%will cause ambiguous call with an existing polymorph with type list ~%  ~S~%and effective type list~%  ~S~%"
                      type-list
                      effective-type-list
                      existing-type-list
                      existing-effective-type-list)
              (undefpolymorph name existing-type-list))))

(defmacro defpolymorph (name typed-lambda-list return-type
                        &body body &environment env)
  "  Expects OPTIONAL or KEY args to be in the form

    ((A TYPE) DEFAULT-VALUE) or ((A TYPE) DEFAULT-VALUE AP).

  - NAME could also be
      (NAME
        &KEY
        STATIC-DISPATCH-NAME
        INVALIDATE-PF)

  - STATIC-DISPATCH-NAME could be useful for tracing or profiling

  - If INVALIDATE-PF is non-NIL then the associated polymorphic-function
    is forced to recompute its dispatching after this polymorph is defined.
"
  (destructuring-bind (name
                       &key
                         (static-dispatch-name nil static-dispatch-name-p)
                         invalidate-pf)
      (if (typep name 'function-name)
          (list name)
          name)
    (declare (type function-name name)
             (optimize debug))
    (let+ ((block-name       (blockify-name name))
           (*environment*    env)
           ((&values unsorted-typed-lambda-list ignorable-list)
            (normalize-typed-lambda-list typed-lambda-list))
           (typed-lambda-list (if (member '&key unsorted-typed-lambda-list)
                                  (let ((key-position
                                          (position '&key
                                                    unsorted-typed-lambda-list)))
                                    (append (subseq unsorted-typed-lambda-list
                                                    0 key-position)
                                            '(&key)
                                            (sort (subseq unsorted-typed-lambda-list
                                                          (1+ key-position))
                                                  #'string<
                                                  :key #'caar)))
                                  unsorted-typed-lambda-list))
           (untyped-lambda-list (untyped-lambda-list typed-lambda-list))
           (pf-lambda-list      (if (and (fboundp name)
                                         (typep (fdefinition name) 'polymorphic-function))
                                    (mapcar (lambda (elt)
                                              (if (atom elt) elt (first elt)))
                                            (polymorphic-function-lambda-list
                                             (fdefinition name)))
                                    untyped-lambda-list))
           (parameters          (make-polymorph-parameters-from-lambda-lists
                                 pf-lambda-list typed-lambda-list))
           (lambda-list-type  (lambda-list-type typed-lambda-list :typed t)))
      (declare (type typed-lambda-list typed-lambda-list))

      ;; USE OF INTERN BELOW:
      ;;   We do want STATIC-DISPATCH-NAME symbol collision to actually take place
      ;; when type lists of two polymorphs are "equivalent".
      ;;   (Credits to phoe for pointing out in the issue at
      ;;   https://github.com/digikar99/polymorphic-functions/issues/3)
      ;;   Consider a file A to be
      ;; compiled before restarting a lisp image, and file B after the
      ;; restart. The use of GENTEMP meant that two "separate" compilations of
      ;; the same polymorph in the two files, could result in different
      ;; STATIC-DISPATCH-NAMEs. If the two files were then loaded
      ;; simultaneously, and the polymorphs static-dispatched at some point,
      ;; then there remained the possibility that different static-dispatches
      ;; could be using "different versions" of the polymorph.
      ;;   Thus, we actually do want collisions to take place so that a same
      ;; deterministic/latest version of the polymorph is called; therefore we
      ;; use INTERN.
      (let+ (((&values param-list type-list effective-type-list)
              (polymorph-effective-lambda-list parameters))
             ((&values declarations body doc)
              (extract-declarations body :documentation t))
             (static-dispatch-name
              (if static-dispatch-name-p
                  static-dispatch-name
                  (let* ((p-old
                           (and (fboundp name)
                                (typep (fdefinition name)
                                       'polymorphic-function)
                                (find-polymorph name type-list)))
                         (old-name
                           (when p-old
                             (polymorph-static-dispatch-name
                              p-old))))
                    (if old-name
                        old-name
                        (let ((*package* (find-package
                                          '#:polymorphic-functions.nonuser)))
                          (intern (write-to-string
                                   `(polymorph ,name ,type-list))
                                  '#:polymorphic-functions.nonuser))))))
             (lambda-declarations (lambda-declarations parameters))
             (lambda-body
              `(list-named-lambda (polymorph ,name ,type-list)
                   ,(symbol-package block-name)
                   ,param-list
                 (declare (ignorable ,@ignorable-list))
                 ,lambda-declarations
                 ,declarations
                 ,(multiple-value-bind (form form-return-type)
                      (ensure-type-form return-type
                                        `(block ,block-name
                                           (locally ,@body))
                                        env)
                    (setq return-type form-return-type)
                    form))))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (unless (and (fboundp ',name)
                        (typep (function ,name) 'polymorphic-function))
             (define-polymorphic-function ,name ,untyped-lambda-list))
           (setf (fdefinition ',static-dispatch-name) ,lambda-body)
           ,(let* ((ftype (ftype-for-static-dispatch static-dispatch-name
                                                     effective-type-list
                                                     return-type
                                                     env))
                   (proclaimation
                     `(proclaim ',ftype)))
              (if optim-debug
                  proclaimation
                  `(handler-bind ((warning #'muffle-warning))
                     ,proclaimation)))
           (register-polymorph ',name nil
                               ',doc
                               ',typed-lambda-list
                               ',type-list
                               ',effective-type-list
                               nil
                               nil
                               ',return-type
                               nil
                               ',static-dispatch-name
                               ',lambda-list-type
                               ',(run-time-applicable-p-form parameters)
                               ,(compiler-applicable-p-lambda-body parameters)
                               #+sbcl (sb-c:source-location))
           ,(when invalidate-pf
              `(invalidate-polymorphic-function-lambda (fdefinition ',name)))
           ',name)))))

(defun undefpolymorph (name type-list)
  "Remove the POLYMORPH associated with NAME with TYPE-LIST"
  (remove-polymorph name type-list)
  (update-polymorphic-function-lambda (fdefinition name) t))

(defun undefine-polymorphic-function (name)
  "Remove the POLYMORPH(-WRAPPER) defined by DEFINE-POLYMORPH"
  (fmakunbound name))
