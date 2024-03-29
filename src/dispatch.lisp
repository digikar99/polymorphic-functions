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
         (untyped-lambda-list (normalize-untyped-lambda-list untyped-lambda-list))
         (untyped-lambda-list (sort-untyped-lambda-list untyped-lambda-list)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,(when overwrite `(undefine-polymorphic-function ',name))
         (register-polymorphic-function ',name ',untyped-lambda-list ,documentation
                                        ,default
                                        :source #+sbcl (sb-c:source-location) #-sbcl nil
                                        :declaration ,dispatch-declaration)
         #+sbcl (sb-c:defknown ,name * * nil :overwrite-fndb-silently t)
         ,(when (fboundp 'pf-compiler-macro)
            `(setf (compiler-macro-function ',name) #'pf-compiler-macro))
         (fdefinition ',name)))))

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

(defun expand-defpolymorph-lite
    (name typed-lambda-list return-type body env)
  (destructuring-bind
      (name &rest keys
       &key invalidate-pf (static-dispatch-name nil static-dispatch-name-p)
       &allow-other-keys)
      (if (typep name 'function-name)
          (list name)
          name)
    (declare (type function-name name)
             (optimize debug))
    (remf keys :invalidate-pf)
    (remf keys :static-dispatch-name)
    (assert (null keys) ()
            "The only legal options for DEFPOLYMORPH are:~%  STATIC-DISPATCH-NAME and INVALIDATE-PF~%Did you intend to polymorphic-functions instead of polymorphic-functions-lite?")
    (let+ ((block-name       (blockify-name name))
           (*environment*    env)
           ((&values unsorted-typed-lambda-list ignorable-list)
            (normalize-typed-lambda-list typed-lambda-list))
           (typed-lambda-list (sort-typed-lambda-list unsorted-typed-lambda-list))
           (untyped-lambda-list (untyped-lambda-list typed-lambda-list))
           (pf-lambda-list      (may-be-pf-lambda-list name untyped-lambda-list))
           (parameters          (make-polymorph-parameters-from-lambda-lists
                                 pf-lambda-list typed-lambda-list))
           (lambda-list-type  (lambda-list-type typed-lambda-list :typed t))
           ((&values param-list type-list effective-type-list)
            (polymorph-effective-lambda-list parameters))
           ((&values declarations body doc)
            (extract-declarations body :documentation t))
           (static-dispatch-name
            (if static-dispatch-name-p
                static-dispatch-name
                (make-or-retrieve-static-dispatch-name name type-list)))
           (lambda-declarations (lambda-declarations parameters))
           ((&values ensure-type-form return-type)
            (ensure-type-form return-type block-name body
                              :variable
                              (remove-duplicates
                               (remove-if
                                #'null
                                (mapcar #'third
                                        (rest lambda-declarations))))
                              :declare
                              (remove-duplicates
                               (rest lambda-declarations)
                               :test #'equal)))
           (lambda-body
            `(list-named-lambda (polymorph ,name ,type-list)
                 ,(symbol-package block-name)
                 ,param-list
               (declare (ignorable ,@ignorable-list))
               ,lambda-declarations
               ,declarations
               ,ensure-type-form))
           ;; LAMBDA-BODY contains the ENSURE-TYPE-FORM that performs
           ;;   run time checks on the return types.
           (ftype-proclaimation
            (ftype-proclaimation
             static-dispatch-name effective-type-list return-type env)))

      `(eval-when (:compile-toplevel :load-toplevel :execute)

         (unless (and (fboundp ',name)
                      (typep (function ,name) 'polymorphic-function))
           (define-polymorphic-function ,name ,untyped-lambda-list))

         (setf (fdefinition ',static-dispatch-name) ,lambda-body)
         ,ftype-proclaimation
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
                             nil
                             #+sbcl (sb-c:source-location))
         ,(when invalidate-pf
            `(invalidate-polymorphic-function-lambda (fdefinition ',name)))
         ',name))))

;;; CLHS recommends that
;;;   Macros intended for use in top level forms should be written so that
;;;   side-effects are done by the forms in the macro expansion. The
;;;   macro-expander itself should not do the side-effects.
;;; Reference: http://clhs.lisp.se/Body/s_eval_w.htm

(defmacro defpolymorph (&whole whole name typed-lambda-list return-type
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
  (if (fboundp 'pf-compiler-macro)
      (uiop:symbol-call '#:polymorphic-functions
                        '#:expand-defpolymorph-full
                        whole name typed-lambda-list return-type body env)
      (expand-defpolymorph-lite name typed-lambda-list return-type body env)))

(defun undefpolymorph (name type-list)
  "Remove the POLYMORPH associated with NAME with TYPE-LIST"
  ;; FIXME: Undefining polymorphs can also lead to polymorph call ambiguity.
  ;; One (expensive) solution is to insert afresh the type lists of all polymorphs
  ;; to resolve it.
  #+sbcl
  (let ((info  (sb-c::fun-info-or-lose name))
        (ctype (sb-c::specifier-type (list 'function type-list '*))))
    (setf (sb-c::fun-info-transforms info)
          (remove-if (curry #'sb-c::type= ctype)
                     (sb-c::fun-info-transforms info)
                     :key #'sb-c::transform-type)))
  (remove-polymorph name type-list)
  (update-polymorphic-function-lambda (fdefinition name) t))

(defun undefine-polymorphic-function (name)
  "Remove the POLYMORPH(-WRAPPER) defined by DEFINE-POLYMORPH"
  (fmakunbound name)
  #+sbcl (sb-c::undefine-fun-name name)
  (setf (compiler-macro-function name) nil))
