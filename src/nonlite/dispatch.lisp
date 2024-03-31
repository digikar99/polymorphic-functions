(in-package #:polymorphic-functions)

(defmacro with-muffled-compilation-warnings (&body body)
  `(locally (declare #+sbcl (sb-ext:muffle-conditions warning))
     (let ((*error-output* (make-string-output-stream)))
       (let (#+sbcl (sb-c::*in-compilation-unit* nil)
             (*compile-verbose* nil))
         (#+sbcl progn
          #-sbcl with-compilation-unit #-sbcl (:override t)
          ;; TODO: Improve error reporting on other systems
          ;; This works on SBCL and CCL
          ,@body)))))

(defun null-env-compilation-warnings (lambda-form)
  (let* ((warnings))
    (with-muffled-compilation-warnings
      (handler-bind ((warning (lambda (c)
                                (push c warnings)
                                (muffle-warning c))))
        (compile nil (ecase (first lambda-form)
                       (cl:lambda
                        lambda-form)))))
    (if warnings
        (format nil "~{~A~^~%~}" (nreverse warnings))
        nil)))

(defmacro with-safe-inline ((inline-safe-lambda-body-var inline-note-var
                             &key whole name inlinep inline inline-lambda-body)
                            &body body)
  (assert (every #'symbolp
                 (list inline-safe-lambda-body-var inline-note-var
                       inlinep inline)))
  `(multiple-value-bind (,inline-safe-lambda-body-var ,inline-note-var)
       (cond ((and ,inlinep ,inline)
              (values ,inline-lambda-body
                      (if-let (null-env-compilation-warnings
                               (null-env-compilation-warnings ,inline-lambda-body))
                        (with-output-to-string (*error-output*)
                          (note-null-env inline-lambda-body
                                         null-env-compilation-warnings))
                        nil)))
             ((and ,inlinep (not ,inline))
              (values nil nil))
             ((and (not ,inlinep)
                   (recursive-function-p ,name ,inline-lambda-body))
              (values nil
                      (with-output-to-string (*error-output*)
                        (note-no-inline ,whole "it is suspected to result in infinite recursive expansion;~%  supply :INLINE T option to override and proceed at your own risk"))))
             (t
              (if-let (null-env-compilation-warnings
                       (null-env-compilation-warnings ,inline-lambda-body))
                (values nil
                        (with-output-to-string (*error-output*)
                          (note-no-inline ,whole "~%")
                          (pprint-logical-block (*error-output* nil
                                                                :per-line-prefix "  ")
                            (note-null-env ,inline-lambda-body
                                           null-env-compilation-warnings))
                          (format *error-output* "~&PROCEED AT YOUR OWN RISK!~%~%")))
                (values ,inline-lambda-body
                        nil))))

     ,@body))

;;; Earlier (until commit e7f11394023cf06075459ea4baa735ec8bda89f3 of polymorphic-functions),
;;; we attempted to use a code-walker to determine if there are
;;; free variables in the form, and accordingly decline to inline
;;; the polymorph. However, cases such as this (and while this is nonsense):
;;;   (define-polymorphic-function foo (a) :overwrite t)
;;;   (let ((a 5))
;;;     (defpolymorph foo ((symbol (eql a))) t
;;;       (declare (ignore symbol))
;;;       a)
;;;     (defun bar () (foo 'a)))
;;; demand a user supplied value for INLINE. We put the same to use and avoid
;;; depending on the code-walker altogether.

;;; The BODY of the DEFPOLYMORPH FORM may contain macro calls referenced to the
;;; null lexenv. When this BODY gets substituted in order to INLINE the PF,
;;; we want to avoid MACROLET (and FLET, LABELS) from overriding the elements
;;; of the null lexenv. That is what the behavior of INLINE-d functions defined
;;; by DEFUN is. (Does the spec say that?)
;;;   The MACROLET can be taken care of by MACROEXPAND-ALL. However, because
;;; compiling to support SUBTYPE and PARAMETRIC polymorphism requires type
;;; information that is only available at the call-compilation site rather than
;;; at the defpolymorph-definition site. Thus, the MACROEXPAND-ALL must happen
;;; within the pf-compiler-macro rather than DEFPOLYMORPH below.
;;;   That still leaves FLET and LABELS though. And that does form a limitation
;;; of polymorphic functions at the time of this writing.

;;; Do minimal work at macro-expansion time?
;;; 1. Well, to be able to handle closures, the compilation phase of the lambda
;;;    needs the env. However, env objects cannot be dumped; nor does it seem like
;;;    a wise idea to do so.
;;; 2. That means, the minimum work that we need to do during macroexpansion time
;;;    involves the emission of the lambda-body.

(setf (documentation 'defpolymorph 'cl:function)
      "  Expects OPTIONAL or KEY args to be in the form

    ((A TYPE) DEFAULT-VALUE) or ((A TYPE) DEFAULT-VALUE AP).

  - NAME could also be
      (NAME
        &KEY (INLINE T)
        STATIC-DISPATCH-NAME
        INVALIDATE-PF
        MORE-OPTIMAL-TYPE-LIST
        SUBOPTIMAL-NOTE)

  - Possible values for INLINE are T, NIL and :MAYBE

  - STATIC-DISPATCH-NAME could be useful for tracing or profiling

  - If INVALIDATE-PF is non-NIL then the associated polymorphic-function
    is forced to recompute its dispatching after this polymorph is defined.

  - SUBOPTIMAL-NOTE and MORE-OPTIMAL-TYPE-LIST are useful for signalling that the
    POLYMORPH chosen for static-dispatch, inlining, or compiler-macro is
    not the most optimal.
    It is recommended that SUBOPTIMAL-NOTE should be the name of a subclass of
    SUBOPTIMAL-POLYMORPH-NOTE - the condition class should have a slot to
    accept the TYPE-LIST of the currently chosen POLYMORPH

  **Note**:
  - INLINE T or :MAYBE can result in infinite expansions for recursive polymorphs.
Proceed at your own risk.
  - Also, because inlining results in type declaration upgradation for purposes
of subtype polymorphism, it is recommended to not mutate the variables used
in the lambda list; the consequences of mutation are undefined.
")

(defun expand-defpolymorph-full
    (whole name typed-lambda-list return-type body env)
  (destructuring-bind
      (name
       &key (inline t ip)
         (static-dispatch-name nil static-dispatch-name-p)
         invalidate-pf
         more-optimal-type-list
         suboptimal-note)
      (if (typep name 'function-name)
          (list name)
          name)
    (declare (type function-name name)
             (optimize debug))
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
           ;; INLINE-LAMBDA-BODY performs no such run time checks.
           ;;   It is used for generating DEFTRANSFORM as well as
           ;;   by PF-COMPILER-MACRO to generate minimal overhead code.
           (inline-lambda-body
            (when inline
              `(lambda ,param-list
                 (declare (ignorable ,@ignorable-list))
                 ,lambda-declarations
                 ,declarations
                 ;; The RETURN-TYPE here would be augmented by
                 ;; PF-COMPILER-MACRO
                 (block ,block-name
                   (locally ,@body)))))
           #+sbcl
           (sbcl-deftransform-form
            (when inline
              (make-and-wrap-sbcl-deftransform-form
               env name typed-lambda-list inline-lambda-body parameters)))
           (ftype-proclaimation
            (ftype-proclaimation
             static-dispatch-name effective-type-list return-type env)))

      (with-safe-inline (inline-safe-lambda-body inline-notes
                         :whole whole :name name :inlinep ip :inline inline
                         :inline-lambda-body inline-lambda-body)

        (setq inline (case inline
                       ((t) (cond (ip inline)
                                  (inline-notes nil)
                                  (t t)))
                       (otherwise inline)))

        ;; NOTE: We need the LAMBDA-BODY due to compiler macros,
        ;; and "objects of type FUNCTION can't be dumped into fasl files"
        `(progn

           (eval-when (:compile-toplevel :load-toplevel :execute)
             (unless (and (fboundp ',name)
                          (typep (function ,name) 'polymorphic-function))
               (define-polymorphic-function ,name ,untyped-lambda-list)))

           #+sbcl ,sbcl-deftransform-form

           (eval-when (:load-toplevel :execute)

             ,(when inline-notes
                ;; Even STYLE-WARNING isn't appropriate to this, because we want to
                ;; inform the user of the warnings even when INLINE option is supplied.
                `(compiler-macro-notes:with-notes
                     (',whole nil :unwind-on-signal nil)
                   (signal 'defpolymorph-note :datum ,inline-notes)
                   t))

             ;; We have implemented inlining through the PF-COMPILER-MACRO.
             ;; In addition to inlining, it also propagates the type declarations
             ;; so that further compiler/macroexpansions can make use of this info.

             ;; This type declaration is not handled by the implementation
             ;; provided inlining of DEFUN-defined functions.

             ,ftype-proclaimation
             ,(if inline-notes
                  ;; If there are issues related to INLINING,
                  ;; suppress the other warnings and let the user
                  ;; deal with them first.
                  `(with-muffled-compilation-warnings
                     (setf (fdefinition ',static-dispatch-name) ,lambda-body))
                  `(setf (fdefinition ',static-dispatch-name) ,lambda-body)))

           (eval-when (:compile-toplevel :load-toplevel :execute)
             (register-polymorph ',name ',inline
                                 ',doc
                                 ',typed-lambda-list
                                 ',type-list
                                 ',effective-type-list
                                 ',more-optimal-type-list
                                 ',suboptimal-note
                                 ',return-type
                                 ',inline-safe-lambda-body
                                 ',static-dispatch-name
                                 ',lambda-list-type
                                 ',(run-time-applicable-p-form parameters)
                                 ,(compiler-applicable-p-lambda-body parameters)
                                 #+sbcl (sb-c:source-location))
             ,(when invalidate-pf
                `(invalidate-polymorphic-function-lambda (fdefinition ',name)))
             ',name))))))

(setf (cl:documentation 'undefine-polymorphic-function 'cl:function)
      "Remove the POLYMORPH(-WRAPPER) defined by DEFINE-POLYMORPH
CL:FMAKUNBOUND will be insufficient, because polymorphic-functions
also have a compiler macro defined for them. Additionally, on SBCL,
they may also have transforms associated with them.")
