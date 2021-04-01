(in-package adhoc-polymorphic-functions)

;;; As per the discussion at https://github.com/Bike/introspect-environment/issues/4
;;; the FREE-VARIABLES-P cannot be substituted by a simple CLOSUREP (sb-kernel:closurep)
;;; TODO: Find the limitations of HU.DWIM.WALKER (one known is MACROLET)
;;; See the discussion at
;;; https://www.reddit.com/r/lisp/comments/itf0gv/determining_freevariables_in_a_form/
;;; (iterate::free-variables form) has been tried out proves out to be pretty restrictive
(defun free-variables-p (form)
  (let (free-variables)
    (handler-bind ((style-warning #'muffle-warning))
      (with-output-to-string (*error-output*)
        (setq free-variables
              (remove-if-not (lambda (elt)
                               (typep elt 'hu.dwim.walker:free-variable-reference-form))
                             (hu.dwim.walker:collect-variable-references
                              (hu.dwim.walker:walk-form
                               form))))))
    (mapcar (lambda (free-var-reference-form)
              (slot-value free-var-reference-form 'hu.dwim.walker::name))
            free-variables)))

(defun recursive-function-p (name body)
  (when body
    (cond ((listp body)
           (if (eq name (car body))
               t
               (some (curry 'recursive-function-p name) (cdr body))))
          (t nil))))

;;; - run-time correctness requires
;;;   - DEFINE-POLYMORPH-FUNCTION -> DEFUN
;;;   - DEFPOLYMORPH
;;; - compile-time correctness requires
;;;   - DEFINE-POLYMORPH-FUNCTION -> DEFINE-COMPILER-MACRO
;;;   - GET-TYPE-LIST
;;;   - DEFPOLYMORPH-COMPILER-MACRO

(defmacro with-compiler-note (&body body)
  `(handler-case (progn ,@body)
     (no-applicable-polymorph (condition)
       (format *error-output*
               "~%; While compiling ~S: ~&; ~A"
               form
               (str-join (uiop:strcat #\newline ";  ")
                         (str-split #\newline (format nil "~A" condition))))
       original-form)
     (form-type-failure (condition)
       (when optim-speed
         (format *error-output*
                 (uiop:strcat "~%; Optimization of ~&;  "
                              (str-join (uiop:strcat #\newline ";  ")
                                        (str-split #\newline (format nil " ~S"
                                                                     original-form)))
                              "~%; is left to ~A because ADHOC-POLYMORPHIC-FUNCTIONS "
                              "is unable to optimize it ~%; because~&; ~A~&; ~&")
                 (lisp-implementation-type)
                 (str-join (uiop:strcat #\newline ";  ")
                           (str-split #\newline (format nil "~A" condition)))))
       (cond (optim-debug original-form)
             ((and optim-speed
                   (member :sbcl *features*)
                   original-form))
             ((or optim-speed optim-slight-speed) block-form)
             (t (error "Non-exhaustive cases in WITH-COMPILER-NOTE!"))))
     ((or polymorph-body-has-free-variables
       recursive-expansion-is-possibly-infinite) (condition)
       (when optim-speed
         (format *error-output*
                 (uiop:strcat "~&; "
                              (format nil "Unable to optimize ~S because:" original-form)
                              "~&;  ~A")
                 (str-join (uiop:strcat #\newline ";  ")
                           (str-split #\newline (format nil "~A" condition)))))
       ;; FIXME: Will SBCL optimize these cases - should these conditions be merged
       ;; into the previous FORM-TYPE-FAILURE case?
       (cond (optim-debug original-form)
             ((and optim-speed
                   (member :sbcl *features*)
                   original-form))
             ((or optim-speed optim-slight-speed) block-form)
             (t (error "Non-exhaustive cases in WITH-COMPILER-NOTE!"))))))

(defmacro define-polymorphic-function (name untyped-lambda-list &key overwrite &environment env)
  "Define a function named NAME that can then be used for DEFPOLYMORPH
for specializing on various argument types.

If OVERWRITE is T, all the existing polymorphs associated with NAME are deleted,
and new polymorphs will be ready to be installed.
If OVERWRITE is NIL, a continuable error is raised if the LAMBDA-LIST has changed."
  (declare (type function-name       name)
           (type untyped-lambda-list untyped-lambda-list))
  ;; TODO: Handle the case of redefinition
  (let ((*name*        name)
        (*environment*  env))
    `(,@(if optim-debug
            `(progn)
            `(handler-bind ((style-warning #'muffle-warning))))
      (eval-when (:compile-toplevel :load-toplevel :execute)
        ,(when overwrite
           `(undefine-polymorphic-function ',name))
        (register-polymorphic-function ',name ',untyped-lambda-list))
      #+sbcl (sb-c:defknown ,name * * nil :overwrite-fndb-silently t)
      (define-compiler-macro ,name (&whole form &rest args &environment env)
        (declare (ignore args))
        ;; FIXME: Is the assumption that FORM either starts with ',NAME or FUNCALL correct?
        (let ((*environment*                 env)
              (*compiler-macro-expanding-p*    t)
              (original-form                form)
              (block-form                    nil))
          (with-compiler-note
            (when (eq 'funcall (car form))
              (setf form (cons (second (second form))
                               (cddr form))))
            (setq block-form
                  (let* ((name       (first form))
                         (gensyms    (make-gensym-list (length (rest form))))
                         (block-name (if (and (listp name)
                                              (eq 'setf (first name)))
                                         (second name)
                                         name)))
                    ;; This block is necessary for optimization of &optional and &key
                    ;; and &rest args; otherwise, we will need to cons at runtime!
                    `(block ,block-name
                       (let (,@(loop :for sym :in gensyms
                                     :for form :in (rest form)
                                     :collect `(,sym ,form)))
                         (funcall (the function
                                       (polymorph-lambda
                                        ,(retrieve-polymorph-form
                                          name
                                          ',(lambda-list-type untyped-lambda-list)
                                          gensyms)))
                                  ,@gensyms)))))
            (let* ((arg-list  (rest form))
                   (polymorph (apply #'retrieve-polymorph ',name arg-list)))
              (with-slots (inline-lambda-body return-type type-list
                           compiler-macro-lambda recursively-safe-p free-variables)
                  polymorph
                (cond (compiler-macro-lambda
                          (funcall compiler-macro-lambda
                                   (cons inline-lambda-body (rest form))
                                   env))
                      (optim-speed
                       ;; TODO: Use some other declaration for inlining as well
                       ;; Optimized for speed and type information available
                       (if free-variables
                           (signal 'polymorph-body-has-free-variables
                                   :name ',name :type-list type-list))
                       (if (not recursively-safe-p)
                           (signal 'recursive-expansion-is-possibly-infinite
                                   :form form)                           )
                       `(the ,return-type (,inline-lambda-body ,@(cdr form))))
                      (optim-debug        original-form)
                      (optim-slight-speed block-form)
                      (t (error "Non-exhaustive cases!")))))))))))

(defun extract-declarations (body)
  "Returns two values: DECLARATIONS and remaining BODY"
  (cond ((null body)
         (values `(declare) nil))
        ((and (listp (car body))
              (eq 'declare (caar body)))
         (multiple-value-bind (declarations rest-body) (extract-declarations (rest body))
           (values (nconc (cons 'declare (cdar body))
                          (rest declarations))
                   rest-body)))
        (t
         (values `(declare) body))))

(defun ensure-unambiguous-call (name type-list)
  (loop :for list :in (polymorphic-function-type-lists
                       (fdefinition
                        name))
        :do (when (and (type-list-causes-ambiguous-call-p type-list list)
                       (not (equalp type-list list)))
              (error "The given TYPE-LIST ~%  ~S~%will cause ambiguous call with the type list ~%  ~S~% of an existing polymorph"
                     type-list
                     list)
              (return))))

(defun ensure-non-intersecting-type-lists (name type-list)
  (loop :for list :in (polymorphic-function-type-lists
                       (fdefinition
                        name))
        :do (when (and (type-list-intersect-p type-list list)
                       (not (equalp type-list list)))
              (error "The given TYPE-LIST ~%  ~S~%intersects with the type list ~%  ~S~% of an existing polymorph"
                     type-list
                     list)
              (return))))

;;; Do minimal work at macro-expansion time?
;;; 1. Well, to be able to handle closures, the compilation phase of the lambda
;;;    needs the env. However, env objects cannot be dumped; nor does it seem like
;;;    a wise idea to do so.
;;; 2. That means, the minimum work that we need to do during macroexpansion time
;;;    involves the emission of the lambda-body.

(defmacro defpolymorph (name typed-lambda-list return-type &body body &environment env)
  "  Expects OPTIONAL or KEY args to be in the form ((A TYPE) DEFAULT-VALUE) or ((A TYPE) DEFAULT-VALUE AP).
  NAME could also be (NAME &KEY RECURSIVELY-SAFE-P)"
  (declare (type typed-lambda-list typed-lambda-list))
  (destructuring-bind (name &key (recursively-safe-p t rp)) (if (typep name 'function-name)
                                                                (list name)
                                                                name)
    (declare (type function-name name))
    (let* ((block-name       (if (and (listp name)
                                      (eq 'setf (first name)))
                                 (second name)
                                 name))
           (*environment*    env)
           (lambda-list-type (lambda-list-type typed-lambda-list :typed t)))
      (multiple-value-bind (param-list type-list)
          (defun-lambda-list typed-lambda-list :typed t)
        (multiple-value-bind (declarations body) (extract-declarations body)
          (let* (;; no declarations in FREE-VARIABLE-ANALYSIS-FORM
                 (free-variable-analysis-form `(lambda ,param-list (block ,block-name ,@body)))
                 (form                        `(defpolymorph ,name ,typed-lambda-list ,@body))
                 (lambda-body #+sbcl
                              `(sb-int:named-lambda (polymorph ,name ,type-list) ,param-list
                                 ,(lambda-declarations typed-lambda-list :typed t)
                                 ,declarations
                                 (block ,block-name
                                   ,@(butlast body)
                                   (the ,return-type ,@(or (last body) '(nil)))))
                              #+ccl
                              `(ccl:nfunction (polymorph ,name ,type-list)
                                              (lambda ,param-list
                                                ,(lambda-declarations typed-lambda-list :typed t)
                                                ,declarations
                                                (block ,block-name
                                                  ,@(butlast body)
                                                  (the ,return-type ,@(or (last body) '(nil))))))
                              #-(or ccl sbcl)
                              `(lambda ,param-list
                                 ,(lambda-declarations typed-lambda-list :typed t)
                                 ,declarations
                                 (block ,block-name
                                   ,@(butlast body)
                                   (the ,return-type ,@(or (last body) '(nil))))))
                 (inline-lambda-body #-(or ccl sbcl) lambda-body
                                     #+ccl (nth 2 lambda-body)
                                     #+sbcl `(lambda ,@(nthcdr 2 lambda-body)))
                 ;; TODO: should not contain declarations
                 (free-variables (free-variables-p free-variable-analysis-form))
                 (recursively-safe-p (if rp
                                         recursively-safe-p
                                         (not (recursive-function-p name inline-lambda-body))))
                 ;; TODO: Update SBCL for most-specialized-dispatch
                 ;; #+sbcl (sbcl-transform nil)
                 #+sbcl
                 (node (gensym "NODE"))
                 #+sbcl
                 (sbcl-transform `(sb-c:deftransform ,name
                                      (,param-list ,(if (eq '&rest (lastcar type-list))
                                                        (butlast type-list)
                                                        type-list) *
                                       :policy (< debug speed)
                                       :node ,node)
                                    ;; FIXME: This leads to an O(n^2) complexity
                                    ;; Could sorting help?
                                    (if (most-specialized-applicable-transform-p
                                         ',name ,node ',type-list)
                                        `(apply ,',lambda-body
                                                ,@',(sbcl-transform-body-args typed-lambda-list
                                                                              :typed t))
                                        (sb-c::give-up-ir1-transform)))))
            (multiple-value-bind (inline-safe-lambda-body note)
                (cond (free-variables
                       (values nil
                               (with-output-to-string (*error-output*)
                                 (note-no-inline form "free variables ~S were found"
                                                 free-variables))))
                      ((not recursively-safe-p)
                       (values nil
                               (with-output-to-string (*error-output*)
                                 (note-no-inline form "it is suspected to result in infinite recursive expansion;~%  supply :RECURSIVELY-SAFE-P T option to override"))))
                      (t
                       (values inline-lambda-body nil)))
              ;; NOTE: We need the LAMBDA-BODY due to compiler macros,
              ;; and "objects of type FUNCTION can't be dumped into fasl files
              `(progn
                 #+sbcl ,(when (and (not free-variables) recursively-safe-p)
                           (if optim-debug
                               sbcl-transform
                               `(locally (declare (sb-ext:muffle-conditions style-warning))
                                  (handler-bind ((style-warning #'muffle-warning))
                                    ,sbcl-transform))))
                 ,(when note `(write-string ,note *error-output*))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (register-polymorph ',name ',type-list ',return-type
                                       ',inline-safe-lambda-body
                                       ,lambda-body
                                       ',lambda-list-type
                                       ,recursively-safe-p
                                       ',free-variables)
                   ',name)))))))))

(defmacro defpolymorph-compiler-macro (name type-list compiler-macro-lambda-list
                                       &body body)
  "An example of a type-list for a function with optional args would be (STRING &OPTIONAL INTEGER)"
  (declare (type function-name name)
           (type type-list type-list))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-polymorph-compiler-macro
        ',name ',type-list
        (compile nil (parse-compiler-macro nil
                                           ',compiler-macro-lambda-list
                                           ',body)))
       ',name))

(defun undefpolymorph (name type-list)
  "Remove the POLYMORPH associated with NAME with TYPE-LIST"
  #+sbcl
  (let ((info  (sb-c::fun-info-or-lose name))
        (ctype (sb-c::specifier-type (list 'function type-list '*))))
    (setf (sb-c::fun-info-transforms info)
          (remove-if (curry #'sb-c::type= ctype)
                     (sb-c::fun-info-transforms info)
                     :key #'sb-c::transform-type)))
  (remove-polymorph name type-list))

(defun undefine-polymorphic-function (name)
  "Remove the POLYMORPH(-WRAPPER) defined by DEFINE-POLYMORPH"
  (fmakunbound name)
  (setf (compiler-macro-function name) nil))
