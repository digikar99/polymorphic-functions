(in-package typed-functions)

;; As per the discussion at https://github.com/Bike/introspect-environment/issues/4
;; the FREE-VARIABLES-P cannot be substituted by a simple CLOSUREP (sb-kernel:closurep)
;; TODO: Find the limitations of HU.DWIM.WALKER (one known is MACROLET)
;; See the discussion at
;; https://www.reddit.com/r/lisp/comments/itf0gv/determining_freevariables_in_a_form/
(defun free-variables-p (form)
  (let (free-variables)
    (with-output-to-string (*error-output*)
      (setq free-variables
            (remove-if-not (lambda (elt)
                             (typep elt 'hu.dwim.walker:free-variable-reference-form))
                           (hu.dwim.walker:collect-variable-references
                            (hu.dwim.walker:walk-form
                             form)))))
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
;;;   - DEFINE-TYPED-FUNCTION -> DEFUN
;;;   - DEFUN-TYPED
;;; - compile-time correctness requires
;;;   - DEFINE-TYPED-FUNCTION -> DEFINE-COMPILER-MACRO
;;;   - GET-TYPE-LIST
;;;   - DEFINE-COMPILER-MACRO-TYPED

(defvar *compiler-macro-expanding-p* nil
  "Bound to T inside the DEFINE-COMPILER-MACRO defined in DEFINE-TYPED-FUNCTION")
(defvar *environment*)
(setf (documentation '*environment* 'variable)
      "Bound inside the DEFINE-COMPILER-MACRO defined in DEFINE-TYPED-FUNCTION for
use by functions like TYPE-LIST-APPLICABLE-P")

(defmacro with-compiler-note (form &body body)
  `(handler-case (progn ,@body)
     (condition (condition)
       (format *error-output*
               (cond ((< 1 (policy-quality 'speed env))
                      (uiop:strcat "~&; "
                                   (format nil "Unable to optimize ~S because:" ,form)
                                   "~A"))
                     ((eq 'apply (car form))
                      "~&; ~A")
                     ((= 3 (policy-quality 'debug env))
                      (uiop:strcat "~%; "
                                   (format nil "While compiling ~S:" form)
                                   "~&;  ~A"))
                     (t ""))
               (str:join (uiop:strcat #\newline ";  ")
                         (str:split #\newline (format nil "~A" condition))))
       ,form)))

(defmacro define-typed-function (name untyped-lambda-list &environment env)
  "Define a function named NAME that can then be used for DEFUN-TYPED for specializing on ORDINARY and OPTIONAL argument types."
  (declare (type function-name       name)
           (type untyped-lambda-list untyped-lambda-list))
  ;; TODO: Handle the case of redefinition
  (let ((*name*        name)
        (*environment*  env))
    (register-typed-function-wrapper name untyped-lambda-list)
    (multiple-value-bind (body-form lambda-list) (defun-body untyped-lambda-list)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (register-typed-function-wrapper ',name ',untyped-lambda-list))
         (defun ,name ,lambda-list
           ,body-form)
         (define-compiler-macro ,name (&whole form &rest args &environment env)
           (declare (ignore args))
           (let ((*environment*                 env)
                 (*compiler-macro-expanding-p*    t)
                 (original-form                form))
             (with-compiler-note original-form
               (when (eq 'funcall (car form))
                 (setf form (cons (second (cadr form))
                                  (cddr form))))
               (if (eq ',name (car form))
                   (let ((arg-list (rest form)))
                     (multiple-value-bind (body function dispatch-type-list)
                         (apply 'retrieve-typed-function ',name arg-list)
                       (declare (ignore function))
                       (if (< 1 (policy-quality 'speed env))
                           (progn

                             (unless body
                               ;; TODO: Here the reason concerning free-variables is hardcoded
                               (signal "~%~S with ARG-LIST ~S cannot be inlined due to free-variables" ',name dispatch-type-list))
                             (if-let ((compiler-function (apply
                                                          'retrieve-typed-function-compiler-macro
                                                          ',name arg-list)))
                               (funcall compiler-function
                                        (cons body (rest form))
                                        env)
                               ;; TODO: Use some other declaration for inlining as well
                               ;; Optimized for speed and type information available
                               (if (recursive-function-p ',name body)
                                   (signal "~%Inlining ~S results in (potentially infinite) recursive expansion"
                                           form)
                                   `(,body ,@(cdr form)))))
                           original-form)))
                   (signal "COMPILER-MACRO of ~S can only optimize raw function calls." ',name)))))))))


(defmacro defun-typed (name typed-lambda-list return-type &body body)
  "  Expects OPTIONAL or KEY args to be in the form ((A TYPE) DEFAULT-VALUE) or ((A TYPE) DEFAULT-VALUE AP)."
  (declare (type function-name name)
           (type typed-lambda-list typed-lambda-list))
  (let* ((*name*               name)
         (untyped-lambda-list (typed-function-wrapper-lambda-list
                                (retrieve-typed-function-wrapper
                                 name))))
    (multiple-value-bind (param-list type-list)
        (defun-lambda-list typed-lambda-list :typed t)
      (assert (type-list-compatible-p type-list untyped-lambda-list)
              nil
              "TYPE-LIST ~S is not compatible with the LAMBDA-LIST ~S of the TYPED-FUNCTIONs associated with ~S"
              type-list untyped-lambda-list name)
      (let (;; no declarations in FREE-VARIABLE-ANALYSIS-FORM
            (free-variable-analysis-form `(lambda ,param-list ,@body))
            (form                        `(defun-typed ,name ,typed-lambda-list ,@body)))
        (let* ((lambda-body `(lambda ,param-list
                               ,(lambda-declarations typed-lambda-list :typed t)
                               ,@(butlast body)
                               (the ,return-type ,@(last body)))))
          ;; NOTE: We need the LAMBDA-BODY due to compiler macros,
          ;; and "objects of type FUNCTION can't be dumped into fasl files
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (register-typed-function ',name ',type-list
                                      ',(if-let (free-variables
                                                 (free-variables-p
                                                  ;; TODO: should not contain declarations
                                                  free-variable-analysis-form))
                                          (progn
                                            (terpri *error-output*)
                                            (format *error-output* "; Will not inline ~%;   ")
                                            (write-string
                                             (str:replace-all
                                              (string #\newline)
                                              (uiop:strcat #\newline #\; "  ")
                                              (format nil "~S~%" form))
                                             *error-output*)
                                            (format *error-output*
                                                    "because free variables ~S were found"
                                                    free-variables)
                                            nil)
                                          lambda-body)
                                      ,lambda-body)
             ',name))))))

(defmacro define-compiler-macro-typed (name type-list compiler-macro-lambda-list
                                       &body body)
  "An example of a type-list for a function with optional args would be (STRING &OPTIONAL INTEGER)"
  (declare (type function-name name)
           (type type-list type-list))
  ;; TODO: Handle the case when NAME is not bound to a TYPED-FUNCTION
  (let ((lambda-list (typed-function-wrapper-lambda-list
                      (retrieve-typed-function-wrapper
                       name))))
    (assert (type-list-compatible-p type-list lambda-list)
            nil
            "TYPE-LIST ~S is not compatible with the LAMBDA-LIST ~S of the TYPED-FUNCTIONs associated with ~S"
            type-list lambda-list name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-typed-function-compiler-macro
        ',name ',type-list
        (compile nil (parse-compiler-macro nil
                                           ',compiler-macro-lambda-list
                                           ',body)))
       ',name)))

(defun fmakunbound-typed (name type-list)
  "Remove the TYPED-FUNCTION associated with NAME with TYPE-LIST"
  (remove-typed-function name type-list))

(defun undefine-typed-function (name)
  "Remove the TYPED-FUNCTION(-WRAPPER) defined by DEFINE-TYPED-FUNCTION"
  (remhash name *typed-function-table*)
  (fmakunbound name))
