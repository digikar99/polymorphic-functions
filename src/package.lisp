
(polymorphic-functions.defpackage:defpackage :polymorphic-functions.extended-types
  (:use :cl :ctype)
  (:import-from :ctype
                #:cons-specifier-ctype
                #:ctype)
  (:intern #:*extended-type-specifiers*
           #:upgraded-extended-type)
  (:shadow #:ctype=
           #:extended-type-specifier-p
           #:type-specifier-p
           #:supertypep
           #:subtypep
           #:typep
           #:type=)
  (:export #:extended-type-specifier-p
           #:type-specifier-p
           #:upgrade-extended-type
           #:supertypep
           #:subtypep
           #:typep
           #:type=))

(defpackage #:polymorphic-functions.nonuser
  (:use)
  (:documentation
   "Package for internal use by POLYMORPHIC-FUNCTIONS not intended for direct use by users."))

(polymorphic-functions.defpackage:defpackage :polymorphic-functions
  (:shadowing-import-exported-symbols :polymorphic-functions.extended-types)
  (:use :cl-form-types :alexandria :cl)
  (:import-from :5am #:is #:def-test)
  (:import-from :ctype
                #:typexpand
                #:ctype
                #:cons-specifier-ctype)
  (:import-from :polymorphic-functions.extended-types
                #:*extended-type-specifiers*
                #:upgraded-extended-type)
  (:import-from :introspect-environment
                #:compiler-macroexpand
                #:constant-form-value
                #:parse-compiler-macro)
  (:import-from :cl-environments
                #:function-information
                #:variable-information
                #:declaration-information
                #:define-declaration)
  (:export #:define-polymorphic-function
           #:undefine-polymorphic-function
           #:defpolymorph
           #:defpolymorph-compiler-macro
           #:undefpolymorph
           #:find-polymorph

           ;; Unstable API
           #:polymorphic-function
           #:polymorph
           #:no-applicable-polymorph
           #:polymorphic-function-type-lists
           #:inline-pf
           #:notinline-pf
           #:pf-defined-before-use
           #:not-pf-defined-before-use
           #:*compiler-macro-expanding-p*
           #:polymorph-apropos-list-type

           #:*parametric-type-symbol-predicates*
           #:parametric-type-run-time-lambda-body
           #:parametric-type-compile-time-lambda-body))

(in-package :polymorphic-functions)

(5am:def-suite :polymorphic-functions)

(defmacro catch-condition (form)
  `(handler-case ,form
     (condition (condition) condition)))

(defmacro is-error (form)
  `(5am:signals error ,form))

(defmacro list-named-lambda (name package lambda-list &body body)
  (declare (type list name))
  #+sbcl
  `(sb-int:named-lambda ,name ,lambda-list
     ,@body)
  #+ccl
  `(ccl:nfunction ,name
                  (lambda ,lambda-list
                    ,@body))
  #-(or sbcl ccl)
  (let ((function-name (intern (write-to-string name) package)))
    `(flet ((,function-name ,lambda-list ,@body))
       #',function-name)))

(define-symbol-macro optim-safety (= 3 (policy-quality 'safety env)))

(define-symbol-macro optim-debug (or (= 3 (policy-quality 'debug env))
                                     (> (policy-quality 'debug env)
                                        (policy-quality 'speed env))))
(define-symbol-macro optim-speed (and (/= 3 (policy-quality 'debug env))
                                      (= 3 (policy-quality 'speed env))))
(define-symbol-macro optim-slight-speed (and (/= 3 (policy-quality 'debug env))
                                             (/= 3 (policy-quality 'speed env))
                                             (<= (policy-quality 'debug env)
                                                 (policy-quality 'speed env))))

(defun policy-quality (quality &optional env)
  (second (assoc quality (declaration-information 'optimize env))))

(defun macroexpand-all (form &optional env)
  (cl-form-types.walker:walk-form (lambda (form env)
                                    (declare (ignore env))
                                    form)
                                  form
                                  env))

(defun type-specifier-p (type-specifier)
  "Returns true if TYPE-SPECIFIER is a valid type specfiier."
  (or (when (symbolp type-specifier)
        (documentation type-specifier 'type))
      (block nil
        #+sbcl (return (sb-ext:valid-type-specifier-p type-specifier))
        #+openmcl (return (ccl:type-specifier-p type-specifier))
        #+ecl (return (c::valid-type-specifier type-specifier))
        #+clisp (return (null
                         (nth-value 1 (ignore-errors
                                       (ext:type-expand type-specifier)))))
        (error "TYPE-SPECIFIER-P not available for this implementation"))))

(defun setf-function-name-p (object)
  (and (listp object)
       (null (cddr object))
       (eq 'setf (car object))
       (symbolp (cadr object))))

(deftype function-name ()
  ;; Doesn't work great with subtypep
  "Ref: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#function_name"
  `(or symbol (satisfies setf-function-name-p)))
