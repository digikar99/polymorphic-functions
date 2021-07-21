
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
                #-ecl #:typexpand
                #:ctype
                #:cons-specifier-ctype)
  (:import-from :polymorphic-functions.extended-types
                #:*extended-type-specifiers*
                #:upgraded-extended-type)
  (:import-from :trivial-types
                #:function-name
                #:type-specifier)
  (:import-from :introspect-environment
                #+ecl #:typexpand
                #:policy-quality
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
