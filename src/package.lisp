
(defpackage :polymorphic-functions.extended-types
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

(defpackage :polymorphic-functions
  (:use :polymorphic-functions.extended-types :cl-form-types
        :alexandria :introspect-environment :cl)
  (:import-from :5am #:is #:def-test)
  (:import-from :ctype
                #:ctype
                #:cons-specifier-ctype)
  (:import-from :polymorphic-functions.extended-types
                #:*extended-type-specifiers*
                #:upgraded-extended-type)
  (:import-from :trivial-types
                #:function-name
                #:type-specifier)
  ;; UIOP:DEFINE-PACKAGE doesn't work as correctly on CCL
  ;; So, keep the below in manual sync with above extended things
  (:shadowing-import-from :polymorphic-functions.extended-types
                          #:extended-type-specifier-p
                          #:type-specifier-p
                          #:upgrade-extended-type
                          #:supertypep
                          #:subtypep
                          #:typep
                          #:type=)
  (:export #:define-polymorphic-function
           #:undefine-polymorphic-function
           #:defpolymorph
           #:defpolymorph-compiler-macro
           #:undefpolymorph
           #:find-polymorph

           ;; Unstable API
           #:polymorphic-function
           #:polymorph
           #:type-like
           #:no-applicable-polymorph
           #:polymorphic-function-type-lists))

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

(trivial-package-local-nicknames:add-package-local-nickname :cltl2 :cl-environments.cltl2)
