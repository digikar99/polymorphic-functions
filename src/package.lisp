(defpackage #:polymorphic-functions.nonuser
  (:use)
  (:documentation
   "Package for internal use by POLYMORPHIC-FUNCTIONS not intended for direct use by users."))

(defpackage #:polymorphic-functions
  (:use #:alexandria #:cl #:compiler-macro-notes)
  (:shadow #:named-lambda
           #:list-named-lambda
           #:find-class)
  (:import-from #:5am #:is #:def-test)
  (:import-from #:let-plus #:let+ #:&values)
  (:import-from #:introspect-environment
                #:compiler-macroexpand
                #:constant-form-value
                #:parse-compiler-macro
                #:typexpand)
  (:import-from #:cl-environments.cltl2
                #:function-information
                #:variable-information
                #:declaration-information
                #:define-declaration
                #:augment-environment)
  (:import-from #:trivial-types
                #:function-name)
  (:import-from #:cl-form-types
                #:combine-values-types)
  (:export #:define-polymorphic-function
           #:undefine-polymorphic-function
           #:defpolymorph
           #:defpolymorph-compiler-macro
           #:undefpolymorph
           #:find-polymorph
           #:polymorph-apropos-list-type

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
           #:*disable-static-dispatch*

           #:%deparameterize-type

           #:suboptimal-polymorph-note
           #:more-optimal-polymorph-inapplicable

           #:specializing
           #:specializing-type-of))

(5am:def-suite :polymorphic-functions)
