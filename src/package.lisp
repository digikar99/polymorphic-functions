
(uiop:define-package :polymorphic-functions
  (:mix :cl-form-types :trivial-types :cl :alexandria :introspect-environment)
  (:import-from :5am #:is #:def-test)
  (:shadow #:subtypep
           #:typep)
  (:export #:define-polymorphic-function
           #:undefine-polymorphic-function
           #:defpolymorph
           #:defpolymorph-compiler-macro
           #:undefpolymorph
           #:find-polymorph

           ;; Unstable API
           #:polymorphic-function
           #:polymorph
           #:type-like))

(in-package :polymorphic-functions)

(5am:def-suite :polymorphic-functions)

(declaim (inline subtypep typep))

(defun subtypep (type1 type2 &optional environment)
  (if (and (type-specifier-p type1)
           (type-specifier-p type2))
      (cl:subtypep type1 type2 environment)
      (ctype:subctypep (ctype:specifier-ctype type1 environment)
                       (ctype:specifier-ctype type2 environment))))

(define-compiler-macro subtypep (&whole form type1 type2 &optional env-form &environment env)
  (if (and (constantp type1 env) (constantp type2 env))
      (if (and (type-specifier-p (constant-form-value type1 env))
               (type-specifier-p (constant-form-value type2 env)))
          `(cl:subtypep ,type1 ,type2 ,env-form)
          (once-only (env-form)
            `(ctype:subctypep (ctype:specifier-ctype ,type1 ,env-form)
                              (ctype:specifier-ctype ,type2 ,env-form))))
      form))

(defun typep (object type &optional environment)
  (if (type-specifier-p type)
      (cl:typep object type)
      (ctype:ctypep object (ctype:specifier-ctype type environment))))

(define-compiler-macro typep (&whole form object type &optional env-form &environment env)
  (if (constantp type env)
      (if (type-specifier-p (constant-form-value type env))
          `(cl:typep ,object ,type ,env-form)
          `(ctype:ctypep ,object (ctype:specifier-ctype ,type ,env-form)))
      form))

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
