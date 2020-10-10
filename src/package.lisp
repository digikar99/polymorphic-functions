
(uiop:define-package :typed-functions
  (:mix :trivial-types :cl :alexandria :introspect-environment)
  (:import-from :5am :is :def-test)
  (:export :define-typed-function           
           :undefine-typed-function
           :defun-typed
           :define-compiler-macro-typed
           :fmakunbound-typed))

(in-package :typed-functions)
(trivial-package-local-nicknames:add-package-local-nickname :cm :sandalphon.compiler-macro)

(5am:def-suite :typed-functions)

(defmacro catch-condition (form)
  `(handler-case ,form
     (condition (condition) condition)))

(defmacro is-error (form)
  `(5am:signals error ,form))


