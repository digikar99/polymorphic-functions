
(uiop:define-package :typed-dispatch
  (:mix :trivial-types :cl :alexandria :introspect-environment)
  (:import-from :5am :is :def-test)
  (:export :define-typed-function           
           :undefine-typed-function
           :defun-typed
           :define-compiler-macro-typed
           :fmakunbound-typed))

(in-package :typed-dispatch)
(trivial-package-local-nicknames:add-package-local-nickname :cm :sandalphon.compiler-macro)

(5am:def-suite :typed-dispatch)

(defmacro catch-condition (form)
  `(handler-case ,form
     (condition (condition) condition)))

(defmacro is-error (form)
  `(5am:signals error ,form))


