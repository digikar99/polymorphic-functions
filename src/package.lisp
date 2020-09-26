
(uiop:define-package :typed-dispatch
  (:mix :trivial-types :cl :alexandria :introspect-environment)
  (:import-from :5am :is :def-test)
  (:export :define-typed-function
   :defun-typed
           :define-compiler-macro-typed))

(in-package :typed-dispatch)

(5am:def-suite :typed-dispatch)

(defmacro catch-condition (form)
  `(handler-case ,form
     (condition (condition) condition)))

(defmacro is-error (form)
  `(5am:signals error ,form))


