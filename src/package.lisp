
(uiop:define-package :adhoc-polymorphic-functions
  (:mix :trivial-types :cl :alexandria :introspect-environment)
  (:import-from :5am :is :def-test)
  (:export :define-polymorphic-function           
           :undefine-polymorphic-function
           :defpolymorph
           :defpolymorph-compiler-macro
           :undefpolymorph
           :find-polymorphs))

(in-package :adhoc-polymorphic-functions)

(5am:def-suite :adhoc-polymorphic-functions)

(defmacro catch-condition (form)
  `(handler-case ,form
     (condition (condition) condition)))

(defmacro is-error (form)
  `(5am:signals error ,form))


