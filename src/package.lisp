
(uiop:define-package :adhoc-polymorphic-functions
  (:mix :trivial-types :cl :alexandria :introspect-environment)
  (:import-from :5am :is :def-test)
  (:export :define-polymorphic-function
           :undefine-polymorphic-function
           :defpolymorph
           :defpolymorph-compiler-macro
           :undefpolymorph
           :find-polymorph))

(in-package :adhoc-polymorphic-functions)

(5am:def-suite :adhoc-polymorphic-functions)

(defmacro catch-condition (form)
  `(handler-case ,form
     (condition (condition) condition)))

(defmacro is-error (form)
  `(5am:signals error ,form))

(define-symbol-macro optim-debug (or (= 3 (policy-quality 'debug env))
                                     (> (policy-quality 'debug env)
                                        (policy-quality 'speed env))))
(define-symbol-macro optim-speed (and (/= 3 (policy-quality 'debug env))
                                      (= 3 (policy-quality 'speed env))))
(define-symbol-macro optim-slight-speed (and (/= 3 (policy-quality 'debug env))
                                             (/= 3 (policy-quality 'speed env))
                                             (<= (policy-quality 'debug env)
                                                 (policy-quality 'speed env))))

(defun str-split (separator string)
  (split-sequence:split-sequence separator string))

(defun str-join (separator strings)
  (format nil (uiop:strcat "~{~A~^" separator "~}") strings))

(defun str-replace-all (old new s)
  (str-join new (str-split old s)))
