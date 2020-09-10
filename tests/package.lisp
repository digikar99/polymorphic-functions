(defpackage :typed-dispatch/tests
  (:use :cl :fiveam :typed-dispatch))

(in-package :typed-dispatch/tests)

(def-suite :typed-dispatch)
(in-suite :typed-dispatch)

(defmacro catch-condition (&body body)
  `(handler-case (progn
                   ,@body)
     (condition (condition) condition)))

(def-test required-args-correctness ()
  (with-output-to-string (*error-output*)
    (with-output-to-string (*standard-output*)
      ;; enters infinite loop
      (eval '(define-typed-function my= (a b)))
      (eval `(defun-typed my= ((a string) (b string))
               (string= a b)))
      (eval `(defun-typed my= ((a number) (b number))
               (= a b)))))
  (let ((obj1 "hello")
        (obj2 "world")
        (obj3 "hello")
        (obj4 5)
        (obj5 5.0))
    (is (eq t   (my= obj1 obj3)))
    (is (eq nil (my= obj1 obj2)))
    (is (eq t   (my= obj4 obj5)))
    (is (typep (catch-condition (my= obj1 obj4))
               'error))))

(def-test optional-args-correctness ()
  (with-output-to-string (*error-output*)
    (with-output-to-string (*standard-output*)
      (eval '(define-typed-function bar (a &optional b c)))
      (eval `(defun-typed bar ((a string) &optional ((b integer) 5) ((c integer) 7))
               (list a b c)))))
  (is (equalp (bar "hello")
              '("hello" 5 7)))
  (is (equalp (bar "hello" 6)
              '("hello" 6 7)))
  (is (equalp (bar "hello" 6 9)
              '("hello" 6 9))))

