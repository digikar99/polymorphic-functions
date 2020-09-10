(defpackage :typed-dispatch/tests
  (:use :cl :fiveam :typed-dispatch))

(in-package :typed-dispatch/tests)

(def-suite :typed-dispatch)
(in-suite :typed-dispatch)

(defmacro catch-condition (&body body)
  `(handler-case (progn
                   ,@body)
     (condition (condition) condition)))

(progn
  (define-typed-function my= (a b))
  (defun-typed my= ((a string) (b string))
    (string= a b))
  (defun-typed my= ((a number) (b number))
    (= a b))
  (define-compiler-macro-typed my= (number number) (&whole form a b)
    (declare (ignore b))
    (if (= 0 a)
        ''zero
        form))
  (defun foo ()
    (declare (optimize speed))
    (my= 0 5)))

(def-test required-args-correctness ()
  (let ((obj1 "hello")
        (obj2 "world")
        (obj3 "hello")
        (obj4 5)
        (obj5 5.0))
    (is (eq t   (my= obj1 obj3)))
    (is (eq nil (my= obj1 obj2)))
    (is (eq t   (my= obj4 obj5)))
    (is (typep (catch-condition (my= obj1 obj4))
               'error))
    (is (eq 'zero (foo)))))

(progn
  (define-typed-function bar (a &optional b c))
  (defun-typed bar ((a string) &optional ((b integer) 5) ((c integer) 7))
    (list a b c))
  (define-compiler-macro-typed bar (string &optional integer integer) (&whole form &rest args)
    (declare (ignore args))
    `(list ,form)) ; This usage of FORM also tests infinite recursion
  (defun foobar ()
    (declare (optimize speed))
    (bar "hello" 9)))

(def-test optional-args-correctness ()
  (is (equalp (bar "hello")
              '("hello" 5 7)))
  (is (equalp (bar "hello" 6)
              '("hello" 6 7)))
  (is (equalp (bar "hello" 6 9)
              '("hello" 6 9)))
  (is (equalp (foobar)
              '(("hello" 9 7)))))

