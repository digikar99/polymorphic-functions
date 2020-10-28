(in-package :adhoc-polymorphic-functions)

(5am:in-suite :adhoc-polymorphic-functions)

(progn
  (define-polymorphic-function my= (a b) :override t)
  (defpolymorph my= ((a string) (b string)) boolean
    (return-from my= (string= a b)))
  (defpolymorph my= ((a number) (b number)) boolean
    (= a b))
  (defpolymorph-compiler-macro my= (number number) (&whole form a b)
    (declare (ignore b))
    (if (= 0 a)
        ''zero
        form))
  (defun my=-caller ()
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
    (is-error (my= obj1 obj4))
    (is (eq 'zero (my=-caller)))))

(progn ; This requires SBCL version 2.0.9+
  (define-polymorphic-function bar (a &optional b c) :override t)
  (defpolymorph bar ((str string) &optional ((b integer) 5) ((c integer) 7)) t
    (list str b c))
  (defpolymorph-compiler-macro bar (string &optional integer integer) (&whole form &rest args)
    (declare (ignore args))
    `(list ,form)) ; This usage of FORM also tests infinite recursion
  (defun bar-caller ()
    (declare (optimize speed))
    (bar "hello" 9)))

(def-test optional-args-correctness ()
  (is (equalp (bar "hello")
              '("hello" 5 7)))
  (is (equalp (bar "hello" 6)
              '("hello" 6 7)))
  (is (equalp (bar "hello" 6 9)
              '("hello" 6 9)))
  (is (equalp (bar-caller)
              '(("hello" 9 7)))))

(define-polymorphic-function baz (c &optional d) :override t)
(let ((a "hello")
      (b 5))
  (defpolymorph baz ((c string) &optional ((d integer) b)) t
    (declare (ignore c))
    (list a d)))

(defun baz-caller (a1 a2)
  (baz a1 a2))

(defun baz-caller-inline (a1 a2)
  (declare (type string a1)
           (type integer a2)
           (optimize speed))
  (baz a1 a2))

(def-test non-null-environment-correctness ()
  (is (equalp (baz-caller "world" 7)
              '("hello" 7)))
  (is (equalp (baz-caller-inline "world" 7)
              '("hello" 7))))

(progn
  (define-polymorphic-function foo (a &optional b) :override t)
  (defpolymorph foo ((str1 string) &optional ((str2 string) "str2")) t
    (declare (ignore str1 str2))
    'string)
  (defpolymorph foo ((num1 number) &optional ((num2 number) pi)) t
    (declare (ignore num1 num2))
    'number))

(def-test untyped-rest-correctness ()
  (is (eq 'string (foo "hello")))
  (is (eq 'string (foo "hello" "world")))
  (is (eq 'number (foo 5.6)))
  (is (eq 'number (foo 5.6 6))))

(progn
  (define-polymorphic-function foobar (a &key key b) :override t)
  (defpolymorph foobar ((str string) &key ((key number) 5) ((b string) "world")) t
    (declare (ignore str))
    (list 'string key b))
  (defpolymorph-compiler-macro foobar (number &key (:key number) (:b string)) (&whole form &rest args)
    (declare (ignore args))
    `(list ,form))
  (defpolymorph foobar ((num number) &key ((key number) 6) ((b string) "world")) t
    (declare (ignore num))
    (list 'number key b))
  (defun foobar-caller ()
    (declare (optimize speed))
    (foobar 7 :key 10)))

(def-test typed-key-correctness ()
  (is (equalp '(string 5 "world")    (foobar "hello")))
  (is (equalp '(string 5.6 "world")  (foobar "hello" :key 5.6)))
  (is (equalp '(number 6 "world")    (foobar 5.6)))
  (is (equalp '(number 9 "world")    (foobar 5.6 :key 9)))
  (is (equalp '((number 10 "world")) (foobar-caller)))
  (is (equalp '(number 6 "bye")      (foobar 5.6 :b "bye")))
  (is (equalp '(number 4.4 "bye")    (foobar 5.6 :b "bye" :key 4.4))))

(progn
  (define-polymorphic-function foz (a) :override t)
  (defpolymorph foz ((a number)) t
    (declare (optimize speed))
    (if (= a 5)
        'number
        (foz "hello")))
  (defpolymorph foz ((a string)) t
    (declare (optimize speed))
    (if (string= a "hello")
        'string
        (foz 5)))
  ;; Will result in infinite expansion upon redefinition, if compilation is not done correctly
  (defpolymorph foz ((a number)) t
    (declare (optimize speed))
    (if (= a 5)
        'number
        (foz "hello"))))

(def-test recursive-correctness ()
  (is (eq 'number (foz 5)))
  (is (eq 'string (foz "hello")))
  (is (eq 'number (foz "world")))
  (is (eq 'string (foz 7))))

(define-polymorphic-function my+ (arg &rest args) :override t)
(progn
  (defpolymorph my+ ((num number) &rest numbers) number
    (if numbers
        (+ num (apply 'my+ numbers))
        num))
  (defpolymorph-compiler-macro my+ (number) (&whole form &rest args)
    (declare (ignore args))
    `(list (+ ,@(cdr form))))
  (defun my+-number-caller ()
    (declare (optimize speed))
    (my+ 3 2 8))
  (defpolymorph my+ ((str string) &rest strings) string
    (apply 'concatenate 'string str strings))
  (defpolymorph my+ ((l list) &rest lists) list
    (apply 'append l lists)))

(def-test untyped-rest-correctness ()
  (is (eq 9 (my+ 2 3 4)))
  (is (string= "helloworld" (my+ "hello" "world")))
  (is (equalp '(1 2 3) (my+ '(1 2) '(3))))
  (is (equalp '(13) (my+-number-caller))))

(def-test undefpolymorph ()
  (with-output-to-string (*error-output*)
    (eval '(define-polymorphic-function fmakunbound-tester (a) :override t))
    (eval '(progn            
            (defpolymorph fmakunbound-tester ((a list)) symbol
              (declare (ignore a))
              'list)
            (defpolymorph fmakunbound-tester ((a string)) symbol
              (declare (ignore a))
              'string)))
    (locally (declare (notinline fmakunbound-tester))
      (is (equalp 'list   (fmakunbound-tester '(a))))
      (is (equalp 'string (fmakunbound-tester "hello")))
      (undefpolymorph 'fmakunbound-tester '(list))
      (is-error (fmakunbound-tester '(a)))
      (is (equalp 'string (fmakunbound-tester "hello"))))))

(with-output-to-string (*error-output*)
  (def-test undefine-polymorphic-function ()
    (eval '(progn
            (define-polymorphic-function undefine-polymorphic-function-tester (a) :override t)
            (defpolymorph undefine-polymorphic-function-tester ((a list)) symbol
              (declare (ignore a))
              'list)
            (defpolymorph undefine-polymorphic-function-tester ((a string)) symbol
              (declare (ignore a))
              'string)))
    (locally (declare (notinline undefine-polymorphic-function-tester))
      (is (equalp 'list   (undefine-polymorphic-function-tester '(a))))
      (is (equalp 'string (undefine-polymorphic-function-tester "hello")))
      (undefine-polymorphic-function 'undefine-polymorphic-function-tester)
      (is-error (undefine-polymorphic-function-tester '(a)))
      (is-error (undefine-polymorphic-function-tester "hello")))))

#+sbcl
(def-test polymorph-sbcl-transforms ()
  (eval `(progn
           (undefine-polymorphic-function 'sbcl-transform)
           (define-polymorphic-function sbcl-transform (a) :override t)
           (defpolymorph sbcl-transform ((a string)) t
             (declare (ignore a))
             nil)))
  (is (= 1 (length
            (sb-c::fun-info-transforms
             (sb-c::fun-info-or-lose 'sbcl-transform)))))
  (eval `(undefpolymorph 'sbcl-transform '(string)))
  (is (= 0 (length
            (sb-c::fun-info-transforms
             (sb-c::fun-info-or-lose 'sbcl-transform)))))
  (eval `(undefine-polymorphic-function 'sbcl-transform)))
