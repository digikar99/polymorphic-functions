(in-package :typed-functions)

(5am:in-suite :typed-functions)

(progn
  (define-typed-function my= (a b))
  (defun-typed my= ((a string) (b string)) boolean
    (string= a b))
  (defun-typed my= ((a number) (b number)) boolean
    (= a b))
  (define-compiler-macro-typed my= (number number) (&whole form a b)
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

(progn ; This requires SBCL version > 2.0.8: there's a commit after 2.0.8 was released.
  (define-typed-function bar (a &optional b c))
  (defun-typed bar ((str string) &optional ((b integer) 5) ((c integer) 7)) t
    (list str b c))
  (define-compiler-macro-typed bar (string &optional integer integer) (&whole form &rest args)
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

(let ((a "hello")
      (b 5))
  (define-typed-function baz (c &optional d))
  (defun-typed baz ((c string) &optional ((d integer) b)) t
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
  (define-typed-function foo (a &optional b))
  (defun-typed foo ((str1 string) &optional ((str2 string) "str2")) t
    (declare (ignore str1 str2))
    'string)
  (defun-typed foo ((num1 number) &optional ((num2 number) pi)) t
    (declare (ignore num1 num2))
    'number))

(def-test untyped-rest-correctness ()
  (is (eq 'string (foo "hello")))
  (is (eq 'string (foo "hello" "world")))
  (is (eq 'number (foo 5.6)))
  (is (eq 'number (foo 5.6 6))))

(progn
  (define-typed-function foobar (a &key key b))
  (defun-typed foobar ((str string) &key ((key number) 5) ((b string) "world")) t
    (declare (ignore str))
    (list 'string key b))
  (define-compiler-macro-typed foobar (number &key :key number :b string) (&whole form &rest args)
    (declare (ignore args))
    `(list ,form))
  (defun-typed foobar ((num number) &key ((key number) 6) ((b string) "world")) t
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
  (define-typed-function foz (a))
  (defun-typed foz ((a number)) t
    (declare (optimize speed))
    (if (= a 5)
        'number
        (foz "hello")))
  (defun-typed foz ((a string)) t
    (declare (optimize speed))
    (if (string= a "hello")
        'string
        (foz 5)))
  ;; Will result in infinite expansion upon redefinition, if compilation is not done correctly
  (defun-typed foz ((a number)) t
    (declare (optimize speed))
    (if (= a 5)
        'number
        (foz "hello"))))

(def-test recursive-correctness ()
  (is (eq 'number (foz 5)))
  (is (eq 'string (foz "hello")))
  (is (eq 'number (foz "world")))
  (is (eq 'string (foz 7))))

(progn
  (define-typed-function my+ (arg &rest args))
  (defun-typed my+ ((num number) &rest numbers) number
    (if numbers
        (+ num (apply 'my+ numbers))
        num))
  (define-compiler-macro-typed my+ (number) (&whole form &rest args)
    (declare (ignore args))
    `(list (+ ,@(cdr form))))
  (defun my+-number-caller ()
    (declare (optimize speed))
    (my+ 3 2 8))
  (defun-typed my+ ((str string) &rest strings) string
    (apply 'concatenate 'string str strings))
  (defun-typed my+ ((l list) &rest lists) list
    (apply 'append l lists)))

(def-test untyped-rest-correctness ()
  (is (eq 9 (my+ 2 3 4)))
  (is (string= "helloworld" (my+ "hello" "world")))
  (is (equalp '(1 2 3) (my+ '(1 2) '(3))))
  (is (equalp '(13) (my+-number-caller))))

(def-test fmakunbound-typed ()
  (with-output-to-string (*error-output*)
    (eval '(progn
            (define-typed-function fmakunbound-tester (a))
            (defun-typed fmakunbound-tester ((a list)) symbol
              (declare (ignore a))
              'list)
            (defun-typed fmakunbound-tester ((a string)) symbol
              (declare (ignore a))
              'string)))
    (locally (declare (notinline fmakunbound-tester))
      (is (equalp 'list   (fmakunbound-tester '(a))))
      (is (equalp 'string (fmakunbound-tester "hello")))
      (fmakunbound-typed 'fmakunbound-tester '(list))
      (is-error (fmakunbound-tester '(a)))
      (is (equalp 'string (fmakunbound-tester "hello"))))))

(with-output-to-string (*error-output*)
  (def-test undefine-typed-function ()
    (eval '(progn
            (define-typed-function undefine-typed-function-tester (a))
            (defun-typed undefine-typed-function-tester ((a list)) symbol
              (declare (ignore a))
              'list)
            (defun-typed undefine-typed-function-tester ((a string)) symbol
              (declare (ignore a))
              'string)))
    (locally (declare (notinline undefine-typed-function-tester))
      (is (equalp 'list   (undefine-typed-function-tester '(a))))
      (is (equalp 'string (undefine-typed-function-tester "hello")))
      (undefine-typed-function 'undefine-typed-function-tester)
      (is-error (undefine-typed-function-tester '(a)))
      (is-error (undefine-typed-function-tester "hello")))))
