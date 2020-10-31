(in-package :adhoc-polymorphic-functions)

(5am:in-suite :adhoc-polymorphic-functions)

(defmacro ignoring-error-output (&body body)
  `(with-output-to-string (*error-output*) ,@body))

(def-test required-args-correctness ()
  (ignoring-error-output
    (eval `(progn
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
               (my= 0 5)))))
  (let ((obj1 "hello")
        (obj2 "world")
        (obj3 "hello")
        (obj4 5)
        (obj5 5.0))
    (is (eq t   (my= obj1 obj3)))
    (is (eq nil (my= obj1 obj2)))
    (is (eq t   (my= obj4 obj5)))
    (is-error (my= obj1 obj4))
    (is (eq 'zero (my=-caller))))
  (undefine-polymorphic-function 'my=)
  (fmakunbound 'my=-caller))

(def-test optional-args-correctness ()
  (ignoring-error-output
    (eval `(progn ; This requires SBCL version 2.0.9+
             (define-polymorphic-function bar (a &optional b c) :override t)
             (defpolymorph bar ((str string) &optional ((b integer) 5) ((c integer) 7)) t
               (list str b c))
             (defpolymorph-compiler-macro bar (string &optional integer integer)
                 (&whole form &rest args)
               (declare (ignore args))
               `(list ,form)) ; This usage of FORM also tests infinite recursion
             (defun bar-caller ()
               (declare (optimize speed))
               (bar "hello" 9)))))
  (is (equalp (bar "hello")
              '("hello" 5 7)))
  (is (equalp (bar "hello" 6)
              '("hello" 6 7)))
  (is (equalp (bar "hello" 6 9)
              '("hello" 6 9)))
  (is (equalp (bar-caller)
              '(("hello" 9 7))))
  (undefine-polymorphic-function 'bar)
  (fmakunbound 'bar-caller))

(def-test non-null-environment-correctness ()
  (ignoring-error-output
    (eval `(define-polymorphic-function baz (c &optional d) :override t))
    (eval `(let ((a "hello")
                 (b 5))
             (defpolymorph baz ((c string) &optional ((d integer) b)) t
               (declare (ignore c))
               (list a d))))
    (eval `(defun baz-caller (a1 a2)
             (baz a1 a2)))
    (eval `(defun baz-caller-inline (a1 a2)
             (declare (type string a1)
                      (type integer a2)
                      (optimize speed))
             (baz a1 a2)))
    (is (equalp (baz-caller "world" 7)
                '("hello" 7)))
    (is (equalp (baz-caller-inline "world" 7)
                '("hello" 7)))
    (eval `(progn
             (undefine-polymorphic-function 'baz)
             (fmakunbound 'baz-caller)
             (fmakunbound 'baz-caller-inline)))))

(def-test typed-key-correctness ()
  (ignoring-error-output
    (eval `(progn
             (define-polymorphic-function foobar (a &key key b) :override t)
             (defpolymorph foobar ((str string) &key ((key number) 5) ((b string) "world")) t
               (declare (ignore str))
               (list 'string key b))
             (defpolymorph-compiler-macro foobar (number &key (:key number) (:b string))
                 (&whole form &rest args)
               (declare (ignore args))
               `(list ,form))
             (defpolymorph foobar ((num number) &key ((key number) 6) ((b string) "world")) t
               (declare (ignore num))
               (list 'number key b))
             (defun foobar-caller ()
               (declare (optimize speed))
               (foobar 7 :key 10)))))
  (is (equalp '(string 5 "world")    (foobar "hello")))
  (is (equalp '(string 5.6 "world")  (foobar "hello" :key 5.6)))
  (is (equalp '(number 6 "world")    (foobar 5.6)))
  (is (equalp '(number 9 "world")    (foobar 5.6 :key 9)))
  (is (equalp '((number 10 "world")) (foobar-caller)))
  (is (equalp '(number 6 "bye")      (foobar 5.6 :b "bye")))
  (is (equalp '(number 4.4 "bye")    (foobar 5.6 :b "bye" :key 4.4)))
  (undefine-polymorphic-function 'foobar)
  (fmakunbound 'foobar-caller))

(def-test recursive-correctness ()
  (ignoring-error-output
    (eval `(progn
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
             ;; Will result in infinite expansion upon redefinition,
             ;; if compilation is not done correctly
             (defpolymorph foz ((a number)) t
               (declare (optimize speed))
               (if (= a 5)
                   'number
                   (foz "hello"))))))
  (is (eq 'number (foz 5)))
  (is (eq 'string (foz "hello")))
  (is (eq 'number (foz "world")))
  (is (eq 'string (foz 7)))
  (undefine-polymorphic-function 'foz))

(def-test untyped-rest-correctness ()
  (ignoring-error-output
    (eval `(define-polymorphic-function my+ (arg &rest args) :override t))
    (eval `(progn
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
               (apply 'append l lists)))))
  (is (eq 9 (my+ 2 3 4)))
  (is (string= "helloworld" (my+ "hello" "world")))
  (is (equalp '(1 2 3) (my+ '(1 2) '(3))))
  (is (equalp '(13) (my+-number-caller)))
  (undefine-polymorphic-function 'my+)
  (fmakunbound 'my+-number-caller))

(def-test undefpolymorph ()
  (with-output-to-string (*error-output*)
    (eval '(define-polymorphic-function undefpolymorph-tester (a) :override t))
    (eval '(progn
            (defpolymorph undefpolymorph-tester ((a list)) symbol
              (declare (ignore a))
              'list)
            (defpolymorph undefpolymorph-tester ((a string)) symbol
              (declare (ignore a))
              'string)))
    (locally (declare (notinline undefpolymorph-tester))
      (is (equalp 'list   (undefpolymorph-tester '(a))))
      (is (equalp 'string (undefpolymorph-tester "hello")))
      (undefpolymorph 'undefpolymorph-tester '(list))
      (is-error (undefpolymorph-tester '(a)))
      (is (equalp 'string (undefpolymorph-tester "hello"))))
    (undefine-polymorphic-function 'fmakunbound-tester)))

(def-test undefine-polymorphic-function ()
  (ignoring-error-output
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
      (is-error (undefine-polymorphic-function-tester "hello")))
    (undefine-polymorphic-function 'undefine-polymorphic-function-tester)))

#+sbcl
(def-test polymorph-sbcl-transforms ()
  (with-output-to-string (*error-output*)
    (eval `(progn
             (undefine-polymorphic-function 'sbcl-transform)
             (define-polymorphic-function sbcl-transform (a) :override t)
             (defpolymorph sbcl-transform ((a string)) t))))
  (is (= 1 (length
            (sb-c::fun-info-transforms
             (sb-c::fun-info-or-lose 'sbcl-transform)))))
  (eval `(undefpolymorph 'sbcl-transform '(string)))
  (is (= 0 (length
            (sb-c::fun-info-transforms
             (sb-c::fun-info-or-lose 'sbcl-transform)))))
  (eval `(undefine-polymorphic-function 'sbcl-transform)))

(def-test intersecting-type-lists ()
  (ignoring-error-output
    (eval `(progn
             (undefine-polymorphic-function 'intersecting-type-lists-tester)
             (define-polymorphic-function intersecting-type-lists-tester (a))
             (defpolymorph intersecting-type-lists-tester ((a string)) t)))
    (is-error (eval `(defpolymorph intersecting-type-lists-tester ((a array)) t)))
    (5am:is-true  (eval `(defpolymorph intersecting-type-lists-tester
                             ((a (and array (not string)))) t)))
    (eval `(undefpolymorph 'intersecting-type-lists-tester
                           '((and array (not string)))))
    (eval `(undefpolymorph 'intersecting-type-lists-tester
                           '(string)))
    (5am:is-true  (eval `(defpolymorph intersecting-type-lists-tester ((a array)) t)))
    (is-error (eval `(defpolymorph intersecting-type-lists-tester ((a string)) t)))
    (eval `(undefine-polymorphic-function 'intersecting-type-lists-tester))))

(def-test once-only ()
  (ignoring-error-output
    (eval `(progn
             (define-polymorphic-function my= (&key a b) :override t)
             (defpolymorph my= (&key ((a number) 0) ((b number) 0)) boolean
               (= a b)))))
  (is (= 3 (eval `(let ((a 1))
                    (my= :a (incf a) :b (incf a))
                    a))))
  (undefine-polymorphic-function 'my=))
