(in-package #:polymorphic-functions)

(5am:in-suite :polymorphic-functions)

(defmacro ignoring-error-output (&body body)
  `(let ((*error-output* (make-string-output-stream))
         (*disable-static-dispatch* nil))
     (handler-bind ((warning #'muffle-warning))
       ,@body)))

;; unwind-protect (apparantly) does not have an effect in the def-test forms below :/

(def-test required-args-correctness ()
  (ignoring-error-output
    (eval `(progn
             (define-polymorphic-function my= (a b) :overwrite t)
             (defpolymorph my= ((a string) (b string)) boolean
               (return-from my= (string= a b)))
             (defpolymorph my= ((a number) (b number)) boolean
               (= a b)))))
  (ignoring-error-output
    (eval `(let ((obj1 "hello")
                 (obj2 "world")
                 (obj3 "hello")
                 (obj4 5)
                 (obj5 5.0))
             (is (eq t   (my= obj1 obj3)))
             (is (eq nil (my= obj1 obj2)))
             (is (eq t   (my= obj4 obj5)))
             (is-error (my= obj1 obj4)))))
  (undefine-polymorphic-function 'my=))

(def-test optional-args-correctness ()
  (ignoring-error-output
    (eval `(progn                  ; This requires SBCL version 2.0.9+
             (define-polymorphic-function bar (a &optional b c) :overwrite t)
             (defpolymorph bar ((str string) &optional ((b integer) 5) ((c integer) 7)) t
               (list str b c)))))
  (is (equal (eval `(bar "hello"))
             '("hello" 5 7)))
  (is (equal (eval `(bar "hello" 6))
             '("hello" 6 7)))
  (is (equal (eval `(bar "hello" 6 9))
             '("hello" 6 9)))
  (undefine-polymorphic-function 'bar))

(def-test typed-key-correctness ()
  (ignoring-error-output
    (eval `(progn
             (define-polymorphic-function foobar (a &key key b) :overwrite t)
             (defpolymorph foobar ((str string) &key ((key number) 5) ((b string) "world")) t
               (declare (ignore str))
               (list 'string key b))
             (defpolymorph foobar ((num number) &key ((key number) 6) ((b string) "world")) t
               (declare (ignore num))
               (list 'number key b)))))
  (is (equal '(string 5 "world")    (eval `(foobar "hello"))))
  (is (equal '(string 5.6 "world")  (eval `(foobar "hello" :key 5.6))))
  (is (equal '(number 6 "world")    (eval `(foobar 5.6))))
  (is (equal '(number 9 "world")    (eval `(foobar 5.6 :key 9))))
  (is (equal '(number 6 "bye")      (eval `(foobar 5.6 :b "bye"))))
  (is (equal '(number 4.4 "bye")    (eval `(foobar 5.6 :b "bye" :key 4.4))))
  (undefine-polymorphic-function 'foobar))

(def-test rest-correctness-1 ()
  (ignoring-error-output
    (eval `(define-polymorphic-function my+ (arg &rest args) :overwrite t))
    (eval `(progn
             (defpolymorph my+ ((num number) &rest numbers) number
               (if numbers
                   (+ num (apply 'my+ numbers))
                   num))
             (defpolymorph my+ ((l list) &rest lists) list
               (apply 'append l lists))
             (defpolymorph my+ ((str string) (num number) &key ((coerce t) nil)) string
               (if coerce
                   (uiop:strcat str (write-to-string num))
                   str)))))
  (is (eq 9 (eval `(my+ 2 3 4))))
  (is (equal '(1 2 3) (eval `(my+ '(1 2) '(3)))))
  #+(or sbcl ccl ecl cmucl)
  (is (string= "hello5" (eval `(my+ "hello" 5 :coerce t))))
  (undefine-polymorphic-function 'my+))

(def-test rest-correctness-2 ()
  (ignoring-error-output
    (unwind-protect
         (progn
           (eval
            `(progn
               (define-polymorphic-function rest-tester (a &rest args) :overwrite t)
               (defpolymorph rest-tester ((a number) &key ((b number) 0)) number
                 (+ a b))
               (defpolymorph rest-tester ((a number) (b number) &key ((c number) 0)) number
                 (+ a b c)))))
      (is (= 4 (eval `(rest-tester 4))))
      (is (= 6 (eval `(rest-tester 4 :b 2))))
      (is (= 6 (eval `(rest-tester 4 2))))
      (is (= 8 (eval `(rest-tester 4 2 :c 2)))))))

(def-test undefpolymorph ()
  (ignoring-error-output
    (eval '(define-polymorphic-function undefpolymorph-tester (a) :overwrite t))
    (eval '(progn
            (defpolymorph undefpolymorph-tester ((a list)) symbol
              (declare (ignore a))
              'list)
            (defpolymorph undefpolymorph-tester ((a string)) symbol
              (declare (ignore a))
              'string)))
    (eval `(locally (declare (notinline undefpolymorph-tester))
             (is (equal 'list   (undefpolymorph-tester '(a))))
             (is (equal 'string (undefpolymorph-tester "hello")))
             (undefpolymorph 'undefpolymorph-tester '(list))
             (is-error (undefpolymorph-tester '(a)))
             (is (equal 'string (undefpolymorph-tester "hello")))))
    (undefine-polymorphic-function 'fmakunbound-tester)))

(def-test undefine-polymorphic-function ()
  (ignoring-error-output
    (eval '(progn
            (define-polymorphic-function undefine-polymorphic-function-tester (a) :overwrite t)
            (defpolymorph undefine-polymorphic-function-tester ((a list)) symbol
              (declare (ignore a))
              'list)
            (defpolymorph undefine-polymorphic-function-tester ((a string)) symbol
              (declare (ignore a))
              'string)))
    (eval `(locally (declare (notinline undefine-polymorphic-function-tester))
             (is (equal 'list   (undefine-polymorphic-function-tester '(a))))
             (is (equal 'string (undefine-polymorphic-function-tester "hello")))
             (undefine-polymorphic-function 'undefine-polymorphic-function-tester)
             (is-error (undefine-polymorphic-function-tester '(a)))
             (is-error (undefine-polymorphic-function-tester "hello"))))
    (undefine-polymorphic-function 'undefine-polymorphic-function-tester)))

(def-test ambiguous-type-lists ()
  (ignoring-error-output
    (eval `(progn
             (undefine-polymorphic-function 'ambiguous-type-lists-tester)
             (define-polymorphic-function ambiguous-type-lists-tester (&key a) :overwrite t)
             (defpolymorph ambiguous-type-lists-tester (&key ((a string) "")) t
               (declare (ignore a)))))
    (is-error (eval `(defpolymorph ambiguous-type-lists-tester (&key ((a (or simple-string number)) 5)) t
                       (declare (ignore a)))))
    (is-error (eval `(defpolymorph ambiguous-type-lists-tester
                         (&key ((a (and array (not string))) #())) t
                       (declare (ignore a)))))
    (eval `(undefpolymorph 'ambiguous-type-lists-tester
                           '(&key (:a (and array (not string))))))
    (eval `(undefpolymorph 'ambiguous-type-lists-tester
                           '(&key (:a string))))
    (5am:is-true (eval `(defpolymorph ambiguous-type-lists-tester (&key ((a array) #())) t
                          (declare (ignore a)))))
    (5am:is-true (eval `(defpolymorph ambiguous-type-lists-tester (&key ((a string) "")) t
                          (declare (ignore a)))))
    (eval `(undefine-polymorphic-function 'ambiguous-type-lists-tester))))

(def-test specialized-type-lists ()
  (ignoring-error-output
    (eval `(progn
             (undefine-polymorphic-function 'most-specialized-polymorph-tester)
             (define-polymorphic-function most-specialized-polymorph-tester (a))
             (defpolymorph most-specialized-polymorph-tester ((a string)) symbol
               (declare (ignore a))
               'string)
             (defpolymorph most-specialized-polymorph-tester ((a array)) symbol
               (declare (ignore a))
               'array)))
    (eval `(let ((a "string")
                 (b #(a r r a y)))
             (5am:is-true (eq 'string (most-specialized-polymorph-tester a)))
             (5am:is-true (eq 'array  (most-specialized-polymorph-tester b)))))
    (eval `(undefine-polymorphic-function 'most-specialized-polymorph-tester))))

(def-test once-only ()
  (ignoring-error-output
    (eval `(progn
             (define-polymorphic-function my= (&key a b) :overwrite t)
             (defpolymorph my= (&key ((a number) 0) ((b number) 0)) boolean
               (= a b)))))
  (is (= 3 (eval `(let ((a 1))
                    (my= :a (incf a) :b (incf a))
                    a))))
  (undefine-polymorphic-function 'my=))

(def-test setf-polymorphs ()
  (ignoring-error-output
    (eval `(progn
             (define-polymorphic-function (setf foo) (a b) :overwrite t)
             (defpolymorph (setf foo) ((a number) (b number)) t
               (list a b)))))
  (is (equal '(2 3) (eval '(funcall #'(setf foo) 2 3))))
  (undefine-polymorphic-function '(setf foo)))


(def-test return-type-check ()
  (ignoring-error-output
    (eval `(progn
             (defun my-identity (x) x)
             (define-polymorphic-function foo (x) :overwrite t))))

  ;; Basic
  (5am:is-true (eval `(defpolymorph foo ((x string)) string x)))
  (5am:is-true (eval `(defpolymorph foo ((x string)) string (my-identity x))))
  (5am:is-true (eval `(defpolymorph foo ((x string))
                          (values string number)
                        (values x 5 #\a))))

  ;; Optional
  (5am:is-true (eval `(defpolymorph foo ((x string))
                          (values string number &optional)
                        (values x 5))))
  ;; Rest
  (5am:is-true (eval `(defpolymorph foo ((x string))
                          (values string number &rest t)
                        (values x 5))))
  (5am:is-true (eval `(defpolymorph foo ((x string))
                          (values string number &rest t)
                        (values x 5 #\a))))

  ;; Optional and Rest
  (5am:is-true (eval `(defpolymorph foo ((x string))
                          (values string number &optional character &rest t)
                        (values x 5 #\a))))
  (5am:is-true (eval `(defpolymorph foo ((x string))
                          (values string number &optional character &rest t)
                        (values x 5))))
  (5am:is-true (eval `(defpolymorph foo ((x string))
                          (values string number &optional character &rest t)
                        (values x 5 #\a ""))))

  (undefine-polymorphic-function 'foo)
  (fmakunbound 'my-identity))
