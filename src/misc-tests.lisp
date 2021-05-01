(in-package :adhoc-polymorphic-functions)

(5am:in-suite :adhoc-polymorphic-functions)

(defmacro ignoring-error-output (&body body)
  `(locally (declare #+sbcl(sb-ext:muffle-conditions style-warning))
     (with-output-to-string (*error-output*) ,@body)))

;; unwind-protect (apparantly) does not have an effect in the def-test forms below :/

(def-test required-args-correctness ()
  (ignoring-error-output
    (eval `(progn
             (define-polymorphic-function my= (a b) :overwrite t)
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
    ;; Cannot use an `eval` here, because we do want to test if the
    ;; things work correctly in a non-null lexical environment
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
             (define-polymorphic-function bar (a &optional b c) :overwrite t)
             (defpolymorph bar ((str string) &optional ((b integer) 5) ((c integer) 7)) t
               (list str b c))
             (defpolymorph-compiler-macro bar (string &optional integer integer)
                 (&whole form &rest args)
               (declare (ignore args))
               `(list ,form)) ; This usage of FORM also tests infinite recursion
             (defun bar-caller ()
               (declare (optimize speed))
               (bar "hello" 9)))))
  (is (equalp (eval `(bar "hello"))
              '("hello" 5 7)))
  (is (equalp (eval `(bar "hello" 6))
              '("hello" 6 7)))
  (is (equalp (eval `(bar "hello" 6 9))
              '("hello" 6 9)))
  (is (equalp (eval `(bar-caller))
              '(("hello" 9 7))))
  (undefine-polymorphic-function 'bar)
  (fmakunbound 'bar-caller))

(def-test typed-key-correctness ()
  (ignoring-error-output
    (eval `(progn
             (define-polymorphic-function foobar (a &key key b) :overwrite t)
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
  (is (equalp '(string 5 "world")    (eval `(foobar "hello"))))
  (is (equalp '(string 5.6 "world")  (eval `(foobar "hello" :key 5.6))))
  (is (equalp '(number 6 "world")    (eval `(foobar 5.6))))
  (is (equalp '(number 9 "world")    (eval `(foobar 5.6 :key 9))))
  (is (equalp '((number 10 "world")) (eval `(foobar-caller))))
  (is (equalp '(number 6 "bye")      (eval `(foobar 5.6 :b "bye"))))
  (is (equalp '(number 4.4 "bye")    (eval `(foobar 5.6 :b "bye" :key 4.4))))
  (undefine-polymorphic-function 'foobar)
  (fmakunbound 'foobar-caller))

(def-test recursively-unsafe ()
  (ignoring-error-output
    (eval `(progn
             (define-polymorphic-function foz (a) :overwrite t)
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
  (is (eq 'number (eval `(foz 5))))
  (is (eq 'string (eval `(foz "hello"))))
  (is (eq 'number (eval `(foz "world"))))
  (is (eq 'string (eval `(foz 7))))
  (undefine-polymorphic-function 'foz))

;;; FIXME: Add a test for recursive safety
;;; How to distinguish between runtime call vs compile time call?

(def-test rest-correctness ()
  (ignoring-error-output
    (eval `(define-polymorphic-function my+ (arg &rest args) :overwrite t))
    (eval `(progn
             (defpolymorph my+ ((num number) &rest numbers) number
               (if numbers
                   (+ num (apply 'my+ numbers))
                   num))
             (defpolymorph-compiler-macro my+ (number &rest) (&whole form &rest args)
               (declare (ignore args))
               `(list (+ ,@(cdr form))))
             (defun my+-number-caller ()
               (declare (optimize speed))
               (my+ 3 2 8))
             (defpolymorph my+ ((l list) &rest lists) list
               (apply 'append l lists))
             (defpolymorph my+ ((str string) (num number) &key ((coerce t) nil)) string
               (if coerce
                   (concatenate 'string str (write-to-string num))
                   str)))))
  (is (eq 9 (eval `(my+ 2 3 4))))
  (is (equalp '(1 2 3) (eval `(my+ '(1 2) '(3)))))
  (is (equalp '(13) (eval `(my+-number-caller))))
  (is (string= "hello5" (eval `(my+ "hello" 5 :coerce t))))
  (undefine-polymorphic-function 'my+)
  (fmakunbound 'my+-number-caller))

(def-test undefpolymorph ()
  (with-output-to-string (*error-output*)
    (eval '(define-polymorphic-function undefpolymorph-tester (a) :overwrite t))
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
            (define-polymorphic-function undefine-polymorphic-function-tester (a) :overwrite t)
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
             (define-polymorphic-function sbcl-transform (a) :overwrite t)
             (defpolymorph sbcl-transform ((a string)) t
               (declare (ignore a))))))
  (is (= 1 (length
            (sb-c::fun-info-transforms
             (sb-c::fun-info-or-lose 'sbcl-transform)))))
  (eval `(undefpolymorph 'sbcl-transform '(string)))
  (is (= 0 (length
            (sb-c::fun-info-transforms
             (sb-c::fun-info-or-lose 'sbcl-transform)))))
  (eval `(undefine-polymorphic-function 'sbcl-transform)))

;;; TODO: Add specialization tests pertaining to sbcl-transforms
;;; It feels non-trivial to check if it was the correct transform that was applied
;;; or if it was the correct polymorph being called at runtime.

(def-test ambiguous-type-lists ()
  (ignoring-error-output
    (eval `(progn
             (undefine-polymorphic-function 'ambiguous-type-lists-tester)
             (define-polymorphic-function ambiguous-type-lists-tester (&key a))
             (defpolymorph ambiguous-type-lists-tester (&key ((a string) "")) t
               (declare (ignore a)))))
    (is-error (eval `(defpolymorph ambiguous-type-lists-tester (&key ((a array) #())) t
                       (declare (ignore a)))))
    (is-error (eval `(defpolymorph ambiguous-type-lists-tester
                         (&key ((a (and array (not string))) #())) t
                       (declare (ignore a)))))
    (eval `(undefpolymorph 'ambiguous-type-lists-tester
                           '(&key (:a (and array (not string))))))
    (eval `(undefpolymorph 'ambiguous-type-lists-tester
                           '(&key (:a string))))
    (5am:is-true (eval `(defpolymorph ambiguous-type-lists-tester (&key ((a array))) t
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
             (defpolymorph-compiler-macro most-specialized-polymorph-tester (string) (a)
               (declare (ignore a))
               `'(compiled string))
             (defpolymorph most-specialized-polymorph-tester ((a array)) symbol
               (declare (ignore a))
               'array)
             (defun most-specialized-polymorph-tester-caller ()
               (declare (optimize speed))
               (most-specialized-polymorph-tester "hello"))))
    (let ((a "string")
          (b #(a r r a y)))
      (5am:is-true (eq 'string (most-specialized-polymorph-tester a)))
      (5am:is-true (eq 'array  (most-specialized-polymorph-tester b)))
      (5am:is-true (equalp '(compiled string)
                           (most-specialized-polymorph-tester-caller))))
    (eval `(undefine-polymorphic-function 'most-specialized-polymorph-tester))
    (eval `(fmakunbound 'most-specialized-polymorph-tester-caller))))

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
               (list a b))
             (defpolymorph-compiler-macro (setf foo) (number number) (a b)
               `(list 'compiler-macro ,a ,b))
             (defun setf-foo-caller (a b)
               (declare (optimize speed)
                        (type number a b))
               (funcall #'(setf foo) (the number (+ a b)) b))
             (defun setf-foo-caller-direct (a b)
               (declare (optimize speed)
                        (type number a b))
               (setf (foo b) (the number (+ a b)))))))
  (is (equalp '(compiler-macro 5 3) (eval `(setf-foo-caller 2 3))))
  ;; On SBCL this passed after incorporating compiler macro inside deftransform
  ;; > Don't ask me why it works
  (is (equalp '(compiler-macro 5 3) (eval `(setf-foo-caller-direct 2 3))))
  (fmakunbound 'setf-foo-caller)
  (undefine-polymorphic-function '(setf foo)))
