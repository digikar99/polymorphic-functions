(in-package :polymorphic-functions)

(5am:in-suite :polymorphic-functions)

(defmacro define-compiled-function (name lambda-list &body body)
  #+sbcl
  `(defun ,name ,lambda-list ,@body)
  #-sbcl
  `(compile ',name (lambda ,lambda-list ,@body)))

(defmacro ignoring-error-output (&body body)
  `(let ((*error-output* (make-string-output-stream)))
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
               (= a b))
             (defpolymorph-compiler-macro my= (number number) (&whole form a b)
               (declare (ignore b))
               (if (= 0 a)
                   ''zero
                   form))
             ;; Fix ECL: 21.2.1 does not respect
             ;;   (locally (declare (optimize ...))
             ;;     ...)
             ;; In addition, implementations are free to avoid calling compiler-macro.
             ;; As far as ECL is concerned, it does call compiler-macro with COMPILE.
             ;; And even then, some issues prevail; for the time, just forget static dispatch
             ;; on "all" the implementations
             (define-compiled-function my=-caller ()
               (declare (optimize speed (debug 1)))
               (my= 0 5)))))
  (eval `(let ((obj1 "hello")
               (obj2 "world")
               (obj3 "hello")
               (obj4 5)
               (obj5 5.0))
           (is (eq t   (my= obj1 obj3)))
           (is (eq nil (my= obj1 obj2)))
           (is (eq t   (my= obj4 obj5)))
           (is-error (my= obj1 obj4))
           #+(or sbcl ccl ecl cmucl)
           (is (eq 'zero (my=-caller)))))
  (undefine-polymorphic-function 'my=)
  (fmakunbound 'my=-caller))

(def-test optional-args-correctness ()
  (ignoring-error-output
    (eval `(progn                  ; This requires SBCL version 2.0.9+
             (define-polymorphic-function bar (a &optional b c) :overwrite t)
             (defpolymorph bar ((str string) &optional ((b integer) 5) ((c integer) 7)) t
               (list str b c))
             (defpolymorph-compiler-macro bar (string &optional integer integer)
                 (&whole form &rest args)
               (declare (ignore args))
               `(list ,form)) ; This usage of FORM also tests infinite recursion
             (define-compiled-function bar-caller ()
               (declare (optimize speed (debug 1)))
               (bar "hello" 9)))))
  (is (equalp (eval `(bar "hello"))
              '("hello" 5 7)))
  (is (equalp (eval `(bar "hello" 6))
              '("hello" 6 7)))
  (is (equalp (eval `(bar "hello" 6 9))
              '("hello" 6 9)))
  #+(or sbcl ccl ecl cmucl)
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
             (defpolymorph foobar ((num number) &key ((key number) 6) ((b string) "world")) t
               (declare (ignore num))
               (list 'number key b))
             (defpolymorph-compiler-macro foobar (number &key (:key number) (:b string))
                 (&whole form &rest args)
               (declare (ignore args))
               `(list ,form))
             (define-compiled-function foobar-caller ()
               (declare (optimize speed (debug 1)))
               (foobar 7 :key 10)))))
  (is (equalp '(string 5 "world")    (eval `(foobar "hello"))))
  (is (equalp '(string 5.6 "world")  (eval `(foobar "hello" :key 5.6))))
  (is (equalp '(number 6 "world")    (eval `(foobar 5.6))))
  (is (equalp '(number 9 "world")    (eval `(foobar 5.6 :key 9))))
  #+(or sbcl ccl cmucl) ; Fails on ECL for reasons I haven't debugged
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

(def-test rest-correctness-1 ()
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
             (define-compiled-function my+-number-caller ()
               (declare (optimize speed (debug 1)))
               (my+ 3 2 8))
             (defpolymorph my+ ((l list) &rest lists) list
               (apply 'append l lists))
             (defpolymorph my+ ((str string) (num number) &key ((coerce t) nil)) string
               (if coerce
                   (concatenate 'string str (write-to-string num))
                   str)))))
  (is (eq 9 (eval `(my+ 2 3 4))))
  (is (equalp '(1 2 3) (eval `(my+ '(1 2) '(3)))))
  #+(or sbcl ccl ecl cmucl)
  (is (equalp '(13) (eval `(my+-number-caller))))
  (is (string= "hello5" (eval `(my+ "hello" 5 :coerce t))))
  (undefine-polymorphic-function 'my+)
  (fmakunbound 'my+-number-caller))

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
             (is (equalp 'list   (undefpolymorph-tester '(a))))
             (is (equalp 'string (undefpolymorph-tester "hello")))
             (undefpolymorph 'undefpolymorph-tester '(list))
             (is-error (undefpolymorph-tester '(a)))
             (is (equalp 'string (undefpolymorph-tester "hello")))))
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
             (is (equalp 'list   (undefine-polymorphic-function-tester '(a))))
             (is (equalp 'string (undefine-polymorphic-function-tester "hello")))
             (undefine-polymorphic-function 'undefine-polymorphic-function-tester)
             (is-error (undefine-polymorphic-function-tester '(a)))
             (is-error (undefine-polymorphic-function-tester "hello"))))
    (undefine-polymorphic-function 'undefine-polymorphic-function-tester)))

#+sbcl
(def-test polymorph-sbcl-transforms ()
  (ignoring-error-output
    (eval `(progn
             (undefine-polymorphic-function 'sbcl-transform)
             (define-polymorphic-function sbcl-transform (a) :overwrite t)
             (defpolymorph sbcl-transform ((a array)) t
               (list 'array a))
             (defpolymorph sbcl-transform ((a string)) t
               (list 'string a))
             (defpolymorph-compiler-macro sbcl-transform (string) (&whole form a)
               `(list ',(sb-c::type-specifier (sb-c::%lvar-derived-type a)) ,form)))))
  (is (= 2 (length
            (sb-c::fun-info-transforms
             (sb-c::fun-info-or-lose 'sbcl-transform)))))
  (eval `(setf (compiler-macro-function 'sbcl-transform) nil))
  (ignoring-error-output
    (eval `(defun sbcl-transform-caller (b)
             (declare (optimize speed)
                      (type array b))
             (sbcl-transform b))))
  (is (equalp '(array "string") (eval `(sbcl-transform-caller "string"))))
  (ignoring-error-output
    (eval `(defun sbcl-transform-compiler-macro-caller (b)
             (declare (optimize speed)
                      (type string b))
             (sbcl-transform b))))
  (is (equalp '((values string &optional) (string "string"))
              (eval `(sbcl-transform-compiler-macro-caller "string"))))
  (eval `(undefpolymorph 'sbcl-transform '(string)))
  (is (= 1 (length
            (sb-c::fun-info-transforms
             (sb-c::fun-info-or-lose 'sbcl-transform)))))
  (eval `(progn
           (undefine-polymorphic-function 'sbcl-transform)
           (fmakunbound 'sbcl-transform-caller)
           (fmakunbound 'sbcl-transform-compiler-macro-caller))))

#+sbcl
(def-test optional-sbcl-transforms ()
  (ignoring-error-output
    (eval `(progn
             (undefine-polymorphic-function 'optional-transforms)
             (define-polymorphic-function optional-transforms (a &optional b) :overwrite t)
             (defpolymorph optional-transforms ((a string) &optional ((b string) "")) t
               (list 'string a b))))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (optional-transforms a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (optional-transforms a)))))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (optional-transforms a a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (optional-transforms a a)))))
    (eval `(defpolymorph-compiler-macro optional-transforms (string &optional string)
               (&whole form &rest args)
             `(list (list ,@args) ,form)))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (optional-transforms a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (optional-transforms a)))))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (optional-transforms a a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (optional-transforms a a)))))
    (undefine-polymorphic-function 'optional-transforms)))

#+sbcl
(def-test key-sbcl-transforms ()
  (ignoring-error-output
    (eval `(progn
             (undefine-polymorphic-function 'key-transforms)
             (define-polymorphic-function key-transforms (a &key b) :overwrite t)
             (defpolymorph key-transforms ((a string) &key ((b string) "")) t
               (list 'string a b))))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (key-transforms a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (key-transforms a)))))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (key-transforms a :b a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (key-transforms a :b a)))))
    (eval `(defpolymorph-compiler-macro key-transforms (string &key (:b string))
               (&whole form &rest args)
             `(list (list ,@args) ,form)))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (key-transforms a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (key-transforms a)))))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (key-transforms a :b a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (key-transforms a :b a)))))
    (undefine-polymorphic-function 'key-transforms)))

#+sbcl
(def-test rest-sbcl-transforms ()
  (ignoring-error-output
    (eval `(progn
             (undefine-polymorphic-function 'rest-transforms)
             (define-polymorphic-function rest-transforms (a &rest args) :overwrite t)
             (defpolymorph rest-transforms ((s string) &rest args) t
               (list* 'string s args))))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (rest-transforms a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (rest-transforms a)))))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (rest-transforms a a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (rest-transforms a a)))))
    (eval `(defpolymorph-compiler-macro rest-transforms (string &rest) (&whole form &rest args)
             `(list (list ,@args) ,form)))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (rest-transforms a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (rest-transforms a)))))
    (is (equalp (eval `(let ((a "hello"))
                         (declare (optimize speed)
                                  (type string a))
                         (rest-transforms a a)))
                (eval `(let ((a "hello"))
                         (declare (optimize speed))
                         (rest-transforms a a)))))
    (undefine-polymorphic-function 'rest-transforms)))

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
             (defpolymorph-compiler-macro most-specialized-polymorph-tester (string) (a)
               (declare (ignore a))
               `'(compiled string))
             (defpolymorph most-specialized-polymorph-tester ((a array)) symbol
               (declare (ignore a))
               'array)
             (define-compiled-function most-specialized-polymorph-tester-caller ()
               (declare (optimize speed (debug 1)))
               (most-specialized-polymorph-tester "hello"))))
    (eval `(let ((a "string")
                 (b #(a r r a y)))
             (5am:is-true (eq 'string (most-specialized-polymorph-tester a)))
             (5am:is-true (eq 'array  (most-specialized-polymorph-tester b)))
             #+(or sbcl ccl ecl cmucl)
             (5am:is-true (equalp '(compiled string)
                                  (most-specialized-polymorph-tester-caller)))))
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
             (define-compiled-function setf-foo-caller (a b)
               (declare (optimize speed (debug 1))
                        (type number a b))
               (funcall #'(setf foo) (the number (+ a b)) b))
             (define-compiled-function setf-foo-caller-direct (a b)
               (declare (optimize speed (debug 1))
                        (type number a b))
               (setf (foo b) (the number (+ a b)))))))
  (is (equalp '(2 3) (eval '(funcall #'(setf foo) 2 3))))
  #+(or sbcl ccl)
  (is (equalp '(compiler-macro 5 3) (eval `(setf-foo-caller 2 3))))
  ;; On SBCL this passed after incorporating compiler macro inside deftransform
  ;; > Don't ask me why it works
  #+(or sbcl ccl)
  (is (equalp '(compiler-macro 5 3) (eval `(setf-foo-caller-direct 2 3))))
  (fmakunbound 'setf-foo-caller)
  (fmakunbound 'setf-foo-caller-direct)
  (undefine-polymorphic-function '(setf foo)))

(def-test subtype-polymorphism ()
  (unwind-protect
       (handler-bind ((warning #'muffle-warning))
         (eval `(progn
                  (define-polymorphic-function inner (a) :overwrite t)
                  (defpolymorph inner ((a string)) t
                    (declare (ignore a))
                    'string)
                  (defpolymorph inner ((a array)) t
                    (declare (ignore a))
                    'array)))

         (eval `(progn
                  (define-polymorphic-function outer (a) :overwrite t)
                  (defpolymorph (outer :inline t) ((a array)) t
                    (inner a))))
         (is (eq 'string (funcall (compile nil
                                           `(lambda (a)
                                              (declare (optimize speed (debug 1))
                                                       (type string a))
                                              (outer a)))
                                  "hello")))

         (eval `(defpolymorph (outer :inline t) ((a array)) t
                  (let ((b a))
                    (declare (type array b))
                    (inner b))))
         (is (eq 'array (funcall (compile nil
                                           `(lambda (a)
                                              (declare (optimize speed (debug 1))
                                                       (type string a))
                                              (outer a)))
                                 "hello")))
         ;; FIXME: Perhaps, this does not belong here:
         (eval `(defpolymorph (outer :inline t) ((a array)) t
                  (let ((b a))
                    (declare (type-like a b))
                    (inner b))))
         (is (eq 'string (funcall (compile nil
                                           `(lambda (a)
                                              (declare (optimize speed)
                                                       (type string a))
                                              (outer a)))
                                  "hello"))))

    (undefine-polymorphic-function 'inner)
    (undefine-polymorphic-function 'outer)
    (fmakunbound 'outer-caller)))



(def-test parametric-polymorphism ()

  (unwind-protect
       (ignoring-error-output
         (handler-bind ((warning #'muffle-warning))
           (let ((*parametric-type-symbol-predicates*
                   (list (lambda (s)
                           (let* ((name (symbol-name s))
                                  (len  (length name)))
                             (and (char= #\< (elt name 0))
                                  (char= #\> (elt name (1- len)))))))))
             (eval `(progn
                      (define-polymorphic-function foo (a b) :overwrite t)
                      (defpolymorph foo ((array (array <t>))
                                         (elt <t>))
                          t
                        (= elt (aref array 0))))))

           ;; Runtime tests
           (5am:is-true  (eval `(foo (make-array 2 :element-type 'single-float) 0.0)))
           (5am:is-false (eval `(foo (make-array 2 :element-type 'single-float) 1.0)))
           (5am:signals no-applicable-polymorph
             (eval `(foo (make-array 2 :element-type 'single-float) 1.0d0)))

           ;; Compile-time tests
           (5am:is-true  (eval `(funcall (lambda (a b)
                                           (declare (optimize speed)
                                                    (type (simple-array single-float) a)
                                                    (type single-float b))
                                           (foo a b))
                                         (make-array 2 :element-type 'single-float)
                                         0.0)))
           (5am:is-false (eval `(funcall (lambda (a b)
                                           (declare (optimize speed)
                                                    (type (simple-array single-float) a)
                                                    (type single-float b))
                                           (foo a b))
                                         (make-array 2 :element-type 'single-float)
                                         1.0)))
           (ignoring-error-output
             (5am:signals no-applicable-polymorph
               (eval `(funcall (lambda (a b)
                                 (declare (optimize speed)
                                          (type (simple-array single-float) a)
                                          (type double-float b))
                                 (foo a b))
                               (make-array 2 :element-type 'single-float)
                               1.0d0))))))

    (undefine-polymorphic-function 'foo)))
