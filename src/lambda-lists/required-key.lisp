(in-package #:polymorphic-functions)

(defmethod %lambda-list-type ((type (eql 'required-key)) (lambda-list list))
  (let ((state :required))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&rest)
                          (setf state '&rest))
                         ((eq elt '&key)
                          (setf state '&key))
                         ((and *lambda-list-typed-p*   (listp elt)
                               (valid-parameter-name-p (first elt)))
                          t)
                         ((and (not *lambda-list-typed-p*)
                               (valid-parameter-name-p elt))
                          t)
                         (t
                          (return-from %lambda-list-type nil))))
        (&rest (cond ((eq elt '&key)
                      (setf state '&key))
                     ((valid-parameter-name-p elt)
                      t)
                     (t
                      (return-from %lambda-list-type nil))))
        (&key (cond ((and *lambda-list-typed-p*
                          (listp elt)
                          (let ((elt (first elt)))
                            (and (listp elt)
                                 (valid-parameter-name-p (first elt))))
                          (if (null (third elt))
                              t
                              (valid-parameter-name-p (third elt)))
                          (null (fourth elt)))
                     t)
                    ((and (not *lambda-list-typed-p*)
                          (or (valid-parameter-name-p elt)
                              (and (listp elt)
                                   (valid-parameter-name-p (first elt)))))
                     t)
                    (t
                     (return-from %lambda-list-type nil))))))
    (eq state '&key)))

(def-test type-identification-key (:suite lambda-list)
  (is (eq 'required-key (lambda-list-type '(&key)))
      "(defun foo (&key)) does compile")
  (is (eq 'required-key (lambda-list-type '(a &key)))
      "(defun foo (a &key)) does compile")
  (is (eq 'required-key (lambda-list-type '(a &key b))))
  (is-error (lambda-list-type '(a &key 5)))
  (is-error (lambda-list-type '(a &key b &rest)))
  (is (eq 'required-key
          (lambda-list-type '((a string) (b number) &key
                              ((c number))) ; say if it actually is a null-type?
                            :typed t)))
  (is (eq 'required-key
          (lambda-list-type '((a string) (b number) &key
                              ((c number) 5 c))
                            :typed t)))
  (is (eq 'required-key
          (lambda-list-type '((a string) (b number) &key
                              ((c number) 5 c))
                            :typed t)))
  (is (eq 'required-key
          (lambda-list-type '((a string) (b number) &key
                              ((c number) b c))
                            :typed t)))
  (is-error (lambda-list-type '((a string) (b number) &key
                                ((c number) 5 6))
                              :typed t))
  (is-error (lambda-list-type '((a string) (b number) &key
                                ((c number) 5 6 7))
                              :typed t))
  (is-error (lambda-list-type '((a string) (b number) &key
                                (c number))
                              :typed t)))

(def-test effective-lambda-list-key (:suite effective-lambda-list)
  (flet ((effective-typed-lambda-list (typed-lambda-list)
           (let ((typed-lambda-list (normalize-typed-lambda-list typed-lambda-list)))
             (polymorph-effective-lambda-list
              (make-polymorph-parameters-from-lambda-lists
               (untyped-lambda-list typed-lambda-list)
               typed-lambda-list)))))
    (destructuring-bind ((first second third fourth) type-list effective-type-list)
        (multiple-value-list (effective-typed-lambda-list '((a string) (b number) &key
                                                            ((c number) 5))))
      (is (eq first 'a))
      (is (eq second 'b))
      (is (eq third '&key))
      (is (equalp '(c 5) fourth))
      (is (equalp type-list
                  '(string number &key (:c number))))
      (is (equalp effective-type-list
                  '(string number &key (:c (or null number))))))))

(defmethod compute-polymorphic-function-lambda-body
    ((type (eql 'required-key)) (untyped-lambda-list list) declaration
     &optional invalidated-p)
  (let* ((rest-position       (position '&rest untyped-lambda-list))
         (required-parameters (subseq untyped-lambda-list 0 rest-position))
         (keyword-parameters  (subseq untyped-lambda-list (+ 3 rest-position)))
         (rest-args           (nth (1+ rest-position) untyped-lambda-list))
         (block-name          (blockify-name *name*)))
    `((declare (ignorable ,@(mapcar #'first keyword-parameters)
                          ,@(mapcar #'third keyword-parameters))
               (dynamic-extent ,rest-args)
               ,declaration)
      (block ,block-name
        ,(if invalidated-p
             `(progn
                (update-polymorphic-function-lambda (fdefinition ',*name*))
                (apply (fdefinition ',*name*) ,@required-parameters ,rest-args))
             `(apply
               (cl:the cl:function
                       (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                                         (compiler-macro-notes:muffle
                                          compiler-macro-notes:optimization-failure-note))
                         (cond
                           ,@(loop
                               :for i :from 0
                               :for polymorph :in (polymorphic-function-polymorphs
                                                   (fdefinition *name*))
                               :for static-dispatch-name
                                 := (polymorph-static-dispatch-name polymorph)
                               :for runtime-applicable-p-form
                                 := (polymorph-runtime-applicable-p-form polymorph)
                               :collect
                               `(,runtime-applicable-p-form #',static-dispatch-name))
                           (t
                            (return-from ,*name*
                              (funcall ,(polymorphic-function-default (fdefinition *name*))
                                       ',*name* nil (list* ,@required-parameters ,rest-args)))))))
               ,@required-parameters ,rest-args))))))

(defmethod %sbcl-transform-arg-lvars-from-lambda-list-form ((type (eql 'required-key))
                                                            (untyped-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  (let ((key-position (position '&key untyped-lambda-list)))
    `(append ,@(loop :for arg :in (subseq untyped-lambda-list 0 key-position)
                     :collect `(list (cons ',arg ,arg)))
             ,@(loop :for param-name :in (subseq untyped-lambda-list
                                                 (1+ key-position))
                     :collect `(if ,param-name
                                   ,(let ((keyword (intern (symbol-name param-name) :keyword)))
                                      `(list (cons ,keyword ,keyword)
                                             (cons ',param-name ,param-name)))
                                   nil)))))

(defmethod %type-list-compatible-p ((type (eql 'required-key))
                                    (type-list list)
                                    (untyped-lambda-list list))
  (let ((pos-key  (position '&key type-list))
        (pos-rest (or (position '&rest untyped-lambda-list)
                      (position '&key untyped-lambda-list))))
    (unless (and (numberp pos-key)
                 (numberp pos-rest)
                 (= pos-key pos-rest))
      (return-from %type-list-compatible-p nil))
    (let ((assoc-list (subseq type-list (1+ pos-key))))
      (loop :for (param default paramp) :in (subseq untyped-lambda-list (+ pos-key 3))
            :do (unless (assoc-value assoc-list (intern (symbol-name param) :keyword))
                  (return-from %type-list-compatible-p nil))))
    (let ((key-list (mapcar #'first (subseq untyped-lambda-list (+ pos-key 3)))))
      (loop :for (key . rest) :in (subseq type-list (1+ pos-key))
            :do (unless (find (intern (symbol-name key) :keyword) key-list :test #'string=)
                  (return-from %type-list-compatible-p nil))))
    t))

(defmethod %type-list-more-specific-p ((type-1 (eql 'required-key))
                                 (type-2 (eql 'required-key))
                                 list-1 list-2)
  (declare (optimize speed)
           (type list list-1 list-2))
  (let ((key-position-1 (position '&key list-1))
        (key-position-2 (position '&key list-2)))
    (if (= key-position-1 key-position-2)
        (and (loop :for type-1 :in (subseq list-1 0 key-position-1)
                   :for type-2 :in (subseq list-2 0 key-position-2)
                   ;; Return T the moment we find a SUBTYPEP with not TYPE=
                   ;; The ones before this point should be TYPE=
                   :do (cond ((type= type-1 type-2)
                              t)
                             ((subtypep type-1 type-2)
                              (return-from %type-list-more-specific-p t))
                             (t
                              (return nil)))
                   :finally (return t))
             ;; Assume the type-lists are ordered in the lexical order
             (loop :for (param-1 type-1) :in (subseq list-1 (1+ key-position-1))
                   :for (param-2 type-2) :in (subseq list-2 (1+ key-position-2))
                   :do (cond ((not (eq param-1 param-2))
                              (return-from %type-list-more-specific-p nil))
                             ((type= type-1 type-2)
                              t)
                             ((subtypep type-1 type-2)
                              (return-from %type-list-more-specific-p t))
                             (t
                              (return nil)))
                   :finally (return t)))
        nil)))

(def-test type-list-subtype-key (:suite type-list-more-specific-p)
  (5am:is-true  (type-list-more-specific-p '(string &key (:a string))
                                           '(string &key (:a array))))
  (5am:is-true  (type-list-more-specific-p '(string &key (:a string))
                                           '(array  &key (:a string))))
  (5am:is-true  (type-list-more-specific-p '(string &key (:a string))
                                           '(string &key (:a string) (:b number))))
  (5am:is-false (type-list-more-specific-p '(string &key (:a string))
                                           '(string &key (:a number))))
  (5am:is-false (type-list-more-specific-p '(string &key (:a string))
                                           '(number &key (:a string))))
  (5am:is-false (type-list-more-specific-p '(&key (:a string) (:b number))
                                           '(string &key (:a string) (:b number)))))

(defmethod %type-list-intersection-null-p
    ((type-1 (eql 'required-key))
     (type-2 (eql 'required-key))
     list-1 list-2)
  (declare (optimize debug)
           (type list list-1 list-2))
  (let ((key-position-1 (position '&key list-1))
        (key-position-2 (position '&key list-2)))
    (or (/= key-position-1 key-position-2)
        (loop :for type-1 :in (subseq list-1 0 key-position-1)
              :for type-2 :in (subseq list-2 0 key-position-2)
              ;; Return T the moment we have a non-null intersection
              ;; without a definite direction of SUBTYPEP
              :do (if (type= type-1 type-2)
                      t
                      (when (definitive-intersection-null-p type-1 type-2
                              (when (boundp '*environment*)
                                *environment*))
                        (return-from %type-list-intersection-null-p t)))
              :finally (return nil))
        (let ((list-1 (subseq list-1 (1+ key-position-1)))
              (list-2 (subseq list-2 (1+ key-position-2))))
          (loop :while (and list-1 list-2)
                :for (key-1 type-1) := (first list-1)
                :for (key-2 type-2) := (first list-2)
                :do (cond ((not (eq key-1 key-2))
                           ;; FIXME: This might not be correct
                           (return-from %type-list-intersection-null-p t))
                          ((type= type-1 type-2)
                           t)
                          (t
                           (let ((subtypep (definitive-subtypep 'null `(and ,type-1 ,type-2))))
                             (if subtypep
                                 ;; Both can accept NIL; the arguments are optional
                                 ()
                                 ;; At least one argument is compulsory
                                 ;; If there exist at least one &KEY argument which need
                                 ;; to be compulsorily supplied, and both types are different,
                                 ;; then there intersection is NULL
                                 (when (definitive-intersection-null-p
                                           type-1 type-2 (when (boundp '*environment*)
                                                           *environment*))
                                   (return-from %type-list-intersection-null-p t))))))
                    (setq list-1 (rest list-1)
                          list-2 (rest list-2))
                :finally
                   (return
                     (cond ((and (null list-1)
                                 (null list-2))
                            nil)
                           (list-1
                            ;; All accept a NIL => intersection is non-NULL
                            (not (loop :for (key type) :in list-1
                                       :always (typep nil type))))
                           (list-2
                            (not (loop :for (key type) :in list-2
                                       :always (typep nil type))))
                           (t
                            (error "Unhandled case!")))))))))

(def-test type-list-intersection-null-key
    (:suite type-list-intersection-null-p)
  (5am:is-false (type-list-intersection-null-p '(string &key (:a (or null string)))
                                               '(string &key (:a (or null array)))))
  (5am:is-false (type-list-intersection-null-p '(string &key (:a string))
                                               `(string &key (:a string)
                                                        (:b (or null number)))))
  (5am:is-true  (type-list-intersection-null-p '(string &key (:a string))
                                               '(string &key (:a string) (:b number))))
  (5am:is-true  (type-list-intersection-null-p '(string &key (:a string))
                                               '(string &key (:a number))))
  (5am:is-false (type-list-intersection-null-p '(string &key (:a string))
                                               '(string &key (:a array))))
  (5am:is-true  (type-list-intersection-null-p '(string &key (:a string))
                                               '(number &key (:a string))))
  (5am:is-true  (type-list-intersection-null-p '(&key (:a string) (:b number))
                                               '(string &key (:a string) (:b number))))
  (5am:is-false (type-list-intersection-null-p '(string array &key (:a string))
                                               '(array string &key (:a string))))
  (5am:is-false (type-list-intersection-null-p '(string string &key (:a string))
                                               '(array array &key (:a string))))
  (5am:is-false (type-list-intersection-null-p '((or string number) &key (:a string))
                                               '((or string symbol) &key (:a string))))
  )
