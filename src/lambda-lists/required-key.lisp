(in-package :polymorphic-functions)

(defmethod %lambda-list-type ((type (eql 'required-key)) (lambda-list list))
  (let ((state :required))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&key)
                          (setf state '&key))
                         ((and *lambda-list-typed-p*   (listp elt)
                               (valid-parameter-name-p (first  elt)))
                          t)
                         ((and (not *lambda-list-typed-p*)
                               (valid-parameter-name-p elt))
                          t)
                         (t
                          (return-from %lambda-list-type nil))))
        (&key (cond ((and *lambda-list-typed-p*
                          (listp elt)
                          (let ((elt (first elt)))
                            (and (listp elt)
                                 (valid-parameter-name-p (first  elt))))
                          (if (null (third elt))
                              t
                              (valid-parameter-name-p (third elt)))
                          (null (fourth elt)))
                     t)
                    ((and (not *lambda-list-typed-p*)
                          (valid-parameter-name-p elt))
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
    ((type (eql 'required-key)) (untyped-lambda-list list) &optional invalidated-p)
  (let* ((rest-position       (position '&rest untyped-lambda-list))
         (required-parameters (subseq untyped-lambda-list 0 rest-position))
         (keyword-parameters  (subseq untyped-lambda-list (+ 3 rest-position)))
         (rest-args           (nth (1+ rest-position) untyped-lambda-list))
         (block-name          (blockify-name *name*)))
    `((declare (ignorable ,@(mapcar #'first keyword-parameters)
                          ,@(mapcar #'third keyword-parameters))
               (dynamic-extent ,rest-args)
               (optimize speed))
      (block ,block-name
        ,(if invalidated-p
             `(progn
                (update-polymorphic-function-lambda (fdefinition ',*name*))
                (apply (fdefinition ',*name*) ,@required-parameters ,rest-args))
             `(apply
               (the function
                    (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
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
    t))

(defmethod %type-list-subtype-p ((type-1 (eql 'required-key))
                                 (type-2 (eql 'required-key))
                                 list-1 list-2)
  (declare (optimize speed)
           (type list list-1 list-2))
  (let ((key-position-1 (position '&key list-1))
        (key-position-2 (position '&key list-2)))
    (if (= key-position-1 key-position-2)
        (and (every #'subtypep
                    (subseq list-1 0 key-position-1)
                    (subseq list-2 0 key-position-2))
             (loop :for (param type) :in (subseq list-1 (1+ key-position-1))
                   :always (subtypep type (second (assoc (the symbol param)
                                                         (subseq list-2 (1+ key-position-2)))))))
        nil)))

(def-test type-list-subtype-key (:suite type-list-subtype-p)
  (5am:is-true  (type-list-subtype-p '(string &key (:a string))
                                     '(string &key (:a array))))
  (5am:is-true  (type-list-subtype-p '(string &key (:a string))
                                     '(array  &key (:a string))))
  (5am:is-true  (type-list-subtype-p '(string &key (:a string))
                                     '(string &key (:a string) (:b number))))
  (5am:is-false (type-list-subtype-p '(string &key (:a string))
                                     '(string &key (:a number))))
  (5am:is-false (type-list-subtype-p '(string &key (:a string))
                                     '(number &key (:a string))))
  (5am:is-false (type-list-subtype-p '(&key (:a string) (:b number))
                                     '(string &key (:a string) (:b number)))))

(defmethod %type-list-causes-ambiguous-call-p
    ((type-1 (eql 'required-key))
     (type-2 (eql 'required-key))
     list-1 list-2)
  (declare (optimize debug)
           (type list list-1 list-2))
  (let ((key-position-1 (position '&key list-1))
        (key-position-2 (position '&key list-2)))
    (and (= key-position-1 key-position-2)
         (every #'type=
                (subseq list-1 0 key-position-1)
                (subseq list-2 0 key-position-2))
         (let ((list-1 (subseq list-1 (1+ key-position-1)))
               (list-2 (subseq list-2 (1+ key-position-2))))
           (multiple-value-bind (shorter-alist longer-alist)
               (if (<= (length list-1) (length list-2))
                   (values list-1 list-2)
                   (values list-2 list-1))
             (and (loop :for (key type) :in shorter-alist
                        :always (if-let (type-2 (second (assoc key longer-alist)))
                                  (or (type= type type-2)
                                      (and (typep nil type)
                                           (typep nil type-2)))
                                  (return-from %type-list-causes-ambiguous-call-p nil)))
                  (loop :for (key type) :in (set-difference longer-alist shorter-alist
                                                            :key #'first)
                        :always (typep nil type))))))))

(def-test type-list-causes-ambiguous-call-key
    (:suite type-list-causes-ambiguous-call-p)
  (5am:is-true  (type-list-causes-ambiguous-call-p '(string &key (:a (or null string)))
                                                   '(string &key (:a (or null array)))))
  (5am:is-true  (type-list-causes-ambiguous-call-p '(string &key (:a string))
                                                   `(string &key (:a string)
                                                            (:b (or null number)))))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string &key (:a string))
                                                   '(string &key (:a string) (:b number))))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string &key (:a string))
                                                   '(string &key (:a number))))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string &key (:a string))
                                                   '(string &key (:a array))))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string &key (:a string))
                                                   '(number &key (:a string))))
  (5am:is-false (type-list-causes-ambiguous-call-p '(&key (:a string) (:b number))
                                                   '(string &key (:a string) (:b number)))))
