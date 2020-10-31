(in-package :adhoc-polymorphic-functions)

(defmethod %lambda-list-type ((type (eql 'required-untyped-rest)) (lambda-list list))
  (let ((state :required))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&rest)
                          (setf state '&rest))
                         ((and *lambda-list-typed-p*   (listp elt)
                               (valid-parameter-name-p (first  elt))
                               (type-specifier-p       (second elt)))
                          t)
                         ((and (not *lambda-list-typed-p*)
                               (valid-parameter-name-p elt))
                          t)
                         (t
                          (return-from %lambda-list-type nil))))
        (&rest (if (valid-parameter-name-p elt)
                   (setf state :done)
                   (return-from %lambda-list-type nil)))
        (:done (return-from %lambda-list-type nil))))
    (eq state :done)))

(def-test type-identification-untyped-rest (:suite lambda-list)
  (is (eq 'required-untyped-rest (lambda-list-type '(a b &rest args))))
  (is-error (lambda-list-type '(a 5)))
  (is-error (lambda-list-type '(a b &rest)))
  (is-error (lambda-list-type '(a b &rest c d)))
  (is (eq 'required-untyped-rest
          (lambda-list-type '((a string) &rest args)
                            :typed t)))
  (is-error (lambda-list-type '((a string) &rest (args number)) :typed t)))

(defmethod %defun-lambda-list ((type (eql 'required-untyped-rest)) (lambda-list list))
  (let ((rest-position (position '&rest lambda-list)))
    (if *lambda-list-typed-p*
        (values (append (mapcar 'first (subseq lambda-list 0 rest-position))
                        (subseq lambda-list rest-position))
                (mapcar 'second (subseq lambda-list 0 rest-position)))
        (copy-list lambda-list))))

(def-test defun-lambda-list-untyped-rest (:suite defun-lambda-list)
  (is (equalp '(a b &rest c)
              (defun-lambda-list '(a b &rest c))))
  (is-error (defun-lambda-list '(a b &rest)))
  (destructuring-bind (first second third)
      (defun-lambda-list '(a &rest c))
    (is (eq first 'a))
    (is (eq second '&rest))
    (is (eq 'c third)))
  (destructuring-bind ((first second third fourth) type-list)
      (multiple-value-list (defun-lambda-list '((a string) (b number) &rest
                                                c)
                             :typed t))
    (is (eq first 'a))
    (is (eq second 'b))
    (is (eq third '&rest))
    (is (eq fourth 'c))
    (is (equalp type-list '(string number)))))

(defmethod %defun-body ((type (eql 'required-untyped-rest)) (defun-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  `(apply (nth-value 1 (retrieve-polymorph ',*name*
                                                ,@(subseq defun-lambda-list
                                                          0
                                                          (position '&rest defun-lambda-list))))
          ,@(remove '&rest defun-lambda-list)))

(defmethod %sbcl-transform-body-args ((type (eql 'required-untyped-rest)) (typed-lambda-list list))
  (assert *lambda-list-typed-p*)
  (let ((rest-position (position '&rest typed-lambda-list)))
    (append (mapcar 'first (subseq typed-lambda-list 0 rest-position))
            (last typed-lambda-list))))

(defmethod %lambda-declarations ((type (eql 'required-untyped-rest)) (typed-lambda-list list))
  (assert *lambda-list-typed-p*)
  `(declare ,@(mapcar (lambda (elt)
                        `(type ,(second elt) ,(first elt)))
                      (subseq typed-lambda-list
                              0
                              (position '&rest typed-lambda-list)))))

(defmethod %type-list-compatible-p ((type (eql 'required-untyped-rest))
                                    (type-list list)
                                    (untyped-lambda-list list))
  (= (length type-list) (position '&rest untyped-lambda-list)))

(defmethod applicable-p-function ((type (eql 'required-untyped-rest)) (type-list list))
  (let* ((rest-position (position '&rest type-list))
         (param-list (append (mapcar #'type->param (subseq type-list 0 rest-position))
                             `(&rest ,(gensym)))))
    `(lambda ,param-list
       (declare (optimize speed)
                (ignore ,@(last param-list)))
       (if *compiler-macro-expanding-p*
           (and ,@(loop :for param :in (subseq param-list 0 rest-position)
                        :for type  :in (subseq type-list  0 rest-position)
                        :collect `(our-typep ,param ',type)))
           (and ,@(loop :for param :in (subseq param-list 0 rest-position)
                        :for type  :in (subseq type-list  0 rest-position)
                        :collect `(typep ,param ',type)))))))

(defmethod %type-list-intersect-p ((type (eql 'required-untyped-rest)) list-1 list-2)
  (let ((rest-position (position '&rest  list-1)))
    (and (eq rest-position (position '&rest list-2))
         (every #'type-intersect-p
                (subseq list-1 rest-position)
                (subseq list-2 rest-position)))))

(def-test type-list-intersect-untyped-rest (:suite type-list-intersect-p)
  (5am:is-true  (type-list-intersect-p '(string string) '(string array)))
  (5am:is-false (type-list-intersect-p '(string string) '(string number)))
  (5am:is-false (type-list-intersect-p '(string string) '(string))))
