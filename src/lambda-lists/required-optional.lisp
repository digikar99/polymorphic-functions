(in-package :adhoc-polymorphic-functions)

(defmethod %lambda-list-type ((type (eql 'required-optional)) (lambda-list list))
  (let ((state :required))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&optional)
                          (setf state '&optional))
                         ((and *lambda-list-typed-p*   (listp elt)
                               (valid-parameter-name-p (first  elt))
                               (type-specifier-p       (second elt)))
                          t)
                         ((and (not *lambda-list-typed-p*)
                               (valid-parameter-name-p elt))
                          t)
                         (t
                          (return-from %lambda-list-type nil))))
        (&optional (cond ((and *lambda-list-typed-p*   (listp elt)
                               (let ((elt (first elt)))
                                 (and (listp elt)                                    
                                      (valid-parameter-name-p (first  elt))
                                      (type-specifier-p       (second elt))))
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
    (eq state '&optional)))

(def-test type-identification-optional (:suite lambda-list)
  (is (eq 'required-optional (lambda-list-type '(&optional)))
      "(defun foo (&optional)) does compile")
  (is (eq 'required-optional (lambda-list-type '(a &optional)))
      "(defun foo (a &optional)) does compile")
  (is (eq 'required-optional (lambda-list-type '(a &optional b))))
  (is-error (lambda-list-type '(a &optional 5)))
  (is-error (lambda-list-type '(a &optional b &rest)))
  (is (eq 'required-optional
          (lambda-list-type '((a string) (b number) &optional 
                              ((c number))) ; say if it actually is a null-type?
                            :typed t)))
  (is (eq 'required-optional
          (lambda-list-type '((a string) (b number) &optional 
                              ((c number) 5 c))
                            :typed t)))
  (is (eq 'required-optional
          (lambda-list-type '((a string) (b number) &optional 
                              ((c number) 5 c))
                            :typed t)))
  (is (eq 'required-optional
          (lambda-list-type '((a string) (b number) &optional 
                              ((c number) b c))
                            :typed t)))
  (is-error (lambda-list-type '((a string) (b number) &optional 
                                ((c number) 5 6))
                              :typed t))
  (is-error (lambda-list-type '((a string) (b number) &optional 
                                ((c number) 5 6 7))
                              :typed t))
  (is-error (lambda-list-type '((a string) (b number) &optional 
                                (c number))
                              :typed t)))

(defmethod %defun-lambda-list ((type (eql 'required-optional)) (lambda-list list))
  (let ((state       :required)
        (param-list ())
        (type-list  ()))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&optional)
                          (push '&optional param-list)
                          (push '&optional type-list)
                          (setf state '&optional))
                         ((not *lambda-list-typed-p*)
                          (push elt param-list))
                         (*lambda-list-typed-p*
                          (push (first  elt) param-list)
                          (push (second elt)  type-list))
                         (t
                          (return-from %defun-lambda-list nil))))
        (&optional (cond ((not *lambda-list-typed-p*)
                     (push (list elt nil (gensym (symbol-name elt)))
                           param-list))
                    (*lambda-list-typed-p*
                     (push (cons (caar elt) (cdr elt))
                           param-list)
                     (push (cadar elt) type-list))
                    (t
                     (return-from %defun-lambda-list nil))))))
    (values (nreverse param-list)
            (nreverse  type-list))))

(def-test defun-lambda-list-optional (:suite defun-lambda-list)
  (is (equalp '(a b &optional)
              (defun-lambda-list '(a b &optional))))
  (is-error (defun-lambda-list '(a b &optional &rest)))
  (destructuring-bind (first second third fourth)
      (defun-lambda-list '(a &optional c d))
    (is (eq first 'a))
    (is (eq second '&optional))
    (is (eq 'c (first third)))
    (is (eq 'd (first fourth))))
  (destructuring-bind ((first second third fourth) type-list)
      (multiple-value-list (defun-lambda-list '((a string) (b number) &optional 
                                                ((c number) 5))
                             :typed t))
    (is (eq first 'a))
    (is (eq second 'b))
    (is (eq third '&optional))
    (is (equalp '(c 5) fourth))
    (is (equalp type-list '(string number &optional number)))))

(defmethod %defun-body ((type (eql 'required-optional)) (defun-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  (let ((state       :required)
        (return-list ()))
    (loop :for elt := (first defun-lambda-list)
          :until (eq elt '&optional)
          :do (unless (and (symbolp elt)
                           (not (member elt lambda-list-keywords)))
                (return-from %defun-body nil))
              (push elt return-list)
              (setf defun-lambda-list (rest defun-lambda-list)))
    (when (eq '&optional (first defun-lambda-list))
      (setf state             '&optional
            defun-lambda-list (rest defun-lambda-list))
      (labels ((optional-p-tree (optional-lambda-list)
                 (if (null optional-lambda-list)
                     ()
                     (destructuring-bind (sym default symp) (first optional-lambda-list)
                       (declare (ignore default))
                       `(if ,symp
                            (cons ,sym ,(optional-p-tree (rest optional-lambda-list)))
                            ())))))
        (let ((optional-p-tree (optional-p-tree defun-lambda-list)))
          (values (with-gensyms (apply-list)
                    `(let ((,apply-list ,optional-p-tree))
                       (apply (nth-value 1 (apply 'retrieve-polymorph
                                                  ',*name*
                                                  ,@(reverse return-list)
                                                  ,apply-list))
                              ,@(reverse return-list)
                              ,apply-list)))
                  defun-lambda-list))))))

(defmethod %sbcl-transform-body-args ((type (eql 'required-optional)) (typed-lambda-list list))
  (assert *lambda-list-typed-p*)
  (let ((optional-position (position '&optional typed-lambda-list)))
    (append (mapcar 'first (subseq typed-lambda-list 0 optional-position))
            (loop :for ((param-name type) default) :in (subseq typed-lambda-list
                                                               (1+ optional-position))
                  :collect param-name)
            '(nil))))

(defmethod %lambda-declarations ((type (eql 'required-optional)) (typed-lambda-list list))
  (assert *lambda-list-typed-p*)
  (let ((state        :required)
        (declarations ()))
    (loop :for elt := (first typed-lambda-list)
          :until (eq elt '&optional)
          :do (push `(type ,(second elt) ,(first elt)) declarations)
              (setf typed-lambda-list (rest typed-lambda-list)))
    (when (eq '&optional (first typed-lambda-list))
      (setf state             '&optional
            typed-lambda-list (rest typed-lambda-list))
      (loop :for elt := (first (first typed-lambda-list))
            :while elt
            :do (push `(type ,(second elt) ,(first elt)) declarations)
                (setf typed-lambda-list (rest typed-lambda-list))))
    `(declare ,@(nreverse declarations))))

(defmethod %type-list-compatible-p ((type (eql 'required-optional))
                                    (type-list list)
                                    (untyped-lambda-list list))
  (and (length= type-list untyped-lambda-list)
       (= (position '&optional type-list)
          (position '&optional untyped-lambda-list))))

(defmethod applicable-p-function ((type (eql 'required-optional)) (type-list list))
  (let* ((optional-position (position '&optional type-list))
         (param-list (loop :for i :from 0
                           :for type :in type-list
                           :collect (if (> i optional-position)
                                        (type->param type '&optional)
                                        (type->param type)))))
    `(lambda ,param-list
       (declare (optimize speed))
       (if *compiler-macro-expanding-p*
           (and ,@(loop :for param :in (subseq param-list 0 optional-position)
                        :for type  :in (subseq type-list  0 optional-position)
                        :collect `(our-typep ,param ',type))
                ,@(loop :for (param default supplied-p)
                          :in (subseq param-list (1+ optional-position))
                        :for type  :in (subseq type-list (1+ optional-position))
                        :collect `(or (not ,supplied-p)
                                      (our-typep ,param ',type))))
           (and ,@(loop :for param :in (subseq param-list 0 optional-position)
                        :for type  :in (subseq type-list  0 optional-position)
                        :collect `(typep ,param ',type))
                ,@(loop :for (param default supplied-p)
                          :in (subseq param-list (1+ optional-position))
                        :for type  :in (subseq type-list (1+ optional-position))
                        :collect `(or (not ,supplied-p)
                                      (typep ,param ',type))))))))

(defmethod %type-list-intersect-p ((type (eql 'required-optional)) list-1 list-2)
  (let ((optional-position (position '&optional list-1)))
    (and (eq optional-position (position '&optional list-2))
         (every #'type-intersect-p
                (subseq list-1 0 optional-position)
                (subseq list-2 0 optional-position)))))

(def-test type-list-intersect-optional (:suite type-list-intersect-p)
  (5am:is-true  (type-list-intersect-p '(string &optional string) '(string &optional array)))
  (5am:is-true  (type-list-intersect-p '(string &optional string) '(string &optional number)))
  (5am:is-false (type-list-intersect-p '(string &optional string) '(number)))
  (5am:is-false (type-list-intersect-p '(string &optional string) '(number &optional string))))
