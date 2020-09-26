(in-package :typed-dispatch)

(defmethod %lambda-list-type ((type (eql 'required-key)) (lambda-list list))
  (let ((state :required))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&key)
                          (setf state '&key))
                         ((and *lambda-list-typed-p*   (listp elt)
                               (valid-parameter-name-p (first  elt))
                               (type-specifier-p       (second elt)))
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

(defmethod %defun-lambda-list ((type (eql 'required-key)) (lambda-list list))
  (let ((state       :required)
        (param-list ())
        (type-list  ()))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&key)
                          (push '&key param-list)
                          (push '&key type-list)
                          (setf state '&key))
                         ((not *lambda-list-typed-p*)
                          (push elt param-list))
                         (*lambda-list-typed-p*
                          (push (first  elt) param-list)
                          (push (second elt)  type-list))
                         (t
                          (return-from %defun-lambda-list nil))))
        (&key (cond ((not *lambda-list-typed-p*)
                     (push (list elt nil (gensym (symbol-name elt)))
                           param-list))
                    (*lambda-list-typed-p*
                     (push (cons (caar elt) (cdr elt))
                           param-list)
                     (push (caar  elt) type-list)
                     (push (cadar elt) type-list))
                    (t
                     (return-from %defun-lambda-list nil))))))
    (values (nreverse param-list)
            (nreverse  type-list))))

(def-test defun-lambda-list-key (:suite defun-lambda-list)
  (is (equalp '(a b &key)
              (defun-lambda-list '(a b &key))))
  (is-error (defun-lambda-list '(a b &rest args &key)))
  (destructuring-bind (first second third fourth)
      (defun-lambda-list '(a &key c d))
    (is (eq first 'a))
    (is (eq second '&key))
    (is (eq 'c (first third)))
    (is (eq 'd (first fourth))))
  (destructuring-bind ((first second third fourth) type-list)
      (multiple-value-list (defun-lambda-list '((a string) (b number) &key 
                                                ((c number) 5))
                             :typed t))
    (is (eq first 'a))
    (is (eq second 'b))
    (is (eq third '&key))
    (is (equalp '(c 5) fourth))
    (is (equalp type-list '(string number &key c number)))))

(defmethod %defun-body ((type (eql 'required-key)) (defun-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  (let ((state       :required)
        (return-list ()))
    (loop :for elt := (first defun-lambda-list)
          :until (eq elt '&key)
          :do (unless (and (symbolp elt)
                           (not (member elt lambda-list-keywords)))
                (return-from %defun-body nil))
              (push elt return-list)
              (setf defun-lambda-list (rest defun-lambda-list)))
    (when (eq '&key (first defun-lambda-list))
      (setf state             '&key
            defun-lambda-list (rest defun-lambda-list))
      (labels ((key-p-tree (key-lambda-list)
                 (if (null key-lambda-list)
                     ()
                     (destructuring-bind (sym default symp) (first key-lambda-list)
                       (declare (ignore default))
                       (let ((recurse-result (key-p-tree (rest key-lambda-list))))
                         `(if ,symp
                              (cons ,(intern (symbol-name sym) :keyword)
                                    (cons ,sym ,recurse-result))
                              ,recurse-result))))))
        (let ((key-p-tree (key-p-tree defun-lambda-list)))
          (values `(let ((apply-list ,key-p-tree))
                     (apply (nth-value 1 (apply 'retrieve-typed-function
                                              ',*name*
                                              ,@(reverse return-list)
                                              apply-list))
                            ,@(reverse return-list)
                            apply-list))
                  defun-lambda-list))))))

(defmethod %lambda-declarations ((type (eql 'required-key)) (typed-lambda-list list))
  (assert *lambda-list-typed-p*)
  (let ((state        :required)
        (declarations ()))
    (loop :for elt := (first typed-lambda-list)
          :until (eq elt '&key)
          :do (push `(type ,(second elt) ,(first elt)) declarations)
              (setf typed-lambda-list (rest typed-lambda-list)))
    (when (eq '&key (first typed-lambda-list))
      (setf state             '&key
            typed-lambda-list (rest typed-lambda-list))
      (loop :for elt := (first (first typed-lambda-list))
            :while elt
            :do (push `(type ,(second elt) ,(first elt)) declarations)
                (setf typed-lambda-list (rest typed-lambda-list))))
    `(declare ,@(nreverse declarations))))
