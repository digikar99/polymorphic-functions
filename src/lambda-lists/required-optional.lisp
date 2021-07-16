(in-package :polymorphic-functions)

(defmethod %lambda-list-type ((type (eql 'required-optional)) (lambda-list list))
  (let ((state :required))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&optional)
                          (setf state '&optional))
                         ((and *lambda-list-typed-p*   (listp elt)
                               (valid-parameter-name-p (first  elt)))
                          t)
                         ((and (not *lambda-list-typed-p*)
                               (valid-parameter-name-p elt))
                          t)
                         (t
                          (return-from %lambda-list-type nil))))
        (&optional (cond ((and *lambda-list-typed-p*   (listp elt)
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

(def-test effective-lambda-list-optional (:suite effective-lambda-list)
  (flet ((effective-typed-lambda-list (typed-lambda-list)
           (let ((typed-lambda-list (normalize-typed-lambda-list typed-lambda-list)))
             (polymorph-effective-lambda-list
              (make-polymorph-parameters-from-lambda-lists
               (untyped-lambda-list typed-lambda-list)
               typed-lambda-list)))))
    (destructuring-bind ((first second third fourth) type-list effective-type-list)
        (multiple-value-list (effective-typed-lambda-list '((a string) (b number) &optional
                                                            ((c number) 5))))
      (is (eq first 'a))
      (is (eq second 'b))
      (is (eq third '&optional))
      (is (equalp '(c 5) fourth))
      (is (equalp type-list '(string number &optional number)))
      (is (equalp effective-type-list '(string number &optional (or null number)))))))

(defmethod compute-polymorphic-function-lambda-body
    ((type (eql 'required-optional)) (effective-lambda-list list) &optional invalidated-p)
  (let* ((optional-position   (position '&optional effective-lambda-list))
         (required-parameters  (subseq effective-lambda-list 0 optional-position))
         (optional-parameters  (subseq effective-lambda-list (1+ optional-position)))
         (args `(nconc (list ,@required-parameters)
                       ,@(loop :for (op default op-p) :in optional-parameters
                               :collect `(when ,op-p (list ,op)))))
         (block-name (blockify-name *name*)))
    (with-gensyms (static-dispatch-fn)
      `((declare (optimize speed)
                 (ignorable ,@(mapcar #'first optional-parameters)
                            ,@(mapcar #'third optional-parameters)))
        (block ,block-name
          ,(if invalidated-p
               `(progn
                  (update-polymorphic-function-lambda (fdefinition ',*name*))
                  (cond ,@(loop :for (name default supplied-p) :in (reverse optional-parameters)
                                :for optional-idx :downfrom (length optional-parameters) :above 0
                                :for parameters := (append required-parameters
                                                           (mapcar #'first
                                                                   (subseq optional-parameters
                                                                           0 optional-idx)))
                                :collect `(,supplied-p
                                           (funcall (fdefinition ',*name*) ,@parameters)))
                        (t
                         (funcall (fdefinition ',*name*) ,@required-parameters))))
               `(let ((,static-dispatch-fn
                        (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
                          (cond
                            ,@(loop
                                :for i :from 0
                                :for polymorph
                                  :in (polymorphic-function-polymorphs (fdefinition *name*))
                                :for static-dispatch-name
                                  := (polymorph-static-dispatch-name polymorph)
                                :for runtime-applicable-p-form
                                  := (polymorph-runtime-applicable-p-form polymorph)
                                :collect
                                `(,runtime-applicable-p-form #',static-dispatch-name))
                            (t
                             (return-from ,block-name
                               (funcall ,(polymorphic-function-default (fdefinition *name*))
                                        ',*name* ,args)))))))
                  (cond ,@(loop :for (name default supplied-p) :in (reverse optional-parameters)
                                :for optional-idx :downfrom (length optional-parameters) :above 0
                                :for parameters := (append required-parameters
                                                           (mapcar #'first
                                                                   (subseq optional-parameters
                                                                           0 optional-idx)))
                                :collect `(,supplied-p
                                           (funcall
                                            (the function ,static-dispatch-fn)
                                            ,@parameters)))
                        (t
                         (funcall (the function ,static-dispatch-fn)
                                  ,@required-parameters))))))))))

(defmethod %sbcl-transform-arg-lvars-from-lambda-list-form ((type (eql 'required-optional))
                                                            (untyped-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  (let ((optional-position (position '&optional untyped-lambda-list)))
    `(append ,@(loop :for arg :in (subseq untyped-lambda-list 0 optional-position)
                     :collect `(list (cons ',arg ,arg)))
             ,@(loop :for param-name :in (subseq untyped-lambda-list
                                                 (1+ optional-position))
                     :collect `(if ,param-name
                                   (list (cons ',param-name ,param-name))
                                   nil)))))

(defmethod %type-list-compatible-p ((type (eql 'required-optional))
                                    (type-list list)
                                    (untyped-lambda-list list))
  (and (length= type-list untyped-lambda-list)
       (if-let ((pos-1 (position '&optional type-list))
                (pos-2 (position '&optional untyped-lambda-list)))
         (= pos-1 pos-2))))

(defmethod %type-list-subtype-p ((type-1 (eql 'required-optional))
                                 (type-2 (eql 'required-optional))
                                 list-1 list-2)
  (declare (optimize speed)
           (type list list-1 list-2))
  (let ((optional-position-1 (position '&optional list-1))
        (optional-position-2 (position '&optional list-2)))
    (if (= optional-position-1 optional-position-2)
        (and (every #'subtypep
                    (subseq list-1 0 optional-position-1)
                    (subseq list-2 0 optional-position-2))
             (every #'subtypep
                    (subseq list-1 (1+ optional-position-1))
                    (subseq list-2 (1+ optional-position-2))))
        ;; Let's hope that this case will be caught by the ambiguous-call-p
        ;; functionality. Let's have this hope for the second part of and above
        ;; as well.
        (error "This case has not been handled!"))))

(def-test type-list-subtype-optional (:suite type-list-subtype-p)
  (5am:is-true  (type-list-subtype-p '(string &optional string)
                                     '(string &optional array)))
  (5am:is-true  (type-list-subtype-p '(&optional string)
                                     '(&optional string number)))
  (5am:is-false (type-list-subtype-p '(string &optional string)
                                     '(string &optional number)))
  (5am:is-false (type-list-subtype-p '(string &optional string)
                                     '(number &optional string))))

(defmethod %type-list-causes-ambiguous-call-p
    ((type-1 (eql 'required-optional))
     (type-2 (eql 'required-optional))
     list-1 list-2)
  (let ((optional-position-1 (position '&optional list-1))
        (optional-position-2 (position '&optional list-2)))
    ;; What if position of optional arguments is not same? Or if lengths are different?
    ;; Eg. '(&optional string) and '(&optional string number) cause an ambiguous call
    ;; Well, yes, but then, any type-list with 0 required arguments would cause an ambiguity.
    (and (= optional-position-1 optional-position-2)
         (every #'type=
                (subseq list-1 0 optional-position-1)
                (subseq list-2 0 optional-position-2)))))

(def-test type-list-causes-ambiguous-call-optional
    (:suite type-list-causes-ambiguous-call-p)
  (5am:is-true  (type-list-causes-ambiguous-call-p '(string &optional string)
                                                   '(string &optional array)))
  (5am:is-true  (type-list-causes-ambiguous-call-p '(&optional string)
                                                   '(&optional string number)))
  (5am:is-true  (type-list-causes-ambiguous-call-p '(string &optional string)
                                                   '(string &optional number)))
  (5am:is-false (type-list-causes-ambiguous-call-p '(string &optional string)
                                                   '(number &optional string))))
