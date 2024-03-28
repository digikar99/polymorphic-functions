(in-package #:polymorphic-functions)

(defmethod %lambda-list-type ((type (eql 'rest)) (lambda-list list))
  (let ((state :required))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&rest)
                          (setf state '&rest))
                         ((and *lambda-list-typed-p*   (listp elt)
                               (valid-parameter-name-p (first  elt)))
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

(def-test type-identification-rest (:suite lambda-list)
  (is (eq 'rest (lambda-list-type '(&rest args))))
  (is (eq 'rest (lambda-list-type '(a b &rest c))))
  (is-error (lambda-list-type '(a 5)))
  (is-error (lambda-list-type '(a b &rest)))
  (is-error (lambda-list-type '(a b &rest c d)))
  (is (eq 'rest
          (lambda-list-type '((a string) &rest args)
                            :typed t)))
  (is-error (lambda-list-type '((a string) &rest (args number)) :typed t)))

(def-test effective-lambda-list-rest (:suite effective-lambda-list)
  (flet ((effective-typed-lambda-list (typed-lambda-list)
           (let ((typed-lambda-list (normalize-typed-lambda-list typed-lambda-list)))
             (polymorph-effective-lambda-list
              (make-polymorph-parameters-from-lambda-lists
               (untyped-lambda-list typed-lambda-list)
               typed-lambda-list)))))
    (destructuring-bind ((first second third fourth) type-list effective-type-list)
        (multiple-value-list (effective-typed-lambda-list '((a string) (b number) &rest
                                                            c)))
      (is (eq first 'a))
      (is (eq second 'b))
      (is (eq third '&rest))
      (is (eq fourth 'c))
      (is (equalp type-list '(string number &rest)))
      (is (equalp effective-type-list
                  '(string number &rest))))))

(defmethod compute-polymorphic-function-lambda-body
    ((type (eql 'rest)) (untyped-lambda-list list) declaration &optional invalidated-p)
  (let* ((rest-position       (position '&rest untyped-lambda-list))
         (rest-args           (nth (1+ rest-position) untyped-lambda-list))
         (required-parameters (subseq untyped-lambda-list 0 rest-position))
         (block-name          (blockify-name *name*)))
    `((declare (ignorable ,@required-parameters)
               (dynamic-extent ,rest-args)
               ,declaration)
      (block ,block-name
        ,(if invalidated-p
             `(progn
                (update-polymorphic-function-lambda (fdefinition ',*name*))
                (apply (fdefinition ',*name*) ,@required-parameters ,rest-args))
             `(apply
               (cl:the cl:function
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
                            (return-from ,block-name
                              (funcall ,(polymorphic-function-default (fdefinition *name*))
                                       ',*name* nil (list* ,@required-parameters ,rest-args)))))))
               ,@required-parameters ,rest-args))))))

(defmethod %sbcl-transform-arg-lvars-from-lambda-list-form ((type (eql 'rest))
                                                            (untyped-lambda-list list))
  (assert (not *lambda-list-typed-p*))
  (let ((rest-position (position '&rest untyped-lambda-list)))
    (with-gensyms (lvar)
      `(list* ,@(loop :for arg :in (subseq untyped-lambda-list 0 rest-position)
                      :collect `(cons ',arg ,arg))
              (loop :for ,lvar :in ,(nth (1+ rest-position) untyped-lambda-list)
                    :collect (cons (gensym) ,lvar))))))

(defmethod %type-list-compatible-p ((type (eql 'rest))
                                    (type-list list)
                                    (untyped-lambda-list list))
  (let ((rest-position (position '&rest untyped-lambda-list)))
    (cond ((member '&rest type-list)
           (<= rest-position (position '&rest type-list)))
          ((member '&key type-list)
           (<= rest-position (position '&key type-list)))
          (t
           (<= rest-position (length type-list))))))

(5am:def-suite type-list-subtype-rest :in type-list-more-specific-p)

(defmethod %type-list-more-specific-p
    ((type-1 (eql 'rest)) (type-2 (eql 'rest)) list-1 list-2)
  (let ((rest-position-1 (position '&rest list-1))
        (rest-position-2 (position '&rest list-2)))
    (%type-list-more-specific-p 'required 'required
                          (subseq list-1 0 (min rest-position-1 rest-position-2))
                          (subseq list-2 0 (min rest-position-1 rest-position-2)))))

(def-test type-list-subtype-rest-&-rest (:suite type-list-subtype-rest)
  (5am:is-true  (type-list-more-specific-p '(string array &rest)  '(array string &rest)))
  (5am:is-true  (type-list-more-specific-p '(string string &rest) '(string array &rest)))
  (5am:is-true  (type-list-more-specific-p '(string string &rest) '(string &rest)))
  (5am:is-false (type-list-more-specific-p '(string string &rest) '(string number &rest))))

(defmethod %type-list-more-specific-p
    ((type-1 (eql 'rest)) (type-2 (eql 'required)) list-1 list-2)
  ;; No way the first is going to be a subtype of the other
  ;; The first type-list will either dominate the second or be orthogonal to it
  ;; Or a bit dirty!
  nil)

(def-test type-list-subtype-rest-&-required (:suite type-list-subtype-rest)
  (5am:is-false (type-list-more-specific-p '(string string &rest) '(string string)))
  (5am:is-false (type-list-more-specific-p '(string string &rest) '(string array)))
  (5am:is-false (type-list-more-specific-p '(string &rest) '(string array)))
  (5am:is-false (type-list-more-specific-p '(string &rest) '(array array)))
  (5am:is-false (type-list-more-specific-p '(string string &rest) '(string)))
  (5am:is-false (type-list-more-specific-p '(string string &rest) '(string number))))

(defmethod %type-list-more-specific-p
    ((type-1 (eql 'required)) (type-2 (eql 'rest)) list-1 list-2)
  (let ((list-1-length (length list-1))
        (rest-position (position '&rest list-2)))
    (cond ((< list-1-length rest-position)
           nil)
          ((= list-1-length rest-position)
           (%type-list-more-specific-p 'required 'required
                                 (subseq list-1 0 list-1-length)
                                 (subseq list-2 0 list-1-length)))
          (t nil))))

(def-test type-list-subtype-required-&-rest (:suite type-list-subtype-rest)
  (5am:is-true  (type-list-more-specific-p '(string array)  '(array string &rest)))
  (5am:is-true  (type-list-more-specific-p '(string string) '(string string &rest)))
  (5am:is-true  (type-list-more-specific-p '(string string) '(string array &rest)))
  (5am:is-true  (type-list-more-specific-p '(string)        '(string &rest)))
  (5am:is-false (type-list-more-specific-p '(string array)  '(string string &rest)))
  (5am:is-false (type-list-more-specific-p '(string)        '(string array &rest))))

;;; Ignore the case of required-optional against all others
;;; TODO: Simplify the below methods :D

(defmethod %type-list-more-specific-p ((type-1 (eql 'rest))
                                 (type-2 (eql 'required-key))
                                 list-1 list-2)
  ;; The first type-list will either dominate the second or be orthogonal to it
  ;; Or a bit dirty!
  nil)

(def-test type-list-subtype-rest-&-required-key (:suite type-list-subtype-rest)
  (5am:is-false (type-list-more-specific-p '(string keyword string number &rest)
                                     '(string &key (:a number) (:b string))))
  (5am:is-false (type-list-more-specific-p '(string keyword string  &rest)
                                     '(string &key (:a number) (:b number))))
  (5am:is-false (type-list-more-specific-p '(string keyword string  &rest)
                                     '(string &key (:a number))))
  (5am:is-false (type-list-more-specific-p '(string keyword string number &rest)
                                     '(string &key (:a number) (:b string))))
  (5am:is-false (type-list-more-specific-p '(string keyword &rest) '(string &key)))
  (5am:is-false (type-list-more-specific-p '(string string  &rest) '(string &key)))
  (5am:is-false (type-list-more-specific-p '(string string  &rest) '(string &key (:a string)))))

;;; TODO: Simplify the below methods :D

(defmethod %type-list-more-specific-p ((type-1 (eql 'required-key))
                                       (type-2 (eql 'rest))
                                       list-1 list-2)
  (let ((key-position  (position '&key  list-1))
        (rest-position (position '&rest list-2)))
    (cond ((= key-position rest-position)
           (%type-list-more-specific-p 'required 'required
                                       (subseq list-1 0 key-position)
                                       (subseq list-2 0 key-position)))
          ((< rest-position key-position)
           (%type-list-more-specific-p 'required 'required
                                       (subseq list-1 0 rest-position)
                                       (subseq list-2 0 rest-position)))
          ((< key-position rest-position)
           (and (%type-list-more-specific-p 'required 'required
                                            (subseq list-1 0 key-position)
                                            (subseq list-2 0 key-position))
                ;; The args between &KEY and &REST should be subtype.
                ;; For this we need two things:
                ;; 1. KEYWORD-supertypes at odd positions
                (let ((rest (subseq list-2 key-position rest-position)))
                  (loop :for (name type) :in (subseq list-1 (1+ key-position))
                        :while rest
                        :for type-1 := (first rest)
                        :for type-2 := (second rest)
                        :always (typep name type-1)
                        :do (setq rest (cddr rest))))
                ;; 2. SUBTYPEs at even positions
                (let ((rest     (subseq list-2 key-position rest-position))
                      (subtypep nil))
                  (loop :for (name type) :in (subseq list-1 (1+ key-position))
                        :while (and rest (not subtypep))
                        :for type-1 := (first rest)
                        :for type-2 := (second rest)
                        :do (if (or (and (rest rest) ; we haven't exhausted REST yet
                                         (typep name type-1)
                                         (subtypep type type-2))
                                    (and (null (rest rest)) ; we exhausted REST at the type-2
                                         (typep name type-1)))
                                (setq subtypep t)))
                  subtypep))))))

(def-test type-list-subtype-required-key-&-rest (:suite type-list-subtype-rest)
  (5am:is-true  (type-list-more-specific-p '(string array &key) '(array string &rest)))
  (5am:is-true  (type-list-more-specific-p '(string array &key) '(string &rest)))
  (5am:is-true  (type-list-more-specific-p '(string &key (:a string)) '(string keyword  &rest)))
  (5am:is-true  (type-list-more-specific-p '(string &key (:a string))
                                     '(string keyword string &rest)))
  (5am:is-true  (type-list-more-specific-p '(string &key (:a string))
                                     '(array  keyword string &rest)))
  (5am:is-true  (type-list-more-specific-p '(string &key (:a number) (:b string))
                                     '(string keyword string &rest)))
  (5am:is-true  (type-list-more-specific-p '(string string &key) '(string string &rest)))
  (5am:is-false (type-list-more-specific-p '(string array &key) '(string string &rest)))
  (5am:is-false (type-list-more-specific-p '(string &key (:a number) (:b string))
                                     '(string keyword string number &rest))))

(defmethod %type-list-more-specific-p ((type-1 (eql 'required-key))
                                 (type-2 (eql 'required))
                                 list-1 list-2)
  nil)

(defmethod %type-list-more-specific-p ((type-1 (eql 'required))
                                 (type-2 (eql 'required-key))
                                 list-1 list-2)
  (let* ((list-1-length (length list-1))
         (key-position  (position '&key  list-2))
         (n-required-1  list-1-length)
         (n-required-2  key-position))
    (cond ((= n-required-1 n-required-2)
           (%type-list-more-specific-p 'required 'required
                                 (subseq list-1 0 list-1-length)
                                 (subseq list-2 0 list-1-length)))
          ((< n-required-1 n-required-2)
           nil)
          ((> n-required-1 n-required-2)
           ;; There are places in list-1 that could very well be the equivalents of &key args!
           ;; Let's just hope the other cases are caught by the ambiguous-call detection
           ;; and we are never required to check type-list-more-specific-p for them
           (and (evenp (- n-required-1 n-required-2))
                (%type-list-more-specific-p 'required 'required
                                      (subseq list-1 0 n-required-2)
                                      (subseq list-2 0 n-required-2))
                (let ((key-type-type-pairs-1 (remove-duplicates
                                              (loop :with i := 0
                                                    :while (< i (- n-required-1 n-required-2))
                                                    :with rest := (subseq list-1 n-required-2)
                                                    :collect (subseq rest i (+ i 2))
                                                    :do (incf i 2))
                                              :from-end t
                                              :test #'type= :key #'first))
                      (key-type-type-pairs-2 (mapcar (lambda (key-type)
                                                       `((eql ,(first key-type))
                                                         ,(second key-type)))
                                                     (subseq list-2 (1+ key-position)))))
                  (and (subsetp key-type-type-pairs-1 key-type-type-pairs-2
                                :test #'type= :key #'first)
                       (loop :for (key-type type-2) :in key-type-type-pairs-1
                             :always (subtypep
                                      type-2
                                      (second (assoc key-type key-type-type-pairs-2
                                                     :test #'type=)))))))))))

(def-test type-list-subtype-required-&-required-key (:suite type-list-subtype-rest)
  (5am:is-true  (type-list-more-specific-p '(string array) '(array string &key)))
  (5am:is-true  (type-list-more-specific-p '(string string) '(string array &key)))
  (5am:is-true  (type-list-more-specific-p '(string (eql :a) string)
                                           '(string &key (:a string))))
  ;; This is allowed:
  ;;   (funcall (lambda (&key a) a) :a 5 :a 6) ;=> 5
  (5am:is-true  (type-list-more-specific-p '(string (eql :a) string (eql :a) string)
                                           '(string &key (:a string))))
  (5am:is-true  (type-list-more-specific-p '(string (eql :a) string (eql :a) number)
                                           '(string &key (:a string))))
  (5am:is-true  (type-list-more-specific-p '(string (eql :b) string)
                                           '(string &key (:a number) (:b string))))
  (5am:is-false (type-list-more-specific-p '(string (eql :a) number (eql :a) string)
                                           '(string &key (:a string))))
  (5am:is-false (type-list-more-specific-p '(string (eql :a) string)
                                           '(string &key (:a number) (:b string))))
  (5am:is-false (type-list-more-specific-p '(string keyword string)
                                           '(string &key (:a number) (:b string))))
  (5am:is-false (type-list-more-specific-p '(string keyword string)
                                           '(string &key (:a string))))
  (5am:is-false (type-list-more-specific-p '(string keyword) '(string &key)))
  (5am:is-false (type-list-more-specific-p '(string) '(string array &key)))
  (5am:is-false (type-list-more-specific-p '(string keyword) '(string &key (:a string))))
  (5am:is-false (type-list-more-specific-p '(string keyword string number)
                                           '(string &key (:a number) (:b string))))
  (5am:is-false (type-list-more-specific-p '(string keyword string)
                                           '(string &key (:a number))))
  (5am:is-false (type-list-more-specific-p '(string string) '(string &key)))
  (5am:is-false (type-list-more-specific-p '(string string) '(string &key (:a string)))))



(5am:def-suite type-list-intersection-null-rest :in type-list-intersection-null-p)

(defmethod %type-list-intersection-null-p
    ((type-1 (eql 'rest)) (type-2 (eql 'rest)) list-1 list-2)
  (let ((rest-position-1 (position '&rest list-1))
        (rest-position-2 (position '&rest list-2)))
    (%type-list-intersection-null-p 'required 'required
                                    (subseq list-1 0 (min rest-position-1 rest-position-2))
                                    (subseq list-2 0 (min rest-position-1 rest-position-2)))))

(def-test type-list-intersection-null-rest-&-rest
    (:suite type-list-intersection-null-rest)
  (5am:is-false (type-list-intersection-null-p '(string string &rest) '(string &rest)))
  (5am:is-false (type-list-intersection-null-p '(string string &rest) '(string array &rest)))
  (5am:is-true  (type-list-intersection-null-p '(string string &rest) '(string number &rest))))

(defmethod %type-list-intersection-null-p
    ((type-1 (eql 'rest)) (type-2 (eql 'required)) list-1 list-2)
  (let ((rest-position (position '&rest  list-1))
        (list-2-length (length list-2)))
    (if (< list-2-length rest-position)
        t
        (%type-list-intersection-null-p 'required 'required
                                        (subseq list-1 0 rest-position)
                                        (subseq list-2 0 rest-position)))))

(def-test type-list-intersection-null-rest-&-required
    (:suite type-list-intersection-null-rest)
  (5am:is-false (type-list-intersection-null-p '(string &rest) '(string)))
  (5am:is-false (type-list-intersection-null-p '(string &rest) '(string array)))
  (5am:is-true  (type-list-intersection-null-p '(string string &rest) '(string)))
  (5am:is-false (type-list-intersection-null-p '(string string &rest) '(string array)))
  (5am:is-true  (type-list-intersection-null-p '(string string &rest) '(string number))))

(defmethod %type-list-intersection-null-p
    ((type-1 (eql 'rest)) (type-2 (eql 'required-key)) list-1 list-2)
  (declare (optimize debug))
  (let ((rest-position (position '&rest list-1))
        (key-position  (position '&key  list-2)))
    (if (>= key-position rest-position)
        (%type-list-intersection-null-p 'required 'required
                                        (subseq list-1 0 rest-position)
                                        (subseq list-2 0 rest-position))
        (or (%type-list-intersection-null-p 'required 'required
                                            (subseq list-1 0 key-position)
                                            (subseq list-2 0 key-position))
            ;; FIXME: This is not known to be looking correct
            ;; Odd positions in the remaining are always occupied by KEYWORD-supertypes
            (not (let ((rest (subseq list-1 key-position rest-position)))
                   (loop :for (name type) :in (subseq list-2 (1+ key-position))
                         :while rest
                         :for type-1 := (first rest)
                         :for type-2 := (second rest)
                         :always (typep name type-1)
                         :do (setq rest (cddr rest)))))
            ;; And even the EVEN positions are such that they be either of the two type-lists
            (let ((rest (subseq list-1 key-position rest-position))
                  (intersection-null t))
              (loop :for (name type) :in (subseq list-2 (1+ key-position))
                    :while (and rest intersection-null)
                    :for type-1 := (first rest)
                    :for type-2 := (second rest)
                    :do (if (or (and (rest rest) ; we haven't exhausted REST yet
                                     (typep name type-1)
                                     (type= type type-2))
                                (and (null (rest rest)) ; we exhausted REST at the type-2
                                     (typep name type-1)))
                            (setq intersection-null nil)))
              intersection-null)))))

;;; Ignore the case of required-optional against all others

(def-test type-list-intersection-null-rest-&-required-key
    (:suite type-list-intersection-null-p)
  (5am:is-false (type-list-intersection-null-p '(string &rest) '(string array &key)))
  (5am:is-false (type-list-intersection-null-p '(string keyword  &rest)
                                               '(string &key (:a string))))
  (5am:is-false (type-list-intersection-null-p '(string keyword string  &rest)
                                               '(string &key (:a string))))
  (5am:is-false (type-list-intersection-null-p '(string keyword string  &rest)
                                               '(string &key (:a number) (:b string))))
  (5am:is-false (type-list-intersection-null-p '(string string &rest) '(string array &key)))
  (5am:is-true  (type-list-intersection-null-p '(string keyword string  &rest)
                                               '(string &key (:a number) (:b number))))
  (5am:is-true  (type-list-intersection-null-p '(string keyword string  &rest)
                                               '(string &key (:a number))))
  (5am:is-true  (type-list-intersection-null-p '(string keyword string number &rest)
                                               '(string &key (:a number) (:b string))))
  (5am:is-true  (type-list-intersection-null-p '(string keyword &rest) '(string &key)))
  (5am:is-true  (type-list-intersection-null-p '(string string  &rest) '(string &key)))
  (5am:is-true  (type-list-intersection-null-p '(string string  &rest)
                                               '(string &key (:a string)))))

(defmethod %type-list-intersection-null-p
    ((type-1 (eql 'required)) (type-2 (eql 'required-key)) list-1 list-2)
  (let ((list-1-length (length list-1))
        (key-position  (position '&key  list-2)))
    (cond ((= key-position list-1-length)
           (%type-list-intersection-null-p 'required 'required
                                           (subseq list-1 0 list-1-length)
                                           (subseq list-2 0 list-1-length)))
          ((< list-1-length key-position)
           t)
          ((< key-position list-1-length)
           (or (not (evenp (- list-1-length key-position)))
               (%type-list-intersection-null-p 'required 'required
                                               (subseq list-1 0 key-position)
                                               (subseq list-2 0 key-position))
               (not
                ;; Odd positions in the remaining are always occupied by KEYWORD-supertypes
                (let ((rest (subseq list-1 key-position list-1-length)))
                  (loop :for (name type) :in (subseq list-2 (1+ key-position))
                        :while rest
                        :for type-1 := (first rest)
                        :for type-2 := (second rest)
                        :always (typep name type-1)
                        :do (setq rest (cddr rest)))))
               ;; And even the EVEN positions are such that they be either of the two type-lists
               (let ((rest (subseq list-1 key-position))
                     (intersection-null t))
                 (loop :for (name type) :in (subseq list-2 (1+ key-position))
                       :while (and rest intersection-null)
                       :for type-1 := (first rest)
                       :for type-2 := (second rest)
                       :do (if (or (and (rest rest) ; we haven't exhausted REST yet
                                        (typep name type-1)
                                        (type= type type-2))
                                   (and (null (rest rest)) ; we exhausted REST at the type-2
                                        (typep name type-1)))
                               (setq intersection-null nil)))
                 intersection-null))))))

(def-test type-list-intersection-null-required-&-required-key
    (:suite type-list-intersection-null-rest)
  (5am:is-false (type-list-intersection-null-p '(string (eql :a) string)
                                               '(string &key (:a string))))
  (5am:is-false (type-list-intersection-null-p '(string keyword string)
                                               '(string &key (:a string))))
  (5am:is-false (type-list-intersection-null-p '(string keyword string)
                                               '(string &key (:a (or null number)) (:b string))))
  (5am:is-true  (type-list-intersection-null-p '(string (eql :b) string)
                                               '(string &key (:a string) (:b number))))
  (5am:is-false (type-list-intersection-null-p '(string string) '(string array &key)))
  (5am:is-true  (type-list-intersection-null-p '(string keyword) '(string &key)))
  (5am:is-true  (type-list-intersection-null-p '(string) '(string array &key)))
  (5am:is-true  (type-list-intersection-null-p '(string keyword) '(string &key (:a string))))
  (5am:is-true  (type-list-intersection-null-p '(string keyword string number)
                                               '(string &key (:a number) (:b string))))
  (5am:is-true  (type-list-intersection-null-p '(string keyword string)
                                               '(string &key (:a number))))
  (5am:is-true  (type-list-intersection-null-p '(string string) '(string &key)))
  (5am:is-true  (type-list-intersection-null-p '(string string) '(string &key (:a string)))))

(defmethod %type-list-intersection-null-p ((type-1 (eql 'required)) (type-2 (eql 'rest)) list-1 list-2)
  (%type-list-intersection-null-p type-2 type-1 list-2 list-1))

(defmethod %type-list-intersection-null-p ((type-1 (eql 'required-key))
                                           (type-2 (eql 'rest))
                                           list-1 list-2)
  (%type-list-intersection-null-p type-2 type-1 list-2 list-1))

(defmethod %type-list-intersection-null-p ((type-1 (eql 'required-key))
                                           (type-2 (eql 'required))
                                           list-1 list-2)
  (%type-list-intersection-null-p type-2 type-1 list-2 list-1))
