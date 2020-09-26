(in-package typed-dispatch)

;; In this file, our main functions/macros are
;; DEFINE-LAMBDA-LIST-HELPER
;; - LAMBDA-LIST-TYPE
;;   - %LAMBDA-LIST-TYPE
;; - PROCESS-FOR-DEFUN
;;   - %PROCESS-FOR-DEFUN
;; - PROCESS-FOR-CALL

;; THE BASICS ==================================================================

(define-constant +lambda-list-types+
    (list 'required
          'required-optional
          'required-key)
  :test #'equalp)

(defun lambda-list-type-p (object)
  "Checks whhether the OBJECT is in +LAMBDA-LIST-TYPES+"
  (member object +lambda-list-types+))

(deftype lambda-list-type () `(satisfies lambda-list-type-p))

(5am:def-suite lambda-list :in :typed-dispatch)

(defun potential-type-of-lambda-list (lambda-list)
  ;; "potential" because it does not check the symbols
  (declare (type list lambda-list))
  (the lambda-list-type
       (let ((intersection (intersection lambda-list lambda-list-keywords)))
         ;; premature optimization and over-abstraction:/
         (cond ((null intersection)
                'required)
               ((and (car intersection) (null (cdr intersection))
                     (member '&optional intersection))
                'required-optional)
               ((and (car intersection) (null (cdr intersection))
                     (member '&key intersection))
                'required-key)
               (t
                (error "Neither of ~A types" +lambda-list-types+))))))

(defvar *potential-type*)
(setf (documentation '*potential-type* 'variable)
      "POTENTIAL-TYPE of the LAMBDA-LIST of the typed function being compiled.
Bound inside the functions defined by TYPED-DISPATCH::DEFINE-LAMBDA-LIST-HELPER")

(defvar *lambda-list*)
(setf (documentation '*lambda-list* 'variable)
      "LAMBDA-LIST of the typed function being compiled. Bound inside the functions
defined by TYPED-DISPATCH::DEFINE-LAMBDA-LIST-HELPER")

(defvar *name*)
(setf (documentation '*name* 'variable)
      "NAME of the typed function being compiled. Bound during the expansion of
DEFINE-TYPED-FUNCTION")

(defmacro define-lambda-list-helper ((outer-name outer-documentation)
                                     (inner-name inner-documentation)
                                     &body action-form)
  "ACTION-FORM should be defined in terms of *POTENTIAL-TYPE* and *LAMBDA-LIST* variables."
  `(progn
     (defun ,outer-name (lambda-list)
       ,outer-documentation
       (declare (type list lambda-list))
       (let ((*potential-type* (potential-type-of-lambda-list lambda-list))
             (*lambda-list*    lambda-list))
         (if (%lambda-list-type *potential-type* lambda-list)
             (progn ,@action-form)
             (error "LAMBDA-LIST ~A is neither of ~%  ~A" lambda-list +lambda-list-types+))))
     (defgeneric ,inner-name (potential-lambda-list-type lambda-list)
       (:documentation ,inner-documentation))
     ;; For better error reporting
     (defmethod ,inner-name ((type t) (lambda-list t))
       (assert (typep type 'lambda-list-type)
               ()
               "Expected POTENTIAL-LAMBDA-LIST-TYPE to be one of ~%  ~A~%but is ~A"
               +lambda-list-types+ type)
       (assert (typep lambda-list 'list)
               ()
               "Expected LAMBDA-LIST to be a LIST but is ~A"
               lambda-list)
       (error "No potential type found for LAMBDA-LIST ~A from amongst ~%  ~A"
              lambda-list +lambda-list-types+))))

(define-constant +lambda-list-type-doc+
  "Returns the type of LAMBDA-LIST from amongst +LAMBDA-LIST-TYPES+.
Raises an ERROR otherwise."
  :test 'string=)

(define-lambda-list-helper
    (lambda-list-type  #.+lambda-list-type-doc+)
    (%lambda-list-type "Checks whether LAMBDA-LIST is of type POTENTIAL-LAMBDA-LIST-TYPE")
  *potential-type*)

(def-test type-identification (:suite lambda-list)
  (is (eq 'required (lambda-list-type '(a b))))
  (is-error (lambda-list-type '(a 5)))
  (is-error (lambda-list-type '(a b &rest))))

;; %LAMBDA-LIST-TYPE ===========================================================

(defmethod %lambda-list-type ((type (eql 'required)) (lambda-list list))
  (every (lambda (elt)
           (and (symbolp elt)
                (not (member elt lambda-list-keywords))))
         lambda-list))

(defmethod %lambda-list-type ((type (eql 'required-optional)) (lambda-list list))
  (let ((state :required))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&optional)
                          (setf state '&optional))
                         ((and (symbolp elt)
                               (not (member elt lambda-list-keywords)))
                          t)
                         (t
                          (return-from %lambda-list-type nil))))
        (&optional (cond ((and (symbolp elt)
                               (not (member elt lambda-list-keywords)))
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
  (is-error (lambda-list-type '(a &optional b &rest))))

(defmethod %lambda-list-type ((type (eql 'required-key)) (lambda-list list))
  (let ((state :required))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&key)
                          (setf state '&key))
                         ((and (symbolp elt)
                               (not (member elt lambda-list-keywords)))
                          t)
                         (t
                          (return-from %lambda-list-type nil))))
        (&key (cond ((and (symbolp elt)
                          (not (member elt lambda-list-keywords)))
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
  (is-error (lambda-list-type '(a &rest c &key b))))

;; PROCESS-FOR-DEFUN ===========================================================

(define-constant +process-for-defun-doc+
  "Processes LAMBDA-LIST to return another lambda-list suitable for the DEFUN generated by the DEFINE-TYPED-FUNCTION. Raises an error if %LAMBDA-LIST-TYPE fails on *POTENTIAL-TYPE*."
  :test 'string=)

(define-constant +process-for-defun-doc-helper+
  "Processes LAMBDA-LIST assuming it is of type TYPE, and returns another lambda-list that is suitable for the DEFUN generated by the DEFINE-TYPED-FUNCTION."
  :test 'string=)

(define-lambda-list-helper
    (process-for-defun  #.+process-for-defun-doc+)
    (%process-for-defun #.+process-for-defun-doc-helper+)
  (%process-for-defun *potential-type* *lambda-list*))

(defmethod %process-for-defun ((type (eql 'required)) (lambda-list list))
  (copy-list lambda-list))

(5am:def-suite process-for-defun :in lambda-list)

(defmethod %process-for-defun ((type (eql 'required-optional)) (lambda-list list))
  (let ((state       :required)
        (return-list ()))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&optional)
                          (push '&optional return-list)
                          (setf state '&optional))
                         ((and (symbolp elt)
                               (not (member elt lambda-list-keywords)))
                          (push elt return-list))
                         (t
                          (return-from %process-for-defun nil))))
        (&optional (cond ((and (symbolp elt)
                               (not (member elt lambda-list-keywords)))
                          (push (list elt nil (gensym (symbol-name elt)))
                                return-list))
                         (t
                          (return-from %process-for-defun nil))))))
    (nreverse return-list)))

(def-test process-for-defun-optional (:suite process-for-defun)
  (is (equalp '(a b &optional)
              (process-for-defun '(a b &optional))))
  (5am:is-true (destructuring-bind (first second third fourth)
                   (process-for-defun '(a &optional c d))
                 (and (eq first 'a)
                      (eq second '&optional)
                      (eq 'c (first third))
                      (eq 'd (first fourth))))))

(defmethod %process-for-defun ((type (eql 'required-key)) (lambda-list list))
  (let ((state       :required)
        (return-list ()))
    (dolist (elt lambda-list)
      (ecase state
        (:required (cond ((eq elt '&key)
                          (push '&key return-list)
                          (setf state '&key))
                         ((and (symbolp elt)
                               (not (member elt lambda-list-keywords)))
                          (push elt return-list))
                         (t
                          (return-from %process-for-defun nil))))
        (&key (cond ((and (symbolp elt)
                          (not (member elt lambda-list-keywords)))
                     (push (list elt nil (gensym (symbol-name elt)))
                           return-list))
                    (t
                     (return-from %process-for-defun nil))))))
    (nreverse return-list)))

(def-test process-for-defun-key (:suite process-for-defun)
  (is (equalp '(a b &key)
              (process-for-defun '(a b &key))))
  (5am:is-true (destructuring-bind (first second third fourth)
                   (process-for-defun '(a &key c d))
                 (and (eq first 'a)
                      (eq second '&key)
                      (eq 'c (first third))
                      (eq 'd (first fourth))))))

;; PROCESS-FOR-CALL =========================================================

(define-constant +process-for-call-doc+
  "Processes LAMBDA-LIST to return code suitable for the caller part
inside the DEFUN generated by the DEFINE-TYPED-FUNCTION. Raises an error if
%LAMBDA-LIST-TYPE fails on *POTENTIAL-TYPE*."
  :test 'string=)

(define-constant +process-for-call-doc-helper+
  "Processes LAMBDA-LIST assuming it is of type TYPE, and returns code that is
is suitable for caller part inside DEFUN generated by the DEFINE-TYPED-FUNCTION."
  :test 'string=)

(define-lambda-list-helper
    (process-for-call  #.+process-for-call-doc+)
    (%process-for-call #.+process-for-call-doc-helper+)
  (let ((defun-lambda-list (%process-for-defun *potential-type* *lambda-list*)))
    (values (%process-for-call *potential-type* defun-lambda-list)
            defun-lambda-list)))

(defmethod %process-for-call ((type (eql 'required)) (defun-lambda-list list))
  `(funcall (nth-value 1 (retrieve-typed-function ',*name* ,@defun-lambda-list))
            ,@defun-lambda-list))

(defmethod %process-for-call ((type (eql 'required-optional)) (defun-lambda-list list))
  (let ((state       :required)
        (return-list ()))
    (loop :for elt := (first defun-lambda-list)
          :until (eq elt '&optional)
          :do (unless (and (symbolp elt)
                           (not (member elt lambda-list-keywords)))
                (return-from %process-for-call nil))
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
          (values `(let ((apply-list ,optional-p-tree))
                     (apply (nth-value 1 (apply 'retrieve-typed-function
                                                ',*name*
                                                ,@(reverse return-list)
                                                apply-list))
                            ,@(reverse return-list)
                            apply-list))
                  defun-lambda-list))))))

(defmethod %process-for-call ((type (eql 'required-key)) (defun-lambda-list list))
  (let ((state       :required)
        (return-list ()))
    (loop :for elt := (first defun-lambda-list)
          :until (eq elt '&key)
          :do (unless (and (symbolp elt)
                           (not (member elt lambda-list-keywords)))
                (return-from %process-for-call nil))
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



(defun valid-parameter-name-p (name)
  (and (symbolp name)
       (not (eq t name))
       (not (eq nil name))))

;; &key args would be stored in the form of a plist - that is: the cdr of the type-list
;; following the &key word should be a plist

(defun typed-lambda-list-p (list)
  "Returns five values:
  the first value indicates whether the given LIST is a TYPED-LAMBDA-LIST,
  the second is a UNTYPED-LAMBDA-LIST corresponding to the given LIST,
  the third is a LIST of TYPEs corresponding to the given LIST,
  the fourth is a LIST of PARAMETERs corresponding to the third value,
  the fifth is the processed lambda-list suitable for defining the lambda inside DEFUN-TYPED"
  (declare (type list list))
  (let ((typed-lambda-list-p   t)
        (untyped-lambda-list nil)
        (type-list           nil)
        (typed-param-list    nil)
        (processed-lambda-list nil))
    (when (or (intersection list (set-difference lambda-list-keywords '(&optional &key)))
              (and (member '&optional list)
                   (member '&key list)))
      (return-from typed-lambda-list-p nil))
    (macrolet ((update-with-and (obj)
                 `(if typed-lambda-list-p
                      (setq typed-lambda-list-p
                            (and typed-lambda-list-p ,obj))))
               (next (list)
                 `(setq ,list (cdr ,list))))
      ;; first process required args
      (loop :while (and typed-lambda-list-p list)
            :for param := (first list)
            :until (member param lambda-list-keywords)
            :do (update-with-and (listp param))
                (update-with-and (valid-parameter-name-p (first param)))
                (update-with-and (type-specifier-p (second param)))
                (update-with-and (null (cddr param))) ; max-length 2
                (when typed-lambda-list-p
                  (push (first param) untyped-lambda-list)
                  (push (first param) typed-param-list)
                  (push (first param) processed-lambda-list)
                  (push (second param) type-list))
                (next list))
      ;; optionally the &optional args
      (loop :for keyword :in '(&optional &key) :do
        ;; TODO: Processing &allow-other-keys and &rest
        (when (eq keyword (first list))
          (push keyword untyped-lambda-list)
          (push keyword processed-lambda-list)
          (push keyword type-list) ; require the &optional keyword in RETRIEVE-TYPED-FUNCTION
          (next list)
          (loop :while (and typed-lambda-list-p list)
                :for param := (first list)
                :until (member param lambda-list-keywords)
                :do (update-with-and (listp param))
                    (update-with-and (listp (first param)))
                    (update-with-and (null (cdddr param))) ; max-length 3
                    (update-with-and (destructuring-bind ((name type)
                                                          &optional default-value
                                                            (given-p nil given-p-p))
                                         param
                                       (if given-p-p
                                           (push (list name default-value given-p)
                                                 processed-lambda-list)
                                           (push (list name default-value)
                                                 processed-lambda-list))
                                       (and (valid-parameter-name-p name)
                                            (type-specifier-p type)
                                            (if given-p-p
                                                (valid-parameter-name-p given-p)
                                                t))))
                    (when typed-lambda-list-p
                      (push (first (first param))  untyped-lambda-list)
                      (push (first (first param))  typed-param-list)
                      (when (eq '&key keyword)
                        (push (intern (symbol-name (first (first param)))
                                      :keyword)
                              type-list))
                      (push (second (first param)) type-list))
                    (next list))))
      (update-with-and (handler-case (progn
                                       (parse-ordinary-lambda-list list)
                                       t)
                         (program-error (c)
                           (declare (ignore c))
                           nil)))
      (when typed-lambda-list-p
        (setq untyped-lambda-list
              (append (nreverse untyped-lambda-list)
                      list))
        (setq processed-lambda-list
              (append (nreverse processed-lambda-list)
                      list))))
    (check-type untyped-lambda-list untyped-lambda-list)
    (values typed-lambda-list-p
            untyped-lambda-list
            (nreverse type-list)
            (nreverse typed-param-list)
            processed-lambda-list)))

(deftype typed-lambda-list ()
  "Examples:
  ((a integer) (b integer))
  ((a integer) &optional ((b integer) 0 b-supplied-p))"
  `(satisfies typed-lambda-list-p))

(defun untyped-lambda-list-p (list)
  "Returns three values:
  the first value indicates if the given LIST is an UNTYPED-LAMBDA-LIST,
  if the first value is T, that is, if the LIST is an UNTYPED-LAMBDA-LIST, the second value is the list of PARAMETERs that will be considered for typed dispatch
  the third value is the processed typed lambda-list; in particular, it adds an \"argp\" to the &optional and &key args, to help the DEFINE-TYPED-FUNCTION produce the correct function. In the absence of both &optional and &key args, this is the same as the given LIST whenever LIST is a valid UNTYPED-LAMBDA-LIST"
  (multiple-value-bind () (parse-ordinary-lambda-list )))

(defun untyped-lambda-list-p (list)
  "Returns three values:
  the first value indicates if the given LIST is an UNTYPED-LAMBDA-LIST,
  if the first value is T, that is, if the LIST is an UNTYPED-LAMBDA-LIST, the second value is the list of PARAMETERs that will be considered for typed dispatch
  the third value is the processed typed lambda-list; in particular, it adds an \"argp\" to the &optional and &key args, to help the DEFINE-TYPED-FUNCTION produce the correct function. In the absence of both &optional and &key args, this is the same as the given LIST whenever LIST is a valid UNTYPED-LAMBDA-LIST"
  (declare (type list list))
  (let ((untyped-lambda-list-p t)
        (typed-param-list    nil)
        (processed-untyped-lambda-list nil))
    (when (or (intersection list (set-difference lambda-list-keywords '(&optional &key)))
              (and (member '&optional list)
                   (member '&key list)))
      (return-from untyped-lambda-list-p nil))
    (macrolet ((update-with-and (obj)
                 `(if untyped-lambda-list-p
                      (setq untyped-lambda-list-p
                            (and untyped-lambda-list-p ,obj))))
               (next (list)
                 `(setq ,list (cdr ,list))))
      ;; first process required args
      (loop :while (and untyped-lambda-list-p list)
            :for param := (first list)
            :until (member param lambda-list-keywords)
            :do (update-with-and (and (symbolp param)
                                      (not (eq t param))))
                (when untyped-lambda-list-p
                  (push param typed-param-list)
                  (push param processed-untyped-lambda-list))
                (next list))
      ;; optionally the &optional args
      (loop :for keyword :in '(&optional &key) :do
        (when (eq keyword (first list))
          (push keyword processed-untyped-lambda-list)
          (next list)
          (loop :while (and untyped-lambda-list-p list)
                :for param := (first list)
                :until (member param lambda-list-keywords)
                :for param-list-p := (listp param)
                :for param-symbol := (etypecase param
                                       (symbol param)
                                       (list (first param)))
                :do (update-with-and (valid-parameter-name-p param-symbol))
                    (when untyped-lambda-list-p
                      (let ((optional-param (list param-symbol
                                                  nil
                                                  (gensym (symbol-name param-symbol)))))
                        (push (if param-list-p
                                  param
                                  optional-param)
                              typed-param-list)
                        (push (if param-list-p
                                  param
                                  optional-param)
                              processed-untyped-lambda-list)))
                    (next list))))
      (update-with-and (handler-case (progn
                                       (parse-ordinary-lambda-list list)
                                       t)
                         (program-error (c)
                           (declare (ignore c))
                           nil)))
      (setq processed-untyped-lambda-list
            (append (nreverse processed-untyped-lambda-list) list)))
    (values untyped-lambda-list-p
            (nreverse typed-param-list)
            processed-untyped-lambda-list)))

(deftype untyped-lambda-list ()
  "Examples:
  (a b)
  (a b &optional c)
Non-examples:
  ((a string))"
  `(satisfies untyped-lambda-list-p))

