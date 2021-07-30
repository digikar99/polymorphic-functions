(in-package :polymorphic-functions)

(5am:def-suite lambda-list :in :polymorphic-functions)

;;; FIXME: Even though this file does a good amount of lambda-list processing
;;; it does not do some things, and not quite validation yet.

(5am:def-suite effective-lambda-list :in lambda-list)

(defun polymorphic-function-make-effective-lambda-list (untyped-lambda-list)
  (check-type untyped-lambda-list untyped-lambda-list)
  (let ((optional-position (position '&optional untyped-lambda-list))
        (keyword-position  (position '&key untyped-lambda-list)))
    (cond ((and (null optional-position)
                (null keyword-position))
           (copy-list untyped-lambda-list))
          ;; FIXME: Should handle default arguments easily
          (optional-position
           (append (subseq untyped-lambda-list 0 optional-position)
                   '(&optional)
                   (mapcar (lambda (elt)
                             (list elt nil (gensym (symbol-name elt))))
                           (subseq untyped-lambda-list (1+ optional-position)))))
          (keyword-position
           (append (subseq untyped-lambda-list 0 keyword-position)
                   (list '&rest (gensym "ARGS") '&key)
                   (mapcar (lambda (elt)
                             (list elt nil (gensym (symbol-name elt))))
                           (subseq untyped-lambda-list (1+ keyword-position)))))
          (t
           (error "Unexpected")))))

(def-test effective-lambda-list-untyped (:suite effective-lambda-list)

  (is (equalp '(a b &optional)
              (polymorphic-function-make-effective-lambda-list '(a b &optional))))
  (is-error (polymorphic-function-make-effective-lambda-list '(a b &optional &rest)))
  (destructuring-bind (first second third fourth)
      (polymorphic-function-make-effective-lambda-list '(a &optional c d))
    (is (eq first 'a))
    (is (eq second '&optional))
    (is (eq 'c (first third)))
    (is (eq 'd (first fourth))))

  (is-error (polymorphic-function-make-effective-lambda-list '(a b &rest args &key)))
  (destructuring-bind (first second third fourth fifth sixth)
      (polymorphic-function-make-effective-lambda-list '(a &key c d))
    (declare (ignore third))
    (is (eq first 'a))
    (is (eq second '&rest))
    (is (eq fourth '&key))
    (is (eq 'c (first fifth)))
    (is (eq 'd (first sixth))))

  (is (equalp '(a b &rest c)
              (polymorphic-function-make-effective-lambda-list '(a b &rest c))))
  (is-error (polymorphic-function-make-effective-lambda-list '(a b &rest)))
  (destructuring-bind (first second third)
      (polymorphic-function-make-effective-lambda-list '(a &rest c))
    (is (eq first 'a))
    (is (eq second '&rest))
    (is (eq 'c third))))

(defun normalize-typed-lambda-list (typed-lambda-list)
  (let ((state           'required)
        (normalized-list ()))
    (dolist (elt typed-lambda-list)
      (if (member elt lambda-list-keywords)
          (progn
            (setq state elt)
            (push elt normalized-list))
          (push (case state
                  (required
                   (if (listp elt)
                       elt
                       (list elt t)))
                  ((&optional &key)
                   (cond ((not (listp elt))
                          (list (list elt t) nil))
                         ((not (listp (first elt)))
                          (list (list (first elt) t)
                                (second elt)))
                         (t elt)))
                  (&rest elt))
                normalized-list)))
    (nreverse normalized-list)))

(def-test normalize-typed-lambda-list (:suite lambda-list)
  (5am:is-true (equalp '((a t))
                       (normalize-typed-lambda-list '(a))))
  (5am:is-true (equalp '(&optional ((a t) nil))
                       (normalize-typed-lambda-list '(&optional a))))
  (5am:is-true (equalp '(&key ((a t) nil))
                       (normalize-typed-lambda-list '(&key a))))
  (5am:is-true (equalp '(&rest a)
                       (normalize-typed-lambda-list '(&rest a)))))

(defun untyped-lambda-list (normalized-typed-lambda-list)
  (let ((typed-lambda-list   normalized-typed-lambda-list)
        (state               'required)
        (untyped-lambda-list ()))
    (dolist (elt typed-lambda-list)
      (if (member elt lambda-list-keywords)
          (progn
            (setq state elt)
            (push elt untyped-lambda-list))
          (push (case state
                  (required (first elt))
                  ((&optional &key) (caar elt))
                  (&rest elt))
                untyped-lambda-list)))
    (nreverse untyped-lambda-list)))

(def-test untyped-lambda-list ()
  (is (equalp '(a)   (untyped-lambda-list '((a string)))))
  (is (equalp '(a b) (untyped-lambda-list '((a string) (b number)))))
  (is (equalp '(&optional c)   (untyped-lambda-list '(&optional ((c string))))))
  (is (equalp '(a &optional c) (untyped-lambda-list '((a number) &optional ((c string))))))
  (is (equalp '(&key c)   (untyped-lambda-list '(&key ((c string))))))
  (is (equalp '(a &key c) (untyped-lambda-list '((a number) &key ((c string))))))
  (is (equalp '(a &rest args) (untyped-lambda-list '((a number) &rest args)))))

(defstruct polymorph-parameters
  required
  optional
  rest
  keyword
  min-args
  max-args
  validator-form)

(defstruct (polymorph-parameter (:conc-name pp-))
  "
LOCAL-NAME : Name inside the body of the polymorph
FORM-IN-PF : The form which yields the parameter's value inside the lexical
  environment of the polymorphic-function

Note: Only LOCAL-NAME and FORM-IN-PF are relevant for &REST parameter
"
  local-name
  form-in-pf
  value-type
  default-value-form
  supplied-p-name
  type-parameters
  value-effective-type)

(defstruct type-parameter
  "
RUN-TIME-DEPARAMETERIZERS-LAMBDA-BODY :
  A lambda *expression*, which when compiled produces a one argument function.
  The function is called at run-time with the value bound to the parameter
  (not type-parameter) to obtain the value of the TYPE-PARAMETER.
COMPILE-TIME-DEPARAMETERIZER-LAMBDA-BODY :
  A lambda *expression*, which when compiled produces a one argument function.
  The function is called at compile-time with the type of the value bound
  to the parameter (not type-parameter) to obtain the value of the TYPE-PARAMETER.
"
  name
  run-time-deparameterizer-lambda-body
  compile-time-deparameterizer-lambda
  compile-time-deparameterizer-lambda-body)

(defun make-polymorph-parameters-from-lambda-lists (polymorphic-function-lambda-list
                                                    polymorph-lambda-list)
  (let ((untyped-lambda-list polymorphic-function-lambda-list)
        (typed-lambda-list   polymorph-lambda-list)
        (untyped-state       :required)
        (typed-state         :required)
        (parameters      (make-polymorph-parameters))
        (rest-idx        0)
        (rest-arg        nil)
        (parameter       nil))
    (declare (optimize debug))
    ;; The length of the typed and untyped lambda list need not match.
    ;; But we are guaranteed that typed-lambda-list is at least as long as
    ;; untyped-lambda-list
    (loop :for parameter-specifier :in typed-lambda-list
          ;; :do (print (list parameter-specifier
          ;;                  (car untyped-lambda-list)
          ;;                  parameter-alist))
          :do (setq untyped-state
                    (case (car untyped-lambda-list)
                      (&optional
                       (setq untyped-lambda-list (cdr untyped-lambda-list))
                       '&optional)
                      (&key
                       (setq untyped-lambda-list (cdr untyped-lambda-list))
                       '&key)
                      (&rest
                       (setq untyped-lambda-list (cdr untyped-lambda-list))
                       (setq rest-arg (car untyped-lambda-list))
                       '&rest)
                      (t untyped-state)))

              (setq typed-state
                    (case parameter-specifier
                      (&optional '&optional)
                      (&key '&key)
                      (&rest '&rest)
                      (t typed-state)))


              (unless (member parameter-specifier lambda-list-keywords)

                (ecase untyped-state
                  (:required
                   (destructuring-bind (name type) parameter-specifier
                     (setq parameter
                           (make-polymorph-parameter :local-name name
                                                     ;; TODO: TYPE-PARAMETERS
                                                     :form-in-pf (car untyped-lambda-list)
                                                     :value-type type
                                                     :value-effective-type type))
                     (setq untyped-lambda-list (cdr untyped-lambda-list))))
                  (&optional
                   (destructuring-bind ((name type) &optional default supplied-p-name)
                       parameter-specifier
                     (setq parameter
                           (make-polymorph-parameter :local-name name
                                                     :form-in-pf (car untyped-lambda-list)
                                                     :value-type type
                                                     :default-value-form default
                                                     :value-effective-type
                                                     (if default
                                                         `(or null ,type)
                                                         type)
                                                     :supplied-p-name supplied-p-name))
                     (setq untyped-lambda-list (cdr untyped-lambda-list))))
                  (&rest
                   (if (symbolp parameter-specifier)
                       (progn
                         (setq parameter
                               (make-polymorph-parameter :local-name parameter-specifier
                                                         :form-in-pf
                                                         `(nthcdr ,rest-idx ,rest-arg)))
                         (incf rest-idx))
                       (if (listp (car parameter-specifier))
                           ;; LIST for &KEY parameter-specifier
                           (destructuring-bind ((name type) &optional default supplied-p-name)
                               parameter-specifier
                             (setq parameter
                                   (make-polymorph-parameter :local-name name
                                                             :form-in-pf
                                                             `(ignore-errors
                                                               (getf
                                                                (nthcdr ,rest-idx ,rest-arg)
                                                                ,(intern (symbol-name name)
                                                                         :keyword)))
                                                             :default-value-form default
                                                             :value-type type
                                                             ;; only REQUIRED, KEY or REST
                                                             :value-effective-type
                                                             (if default
                                                                 `(or null ,type)
                                                                 type)
                                                             :supplied-p-name supplied-p-name)))
                           (destructuring-bind (name type) parameter-specifier
                             (setq parameter
                                   (make-polymorph-parameter :local-name name
                                                             :form-in-pf
                                                             `(nth ,rest-idx ,rest-arg)
                                                             :value-type type
                                                             ;; only REQUIRED, KEY or REST
                                                             :value-effective-type type))
                             (incf rest-idx))))
                   (setq untyped-lambda-list (cdr untyped-lambda-list)))
                  (&key
                   (destructuring-bind ((name type) &optional default supplied-p-name)
                       parameter-specifier
                     (setq parameter
                           (make-polymorph-parameter :local-name name
                                                     :form-in-pf (car untyped-lambda-list)
                                                     :value-type type
                                                     :default-value-form default
                                                     :value-effective-type
                                                     (if default
                                                         `(or null ,type)
                                                         type)
                                                     :supplied-p-name supplied-p-name))
                     (setq untyped-lambda-list (cdr untyped-lambda-list)))))

                (setf (pp-type-parameters parameter)
                      (type-parameters-from-parametric-type (pp-value-type parameter)))

                (ecase typed-state
                  (:required (push parameter (polymorph-parameters-required parameters)))
                  (&optional (push parameter (polymorph-parameters-optional parameters)))
                  (&key      (push parameter (polymorph-parameters-keyword parameters)))
                  ;; May be we should avoid list for &REST ?
                  (&rest     (push parameter (polymorph-parameters-rest parameters))))))

    (nreversef (polymorph-parameters-required parameters))
    (nreversef (polymorph-parameters-optional parameters))
    (setf (polymorph-parameters-keyword parameters)
          (sort (polymorph-parameters-keyword parameters)
                #'string< :key #'pp-local-name))

    (let* ((min-args (length (polymorph-parameters-required parameters)))
           (max-args (cond ((polymorph-parameters-rest parameters)
                            nil)
                           (t
                            (+ min-args
                               (length (polymorph-parameters-optional parameters))
                               (* 2 (length (polymorph-parameters-keyword parameters))))))))
      (setf (polymorph-parameters-min-args parameters) min-args)
      (setf (polymorph-parameters-max-args parameters) max-args)
      (when (and (member '&rest polymorphic-function-lambda-list)
                 (not (member '&key polymorphic-function-lambda-list)))
        (setq rest-arg (or rest-arg
                           (nth (1+ (position '&rest polymorphic-function-lambda-list))
                                polymorphic-function-lambda-list)))
        (setf (polymorph-parameters-validator-form parameters)
              (let ((num-required (position '&rest polymorphic-function-lambda-list)))
                `(and (<= ,(- min-args num-required)
                          (length ,rest-arg)
                          ,(- (or max-args lambda-parameters-limit)
                              num-required))
                      ,(if (polymorph-parameters-keyword parameters)
                           `(evenp (length (nthcdr ,rest-idx ,rest-arg)))
                           t))))))

    parameters))

(defun map-polymorph-parameters (polymorph-parameters
                                 &key required optional keyword rest)
  (declare (type (or null function) required optional keyword rest))
  (let ((required-fn required)
        (optional-fn optional)
        (keyword-fn  keyword)
        (rest-fn     rest))
    (with-slots (required optional keyword rest) polymorph-parameters
      (remove-if #'null
                 (append (when required-fn (mapcar required-fn required))
                         (when optional '(&optional))
                         (when optional-fn (mapcar optional-fn optional))
                         (when rest '(&rest))
                         (when rest-fn (mapcar rest-fn rest))
                         (when keyword '(&key))
                         (when keyword-fn (mapcar keyword-fn keyword)))))))

(defun polymorph-effective-lambda-list (polymorph-parameters)
  "Returns 3 values:
- The first value is the LAMBDA-LIST suitable for constructing polymorph's lambda
- The second value is the TYPE-LIST corresponding to the polymorph
- The third value is the EFFECTIVE-TYPE-LIST corresponding to the polymorph"
  (values (let ((type-parameter-name-deparameterizer-list))
            (flet ((populate-type-parameters (pp)
                     (loop :for tp :in (pp-type-parameters pp)
                           :do (with-slots (name
                                            run-time-deparameterizer-lambda-body)
                                   tp
                                 (unless (assoc-value type-parameter-name-deparameterizer-list
                                                      name)
                                   (push `(,name
                                           (,run-time-deparameterizer-lambda-body
                                            ,(pp-local-name pp)))
                                         type-parameter-name-deparameterizer-list))))))
              (append
               (map-polymorph-parameters
                polymorph-parameters
                :required
                (lambda (pp)
                  (populate-type-parameters pp)
                  (pp-local-name pp))
                :optional
                (lambda (pp)
                  (populate-type-parameters pp)
                  (with-slots (local-name default-value-form supplied-p-name)
                      pp
                    (if supplied-p-name
                        (list local-name default-value-form supplied-p-name)
                        (list local-name default-value-form))))
                :rest #'pp-local-name
                :keyword
                (lambda (pp)
                  (populate-type-parameters pp)
                  (with-slots (local-name default-value-form supplied-p-name)
                      pp
                    (if supplied-p-name
                        (list local-name default-value-form supplied-p-name)
                        (list local-name default-value-form)))))
               '(&aux)
               type-parameter-name-deparameterizer-list)))
          (map-polymorph-parameters polymorph-parameters
                                    :required #'pp-value-type
                                    :optional #'pp-value-type
                                    :rest (lambda (arg)
                                            (declare (ignore arg))
                                            nil)
                                    :keyword (lambda (pp)
                                               (with-slots (local-name value-type) pp
                                                 (list (intern (symbol-name local-name) :keyword)
                                                       value-type))))
          (map-polymorph-parameters polymorph-parameters
                                    :required #'pp-value-effective-type
                                    :optional #'pp-value-effective-type
                                    :rest (lambda (arg)
                                            (declare (ignore arg))
                                            nil)
                                    :keyword (lambda (pp)
                                               (with-slots (local-name value-effective-type) pp
                                                 (list (intern (symbol-name local-name) :keyword)
                                                       value-effective-type))))))

(defun lambda-declarations (polymorph-parameters)
  (let ((type-parameter-names ()))
    (flet ((type-decl (pp)
             (with-slots (local-name value-type) pp
               (loop :for tp :in (pp-type-parameters pp)
                     :do (pushnew (type-parameter-name tp) type-parameter-names))
               (let ((value-type (deparameterize-type value-type)))
                 (cond ((type-specifier-p value-type)
                        `(type ,value-type ,local-name))
                       (t
                        `(type ,(upgrade-extended-type value-type) ,local-name)))))))
      `(declare ,@(set-difference (map-polymorph-parameters polymorph-parameters
                                                            :required #'type-decl
                                                            :optional #'type-decl
                                                            :keyword  #'type-decl
                                                            :rest (lambda (pp)
                                                                    (declare (ignore pp))
                                                                    nil))
                                  lambda-list-keywords)
                (ignorable ,@type-parameter-names)))))



(defun compiler-applicable-p-lambda-body (polymorph-parameters)

  ;; TODO: Handle parametric-types

  (let ((type-parameters-alist ()))

    (with-gensyms (form form-type)
      (flet ((app-p-form (param pp)
               (let ((type (pp-value-type pp))
                     (type-parameters (pp-type-parameters pp)))
                 `(let ((,form      (car ,param))
                        (,form-type (cdr ,param)))
                    (cond ((eq t ',type)
                           t)
                          ((eq t ,form-type)
                           (signal 'form-type-failure
                                   :form ,form))
                          (t
                           ,(if type-parameters
                                (loop :for type-parameter :in type-parameters
                                      :do (with-slots (name
                                                       compile-time-deparameterizer-lambda-body)
                                              type-parameter
                                            (push `(,compile-time-deparameterizer-lambda-body
                                                    (cdr ,param))
                                                  (assoc-value type-parameters-alist name)))
                                      :finally (return t))
                                `(subtypep ,form-type ',type))))))))

        (let* ((lambda-list
                 (map-polymorph-parameters polymorph-parameters
                                           :required
                                           (lambda (pp)
                                             (gensym (write-to-string (pp-local-name pp))))
                                           :optional
                                           (lambda (pp)
                                             (list (gensym (write-to-string (pp-local-name pp)))
                                                   nil
                                                   (gensym (concatenate 'string
                                                                        (write-to-string
                                                                         (pp-local-name pp))
                                                                        "-SUPPLIED-P"))))
                                           :keyword
                                           (lambda (pp)
                                             (list (intern (symbol-name (pp-local-name pp))
                                                           :polymorphic-functions)
                                                   nil))
                                           :rest
                                           (lambda (pp)
                                             (gensym (write-to-string (pp-local-name pp))))))

               (lambda-body-forms
                 (let ((ll lambda-list))
                   (map-polymorph-parameters
                    polymorph-parameters
                    :required
                    (lambda (pp)
                      (let ((form (app-p-form (car ll) pp)))
                        (setq ll (cdr ll))
                        form))
                    :optional
                    (lambda (pp)
                      (loop :while (member (car ll) lambda-list-keywords)
                            :do (setq ll (cdr ll)))
                      (let ((form `(or (not ,(third (car ll)))
                                       ,(app-p-form (first (car ll))
                                                    pp))))
                        (setq ll (cdr ll))
                        form))
                    :keyword
                    (lambda (pp)
                      (loop :while (member (car ll) lambda-list-keywords)
                            :do (setq ll (cdr ll)))
                      (let ((form (app-p-form (first (car ll)) pp)))
                        (setq ll (cdr ll))
                        form))
                    :rest
                    (lambda (pp)
                      (declare (ignore pp))
                      nil)))))

          `(lambda ,lambda-list
             (declare (optimize speed)
                      (ignorable ,@(mapcar (lambda (elt)
                                             (etypecase elt
                                               (atom elt)
                                               (list (first elt))))
                                           (set-difference lambda-list lambda-list-keywords))))
             (and ,@(set-difference lambda-body-forms lambda-list-keywords)
                  ,@(loop :for (param . forms) :in type-parameters-alist
                          :collect `(and ,@forms
                                         ,@(loop :for form :in (rest forms)
                                                 :collect `(equalp ,(first forms)
                                                                   ,form)))))))))))


(defun run-time-applicable-p-form (polymorph-parameters)
  (let ((type-parameter-alist ()))
    (flet ((process (pp)
             (if-let (type-parameters (pp-type-parameters pp))
               (loop :for type-parameter :in type-parameters
                     :do (with-slots (name run-time-deparameterizer-lambda-body)
                             type-parameter
                           (push `(,run-time-deparameterizer-lambda-body
                                   ,(pp-form-in-pf pp))
                                 (assoc-value type-parameter-alist name)))
                     :finally (return nil))
               (with-slots (form-in-pf value-effective-type) pp
                 `(typep ,form-in-pf ',value-effective-type)))))
      (let ((type-forms
              (map-polymorph-parameters polymorph-parameters
                                        :required #'process
                                        :optional #'process
                                        :keyword #'process
                                        :rest
                                        (lambda (pp)
                                          (declare (ignore pp))
                                          nil))))
        `(and ,(or (polymorph-parameters-validator-form polymorph-parameters)
                   t)
              ,@(set-difference type-forms lambda-list-keywords)
              ,@(loop :for (name . forms) :in type-parameter-alist
                      :collect `(and ,@(loop :for form :in (rest forms)
                                             :collect `(equalp ,(first forms)
                                                               ,form)))))))))



(defun enhanced-lambda-declarations (polymorph-parameters arg-types &optional return-type)

  (let* ((deparameterizer-alist  ())
         (processed-for-keyword-arguments nil))

    (flet ((populate-deparameterizer-alist (pp arg-type)
             (when-let (type-parameters (pp-type-parameters pp))
               (let* ((type-parameter-names (mapcar #'type-parameter-name type-parameters)))
                 (loop :for name :in type-parameter-names
                       :do (unless (assoc-value deparameterizer-alist name)
                             (setf (assoc-value deparameterizer-alist name)
                                   (funcall
                                    (type-parameter-compile-time-deparameterizer-lambda
                                     (find name type-parameters :key #'type-parameter-name))
                                    arg-type))))))))

      (let ((type-forms
              (map-polymorph-parameters
               polymorph-parameters
               :required
               (lambda (pp)
                 (let ((arg-type  (car arg-types)))
                   (setq arg-types (cdr arg-types))
                   (populate-deparameterizer-alist pp arg-type)
                   `(type ,arg-type ,(pp-local-name pp))))
               :optional
               (lambda (pp)
                 (let ((arg-type (car arg-types)))
                   (setq arg-types (cdr arg-types))
                   (populate-deparameterizer-alist pp arg-type)
                   `(type ,(or arg-type (pp-value-type pp))
                          ,(pp-local-name pp))))
               :keyword
               (lambda (pp)
                 (unless processed-for-keyword-arguments
                   (setq processed-for-keyword-arguments t)
                   (setq arg-types
                         (loop :for i :from 0
                               :for arg-type :in arg-types
                               :if (evenp i)
                                 :collect
                                 (progn
                                   (assert (and (member (first arg-type)
                                                        '(member eql))
                                                (= 2 (length arg-type)))
                                           ()
                                           ;; FIXME: Something better than generic error?
                                           "Unable to derive keyword from arg-type ~S"
                                           arg-type)
                                   (second arg-type))
                               :else
                                 :collect arg-type)))
                 (let ((arg-type (getf arg-types
                                       (intern (symbol-name (pp-local-name pp)) :keyword))))
                   (populate-deparameterizer-alist pp arg-type)
                   `(type ,(or arg-type (pp-value-type pp))
                          ,(pp-local-name pp))))
               :rest
               (lambda (pp)
                 (declare (ignore pp))
                 nil))))

        (values `(declare ,@(set-difference type-forms lambda-list-keywords)
                          (ignorable ,@(mapcar #'car deparameterizer-alist)))
                (translate-body return-type deparameterizer-alist))))))

(defun accepts-argument-of-type-p (polymorph-parameters type)
  (flet ((%subtypep (pp) (subtypep type (pp-value-type pp))))
    (some (lambda (arg)
            (eq t arg))
          (map-polymorph-parameters polymorph-parameters
                                    :required #'%subtypep
                                    :optional #'%subtypep
                                    :keyword  #'%subtypep))))
