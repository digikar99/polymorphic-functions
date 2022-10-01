(in-package :polymorphic-functions)

(define-condition return-type-mismatch (condition)
  ((actual   :initarg :actual)
   (index    :initarg :index)))

(define-condition return-type-mismatch/warning (return-type-mismatch warning)
  ((declared :initarg :declared))
  (:report (lambda (c s)
             (with-slots (declared actual index) c
               (if (member index '(1 2 3))
                   (format s (concatenate
                              'string
                              (ecase index
                                (1 "1st")
                                (2 "2nd")
                                (3 "3rd"))
                              " derived return-type (0-indexed) is~%  ~S~%not of declared type~%  ~S")
                           actual declared)
                   (format s
                           "~Sth derived return-type (0-indexed) is~%  ~S~%not of declared type~%  ~S"
                           index actual declared))))))

(define-condition return-type-mismatch/error (return-type-mismatch error)
  ((expected :initarg :expected))
  (:report (lambda (c s)
             (with-slots (expected actual index) c
               (if (member index '(1 2 3))
                   (format s (concatenate
                              'string
                              (ecase index
                                (1 "1st")
                                (2 "2nd")
                                (3 "3rd"))
                              " return-value (0-indexed) is~%  ~S~%not of expected type~%  ~S")
                           actual expected)
                   (format s
                           "~Sth return-value (0-indexed) is~%  ~S~%not of expected type~%  ~S"
                           index actual expected))))))


(define-condition return-type-count-mismatch (condition)
  ((min :initarg :min)
   (max :initarg :max)
   (actual :initarg :actual))
  ;; TODO: Put this to use
  (:report (lambda (c s)
             (if (slot-boundp c 'max)
                 (with-slots (min max actual) c
                   (format s "Expected at least ~S and at most ~S return-values but received ~S"
                           min max actual))
                 (with-slots (min actual) c
                   (format s "Expected at least ~S return-values but received ~S"
                           min actual))))))

(define-condition return-type-count-mismatch/warning (return-type-count-mismatch warning)
  ())

(defun ensure-type-form (type form &optional env)
  "Returns two values: a form that has ASSERTs with SIMPLE-TYPE-ERROR to check the type
as well as the type enhanced using TYPE."
  (declare (optimize debug))
  (let* ((type (cond ((parametric-type-specifier-p type)
                      type)
                     (t
                      (typexpand type env))))
         (optional-position (when (listp type)
                              (position '&optional type)))
         (min-values (cond (optional-position
                            (1- optional-position))
                           ((atom type)
                            1)
                           ((eq 'values (first type))
                            (1- (length type)))
                           (t
                            1)))
         (rest-supplied-p (when (listp type)
                            (member '&rest type)))
         (rest-p (if optional-position
                     (member '&rest type)
                     t))
         (type-forms (if (and (listp type)
                              (eq 'values (first type)))
                         (remove '&rest
                                 (remove '&optional
                                         (rest type)))
                         (list type)))

         (types (loop :for type :in type-forms
                      :collect (deparameterize-type type)))
         (rest-type (if rest-supplied-p
                        (lastcar types)
                        t))
         (types (if rest-supplied-p
                    (butlast types)
                    types))

         (type-forms (loop :for type :in type-forms
                           :collect
                           (if (not (parametric-type-specifier-p type))
                               `(quote ,type)
                               ;; Given a PARAMETRIC-TYPE, we want to traverse it and create a form suitable
                               ;; to be supplied as the second argument to TYPEP
                               (traverse-tree type
                                              (lambda (node)
                                                (etypecase node
                                                  (atom (if (or (parametric-type-symbol-p node)
                                                                (member node '(quote list)))
                                                            node
                                                            (values `(quote ,node) t)))
                                                  (list (if (member (car node) '(quote list))
                                                            node
                                                            `(list ,@node)))))))))
         (rest-type-form (if rest-supplied-p
                             (lastcar type-forms)
                             t))
         (type-forms (if rest-supplied-p
                         (butlast type-forms)
                         type-forms))
         (num-types (length type-forms))
         (form-values (make-gensym-list (length type-forms) "FORM-VALUE"))
         (form-value-list (gensym "FORM-VALUE-LIST"))
         (num-values (gensym "NUM-VALUES"))

         (form-types (let ((may-be-list (form-type form env
                                                   :expand-compiler-macros t
                                                   :constant-eql-types t)))
                       (if (and (listp may-be-list)
                                (eql 'values (first may-be-list)))
                           (remove '&optional (rest may-be-list))
                           (list may-be-list))))
         (form-rest-type (if (member '&rest form-types)
                             (lastcar form-types)
                             nil))
         (form-types (if (member '&rest form-types)
                         (butlast form-types 2)
                         form-types))
         (num-form-types (length form-types))

         (ensure-type-forms
           (loop :for form-value :in form-values
                 :for form-type :in form-types
                 :for type :in type-forms
                 :for compiler-type :in types
                 :for i :from 0
                 :do (multiple-value-bind (nilp knownp)
                         (subtypep `(and ,form-type ,compiler-type) nil env)
                       (when (and (not (type= t form-type))
                                  knownp
                                  nilp)
                         (warn 'return-type-mismatch/warning
                               :index i :actual form-type :declared compiler-type)))
                 :collect
                 `(when ,(cond ((< i min-values)
                                t)
                               (t
                                `(< ,min-values ,num-values)))
                    (assert (typep ,form-value ,type)
                            (,form-value)
                            'return-type-mismatch/error
                            :index ,i
                            :actual ,form-value
                            :expected ,type)))))


    (values


     `(let* ((,form-value-list (multiple-value-list ,form))
             (,num-values (length ,form-value-list)))
        (declare (ignorable ,num-values))

        ,@(let ((return-type-count-form
                  (cond ((and optional-position rest-p)
                         (when (and (not (<= (1- optional-position)
                                             num-form-types))
                                    (not form-rest-type))
                           (warn 'return-type-count-mismatch/warning
                                 :min min-values :actual num-form-types))
                         `(assert (<= ,(1- optional-position)
                                      ,num-values)
                                  (,form-value-list)
                                  'return-type-count-mismatch :min ,min-values :actual ,num-values))
                        ((and optional-position (not rest-p))
                         (when (and (not (<= (1- optional-position)
                                             num-form-types
                                             num-types))
                                    (not form-rest-type))
                           (warn 'return-type-count-mismatch/warning
                                 :min min-values :max num-types :actual num-form-types))
                         `(assert (<= ,(1- optional-position)
                                      ,num-values
                                      ,num-types)
                                  (,form-value-list)
                                  'return-type-count-mismatch
                                  :min ,min-values :max ,num-types :actual ,num-values))))

                (rest-type-form
                  (unless (eq rest-type-form t)
                    (loop :for form-type :in (nthcdr num-types form-types)
                          :for i :from num-types
                          :do (multiple-value-bind (nilp knownp)
                                  (subtypep `(and ,form-type ,rest-type) nil env)
                                (when (and (not (type= t form-type))
                                           knownp
                                           nilp)
                                  (warn 'return-type-mismatch/warning
                                        :index i :actual form-type :declared rest-type)))
                          :finally
                             (multiple-value-bind (subtypep knownp)
                                 (subtypep form-rest-type rest-type)
                               (when (and (not (type= t form-type))
                                          knownp
                                          (not subtypep))
                                 (warn 'return-type-mismatch/warning
                                       :index i :actual form-rest-type :declared rest-type))))
                    (with-gensyms (value i)
                      `(loop :for ,value :in (nthcdr ,num-types ,form-value-list)
                             :for ,i :from ,num-types
                             :do (assert (typep ,value ,rest-type-form)
                                         (,form-value-list)
                                         'return-type-mismatch/error
                                         :expected ,rest-type-form
                                         :actual ,value
                                         :index ,i))))))
            (list return-type-count-form
                  rest-type-form))

        (multiple-value-bind ,form-values
            (values-list ,form-value-list)
          (declare (ignorable ,@form-values))
          ,@ensure-type-forms)

        (values-list ,form-value-list))

     (form-type `(the ,(deparameterize-type type) ,form) env :expand-compiler-macros t))))
