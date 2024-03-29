(in-package #:polymorphic-functions)

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

(defmacro with-return-type (return-type &body body)
  ;; We put the &BODY only for good indentation
  (declare (optimize debug))
  (assert (null (cdr body)))
  (let* ((form (car body))
         (type (typexpand return-type))
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

         (type-forms (loop :for type :in type-forms
                           :collect `(quote ,type)))
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

         (ensure-type-forms
           (loop :for form-value :in form-values
                 :for type :in type-forms
                 :for i :from 0
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


    `(let* ((,form-value-list (multiple-value-list ,form))
            (,num-values (length ,form-value-list)))
       (declare (ignorable ,num-values))

       ,@(let ((return-type-count-form
                 (cond ((and optional-position rest-p)
                        `(assert (<= ,(1- optional-position)
                                     ,num-values)
                                 (,form-value-list)
                                 'return-type-count-mismatch :min ,min-values :actual ,num-values))
                       ((and optional-position (not rest-p))
                        `(assert (<= ,(1- optional-position)
                                     ,num-values
                                     ,num-types)
                                 (,form-value-list)
                                 'return-type-count-mismatch
                                 :min ,min-values :max ,num-types :actual ,num-values))))

               (rest-type-form
                 (unless (eq rest-type-form t)
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

       (values-list ,form-value-list))))

(defun ensure-type-form (type block-name body &key variable declare)
  "Returns two values: a form that has ASSERTs with SIMPLE-TYPE-ERROR to check the type
as well as the type enhanced using TYPE."
  (declare (optimize debug))
  (if (macro-function 'with-return-type-in-env)
      (values `(with-return-type-in-env (:variable ,variable :declare ,declare)
                                        ,type
                 (block ,block-name (locally ,@body)))
              (uiop:symbol-call '#:polymorphic-functions '#:form-type
                                `(the ,type (block ,block-name (locally ,@body)))
                                (uiop:symbol-call '#:cl-environments
                                                  '#:augment-environment
                                                  nil
                                                  :variable variable
                                                  :declare declare)
                         :expand-compiler-macros t))
      (values `(with-return-type ,type
                 (block ,block-name (locally ,@body)))
              type)))
