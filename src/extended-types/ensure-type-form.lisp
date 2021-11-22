(in-package :polymorphic-functions)

(defun ensure-type-form (type form &optional env)
  (declare (optimize debug))
  (let* ((type (typexpand type env))
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
         (types (if (and (listp type)
                         (eq 'values (first type)))
                    (remove '&rest
                            (remove '&optional
                                    (rest type)))
                    (list type)))

         (compiler-types (loop :for type :in types
                               :collect (deparameterize-type type)))
         (compiler-rest-type (if rest-supplied-p
                                 (lastcar compiler-types)
                                 t))
         (compiler-types (if rest-supplied-p
                             (butlast compiler-types)
                             compiler-types))
         
         (types (loop :for type :in types
                      :collect
                      (if (not (parametric-type-specifier-p type))
                          `(quote ,type)
                          ;; Given a PARAMETRIC-TYPE, we want to traverse it and create a form suitable
                          ;; to be supplied as the second argument to TYPEP
                          (traverse-tree type
                                         (lambda (node)
                                           (etypecase node
                                             (atom (if (parametric-type-symbol-p node)
                                                       node
                                                       `(quote ,node)))
                                             (list `(list ,@node))))))))
         (rest-type (if rest-supplied-p
                        (lastcar types)
                        t))
         (types (if rest-supplied-p
                    (butlast types)
                    types))
         (num-types (length types))
         (form-values (make-gensym-list (length types) "FORM-VALUE"))
         (form-value-list (gensym "FORM-VALUE-LIST"))
         (num-values (gensym "NUM-VALUES"))

         (ensure-type-forms
           (loop :for form-value :in form-values
                 :for type :in types
                 :for i :from 0
                 :collect
                 `(when ,(cond ((< i min-values)
                                t)
                               (t
                                `(< ,min-values ,num-values)))
                    (assert (typep ,form-value ,type)
                            (,form-value)
                            'simple-type-error
                            :format-control
                            ,(if (member i '(1 2 3))
                                 (concatenate
                                  'string
                                  (ecase i
                                    (1 "1st")
                                    (2 "2nd")
                                    (3 "3rd"))
                                  " return-value (0-indexed) is~%  ~S~%not of expected type~%  ~S")
                                 "~Sth return-value (0-indexed) is~%  ~S~%not of expected type~%  ~S")
                            :format-arguments
                            ,(if (member i '(1 2 3))
                                 `(list ,form-value ,type)
                                 `(list ,i ,form-value ,type))))))

         (compiler-form-types (let ((may-be-list (cl-form-types:form-type form env)))
                                (if (and (listp may-be-list)
                                         (eql 'values (first may-be-list)))
                                    (remove '&optional (rest may-be-list))
                                    (list may-be-list))))
         (compiler-rest-form-type (if (member '&rest compiler-form-types)
                                      (lastcar compiler-form-types)
                                      nil))
         (compiler-form-types (if (member '&rest compiler-form-types)
                                  (butlast compiler-form-types 2)
                                  compiler-form-types)))

    (flet ((check-compiler-type (i compiler-type compiler-form-type)
             (multiple-value-bind (subtypep knownp)
                 (subtypep compiler-form-type compiler-type)
               (when (and knownp
                          (not subtypep))
                 (if (member i '(1 2 3))
                     (warn (concatenate
                            'string
                            (ecase i
                              (1 "1st")
                              (2 "2nd")
                              (3 "3rd"))
                            " return-value (0-indexed) is~%  ~S~%not of expected type~%  ~S")
                           compiler-form-type compiler-type)
                     (warn 
                      "~Sth return-value (0-indexed) is~%  ~S~%not of expected type~%  ~S"
                      i compiler-form-type compiler-type))))))
      (loop :for compiler-form-type :in compiler-form-types
            :for compiler-type :in compiler-types
            :for i :from 0
            :do (check-compiler-type i compiler-type compiler-form-type)
            :finally
               (loop :for compiler-form-type :in (nthcdr num-types compiler-form-types)
                     :for i :from i
                     :do (check-compiler-type i compiler-rest-type compiler-form-type)
                     :finally (check-compiler-type i compiler-rest-type
                                                   compiler-rest-form-type))))

    `(let* ((,form-value-list (multiple-value-list ,form))
            (,num-values (length ,form-value-list)))
       (declare (ignorable ,num-values))
       ,@(cond ((and optional-position rest-p)
                `((assert (<= ,(1- optional-position)
                              ,num-values)
                          (,form-value-list)
                          'simple-type-error
                          :format-control
                          "Expected at least ~S return-values but received ~S"
                          :format-arguments (list ,min-values
                                                  ,num-values))
                  ,(unless (eq rest-type t)
                     `(assert (every (lambda (value)
                                       (typep value ,rest-type))
                                     (nthcdr ,num-types ,form-value-list))
                              (,form-value-list)
                              'simple-type-error
                              :format-control
                              "&REST return-values~%  ~{~^~&  ~S~}~%are not of expected type~%  ~S"
                              :format-arguments (list (nthcdr ,num-types ,form-value-list)
                                                      ,rest-type)))))
               ((and optional-position (not rest-p))
                `((assert (<= ,(1- optional-position)
                              ,num-values
                              ,num-types)
                          (,form-value-list)
                          'simple-type-error
                          :format-control
                          "Expected at least ~S and at most ~S return-values but received ~S"
                          :format-arguments (list ,min-values
                                                  ,num-types
                                                  ,num-values))))
               (t
                `(,(unless (eq rest-type t)
                     `(assert (every (lambda (value)
                                       (typep value ,rest-type))
                                     (nthcdr ,num-types ,form-value-list))
                              (,form-value-list)
                              'simple-type-error
                              :format-control
                              "&REST return-values~{~^~&  ~S~}~%are not of expected type~%  ~S"
                              :format-arguments (list (nthcdr ,num-types ,form-value-list)
                                                      ,rest-type))))))
       (multiple-value-bind ,form-values
           (values-list ,form-value-list)
         ,@ensure-type-forms)
       (values-list ,form-value-list))))
