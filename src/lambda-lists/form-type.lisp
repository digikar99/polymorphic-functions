(in-package :adhoc-polymorphic-functions)

(5am:def-suite form-type :in :adhoc-polymorphic-functions)
(5am:in-suite form-type)

(defun maybe-constant-form-value (form &optional env default)
  (if (constantp form env)
      (values (constant-form-value form env) t)
      (values default nil)))

;;; We are currently not using SANDALPHON.COMPILER-MACRO:FORM-TYPE (in system "compiler-macro")
;;; because it returns a single value, and apparantly does not provide us with a way to
;;; distinguish between if the type of a form is T vs not-declared.
;;;
;;; The distinction is necessary if we intend to perform safety-checks;
;;; we want to let the user know:
;;; "the type is not declared" vs "a polymorph with these types has not been defined"

(defun form-type (form &optional environment)
  "Returns two values: the first value is the TYPE of FORM if the second value is T"
  (cond ((constantp form environment)
         (values `(eql ,(constant-form-value form environment))
                 t))
        ((symbolp form)
         (values (introspect-environment:variable-type form environment)
                 (cdr (assoc 'type
                             (nth-value 2
                                        (variable-information form environment))))))
        ((listp form)
         (values (let ((first (first form)))
                   (or (%form-type first form)
                       (cond ((typep first 'function-name)
                              (cond ((macro-function first environment)
                                     (form-type (macroexpand form environment)
                                                environment))
                                    ((compiler-macro-function first environment)
                                     (let ((expansion
                                             (let ((*error-output* (make-string-output-stream)))
                                               (compiler-macroexpand form environment))))
                                       (if (equalp form expansion)
                                           (signal 'form-type-failure :form form)
                                           (form-type expansion environment))))
                                    ((special-operator-p first)
                                     (cond ((eq 'the first)
                                            (second form))
                                           (t (signal 'form-type-failure :form form))))
                                    (t (signal 'form-type-failure :form form))))
                             ((eq 'lambda (first first))
                              (let ((the-form (first (last first))))
                                (if (and (listp the-form)
                                         (eq 'the (first the-form)))
                                    (second the-form)
                                    (signal 'form-type-failure :form form))))
                             (t (signal 'form-type-failure :form form)))))
                 t))
        (t
         (error "~%We shouldn't have reached here!"))))

;;; Preferably avoid adding implementation specific parts here.
;;; On SBCL, the implementation specific part is essentially the DEFTRANSFORM's.

(defgeneric %form-type (first form))

(defmethod %form-type (first form) nil)

(defmethod %form-type ((first (eql 'function)) form)
  (introspect-environment:function-type (second form) *environment*))

(defmethod %form-type ((first (eql 'the)) form)
  (second form))

(defmethod %form-type ((first (eql 'make-instance)) form)
  (maybe-constant-form-value (second form) *environment* t))

(defmethod %form-type ((first (eql 'make-array)) form)
  (let* ((dimensions (let ((dim-form (second form)))
                       (if (constantp dim-form *environment*)
                           (ensure-list (constant-form-value dim-form *environment*))
                           'cl:*)))
         ;; Make sure that a keyword argument does not
         ;; come from a non-constant form, for example:
         ;;   (make-array 2 (identity :displaced-to) #(0 0 0))
         (constant-key-forms-p (loop :for i :from 0
                                     :for sub-form :in (nthcdr 2 form)
                                     ;; :do (print (list i sub-form
                                     ;;                  (constantp sub-form *environment*)))
                                     :always (or (oddp i)
                                                 (constantp sub-form *environment*)))))
    (if (not constant-key-forms-p)
        `(array * ,dimensions)
        (let* ((element-type (if-let (key-pos (position :element-type form
                                                        :key (rcurry #'maybe-constant-form-value
                                                                     *environment*)))
                               (let ((elt-form (elt form (1+ key-pos))))
                                 (maybe-constant-form-value elt-form *environment* 'cl:*))
                               't))
               
               ;; Assume that we can know if or not the array is adjustable

               (adjustable-p-p t)
               (adjustable-p   (if-let (key-pos (position :adjustable form))
                                 (let ((value-form (elt form (+ 1 key-pos))))
                                   (if (constantp value-form *environment*)
                                       ;; It may be simple if it is not adjustable
                                       (constant-form-value value-form *environment*)
                                       ;; In this case, we do not even know if or
                                       ;; not the array is adjustable
                                       (setq adjustable-p-p nil)))
                                 nil))
               (fill-pointer-p (find :fill-pointer form :key (rcurry #'maybe-constant-form-value
                                                                     *environment*)))
               (displaced-p    (intersection '(:displaced-index-offset :displaced-to)
                                             form :key (rcurry #'maybe-constant-form-value
                                                               *environment*))))

          (let ((simple-p (if (null adjustable-p-p)
                              nil ; we cannot tell if or not the array is adjustable
                              (and (null adjustable-p)
                                   (null fill-pointer-p)
                                   (null displaced-p)
                                   ))))

            (if simple-p
                `(simple-array ,element-type ,dimensions)
                `(array ,element-type ,dimensions)))))))

(def-test make-array ()
  (macrolet ((with-null-env (&body body)
               `(let ((*environment* nil))
                  ,@body)))
    (is (equalp '(array * (3)) (with-null-env (form-type '(make-array 3 (first a) #(0 0 0))))))
    (is (equalp '(simple-array t (3))
                (with-null-env (form-type '(make-array 3 :initial-contents #(0 0 0))))))
    (is (equalp '(simple-array t *)
                (with-null-env (form-type '(make-array a :initial-contents #(0 0 0))))))
    (is (equalp '(array t *)
                (with-null-env (form-type '(make-array a :displaced-to #(0 0 0))))))))
