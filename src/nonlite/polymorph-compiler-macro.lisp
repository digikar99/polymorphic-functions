(in-package :polymorphic-functions)

(defun compiler-retrieve-polymorph (name &rest arg-types-alist)
  (declare (type function-name name))
  (assert *compiler-macro-expanding-p*)
  ;; This function is used by the main compiler macro of the polymorphic-function
  ;; The RETRIEVE-POLYMORPH-FORM below is a complementary to this function.
  (let* ((apf        (fdefinition name))
         (polymorphs (polymorphic-function-polymorphs apf))
         (num-args   (length arg-types-alist)))
    (declare (optimize debug))
    (loop :for polymorph :in polymorphs
          :for lambda-list-type := (polymorph-lambda-list-type polymorph)
          :for type-list := (polymorph-type-list polymorph)
          :for app-p-lambda := (polymorph-compiler-applicable-p-lambda polymorph)
          :do (when (block app-p-lambda
                      (case lambda-list-type
                        (required
                         (if (= num-args (length type-list))
                             (apply app-p-lambda arg-types-alist)
                             nil))
                        (required-optional
                         (if (<= (position '&optional type-list)
                                 num-args
                                 (1- (length type-list)))
                             (apply app-p-lambda arg-types-alist)
                             nil))
                        (required-key
                         (let ((key-pos (position '&key type-list)))
                           (if (and (evenp (- num-args key-pos))
                                    (<= key-pos
                                        num-args
                                        (+ key-pos (* 2 (- (length type-list) key-pos 1)))))
                               (apply app-p-lambda
                                      (loop :for (arg . arg-type) :in arg-types-alist
                                            :for idx :from 0
                                            :with keyword-start := key-pos
                                            :if (and (>= idx keyword-start)
                                                     (evenp (- idx keyword-start)))
                                              :collect (if (and (listp arg-type)
                                                                ;; FIXME: Use CTYPE
                                                                (member (first arg-type)
                                                                        '(eql member))
                                                                (null (cddr arg-type)))
                                                           (second arg-type)
                                                           (return-from app-p-lambda nil))
                                            :else
                                              :collect (cons arg arg-type)))
                               nil)))
                        (rest
                         (if (<= (position '&rest type-list)
                                 num-args)
                             (apply app-p-lambda arg-types-alist)
                             nil))))
                (return-from compiler-retrieve-polymorph polymorph)))))

(defun register-polymorph-compiler-macro (name type-list lambda &optional source)
  (declare (type function-name name)
           (type type-list type-list)
           (type function lambda))
  ;; TODO: Comment why this became impossible
  (assert (find-polymorph name type-list)
          ()
          "Illegal to have a POLYMORPH-COMPILER-MACRO without a corresponding POLYMORPH")
  (let* ((apf              (fdefinition name))
         (lambda-list      (polymorphic-function-effective-lambda-list apf))
         (lambda-list-type (polymorphic-function-lambda-list-type apf))
         (type-list   (type-list-order-keywords type-list)))
    (if (eq lambda-list-type 'rest)
        ;; required-optional can simply be split up into multiple required or required-key
        (assert (not (member '&optional type-list))
                nil
                "&OPTIONAL keyword is not allowed for LAMBDA-LIST~%  ~S~%of the POLYMORPHIC-FUNCTION associated with ~S"
                lambda-list name)
        (assert (type-list-compatible-p lambda-list-type type-list lambda-list)
                nil
                "TYPE-LIST ~S is not compatible with the LAMBDA-LIST ~S of the POLYMORPHs associated with ~S"
                type-list lambda-list name))
    ;; FIXME: How should we account for EFFECTIVE-TYPE-LIST here?
    (ensure-unambiguous-call name type-list type-list)
    (let ((polymorph (find type-list (polymorphic-function-polymorphs apf)
                           :test #'equalp
                           :key #'polymorph-type-list)))
      (setf (polymorph-compiler-macro-lambda polymorph) lambda)
      (setf (polymorph-compiler-macro-source polymorph) source))))

(defun retrieve-polymorph-compiler-macro (name &rest arg-list)
  (declare (type function-name name))
  (let* ((apf                  (fdefinition name))
         (polymorphs           (polymorphic-function-polymorphs apf))
         (type-lists           (polymorphic-function-type-lists apf))
         (apf-lambda-list-type (polymorphic-function-lambda-list-type apf))
         (applicable-polymorphs
           (loop :for polymorph :in polymorphs
                 :if (if (eq 'rest apf-lambda-list-type)
                         (ignore-errors
                          (apply
                           (the function
                                (polymorph-compiler-applicable-p-lambda polymorph))
                           arg-list))
                         (apply
                          (the function
                               (polymorph-compiler-applicable-p-lambda polymorph))
                          arg-list))
                   :collect polymorph)))
    (case (length applicable-polymorphs)
      (1 (polymorph-compiler-macro-lambda (first applicable-polymorphs)))
      (0 (error 'no-applicable-polymorph/error
                :arg-list arg-list :type-lists type-lists))
      (t (error "Multiple applicable POLYMORPHs discovered for ARG-LIST ~S:~%~{~S~^    ~%~}"
                arg-list
                (mapcar #'polymorph-type-list applicable-polymorphs))))))


(defmacro defpolymorph-compiler-macro (name type-list compiler-macro-lambda-list
                                       &body body)
  "Example TYPE-LISTs:
  (NUMBER NUMBER)
  (STRING &OPTIONAL INTEGER)
  (STRING &KEY (:ARG INTEGER))
  (NUMBER &REST)"
  (declare (type function-name name)
           (type type-list type-list))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-polymorph-compiler-macro
        ',name ',type-list
        (compile nil (parse-compiler-macro ',(if (and (listp name)
                                                      (eq 'setf (first name)))
                                                 (second name)
                                                 name)
                                           ',compiler-macro-lambda-list
                                           ',body))
        #+sbcl (sb-c:source-location))
       ',name))

(define-declaration type-like (vars env)
  ;; FIXME: Consequences of emitting CL:TYPE declaration are undefined
  ;; On CCL, args starts with DECL-NAME while not using CL-ENVIRONMENTS-CL
  ;; Other times, it starts with the appropriate args
  (destructuring-bind (original &rest similar) (optima:match vars
                                                 ((list* 'type-like vars)
                                                  vars)
                                                 (_ vars))
    (values :variable
            (loop :with type
                    := (rest (assoc 'cl:type
                                    (nth-value 2 (variable-information original env))))
                  :for var :in similar
                  :collect `(,var cl:type ,type)))))

(define-declaration inline-pf (vars env)
  (values :function
          (loop :for var :in vars
                :collect `(,var inline-pf inline-pf))))

(define-declaration notinline-pf (vars env)
  (values :function
          (loop :for var :in vars
                :collect `(,var inline-pf notinline-pf))))

(define-declaration pf-defined-before-use (args)
  #+sbcl (declare (ignore args))
  (values :declare
          (cons 'pf-defined-before-use t)))

(define-declaration not-pf-defined-before-use (args)
  #+sbcl (declare (ignore args))
  (values :declare
          (cons 'pf-defined-before-use nil)))
