
(polymorphic-functions.defpackage:defpackage :polymorphic-functions.extended-types
  #+extensible-compound-types
  (:use :extensible-compound-types-cl)
  #-extensible-compound-types
  (:use :cl :ctype)
  #-extensible-compound-types
  (:import-from :ctype
                #:cons-specifier-ctype
                #:ctype)
  (:intern #:*extended-type-specifiers*
           #:upgraded-extended-type)
  (:shadow #:ctype=
           #:extended-type-specifier-p
           #:type-specifier-p
           #:supertypep
           #:subtypep
           #:typep
           #:type=)
  (:export #:extended-type-specifier-p
           #:cl-type-specifier-p
           #:type-specifier-p
           #:upgrade-extended-type
           #:supertypep
           #:subtypep
           #:typep
           #:type=

           #:*subtypep-alist*
           #:*extended-subtypep-functions*
           #:subtypep-not-knowo
           #:definitive-subtypep
           #:type-pair-=

           ))

(defpackage #:polymorphic-functions.nonuser
  (:use)
  (:documentation
   "Package for internal use by POLYMORPHIC-FUNCTIONS not intended for direct use by users."))

(polymorphic-functions.defpackage:defpackage :polymorphic-functions
  (:shadowing-import-exported-symbols :polymorphic-functions.extended-types)
  #+extensible-compound-types
  (:use :cl-form-types :alexandria :extensible-compound-types-cl)
  #-extensible-compound-types
  (:use :cl-form-types :alexandria :cl)
  #-extensible-compound-types
  (:import-from :ctype
                #:ctype
                #:cons-specifier-ctype)
  (:import-from :5am #:is #:def-test)
  (:import-from :polymorphic-functions.extended-types
                #:*extended-type-specifiers*
                #:upgraded-extended-type)
  (:import-from :introspect-environment
                #:compiler-macroexpand
                #:constant-form-value
                #:parse-compiler-macro)
  (:import-from :cl-environments.cltl2
                #:function-information
                #:variable-information
                #:declaration-information
                #:define-declaration
                #:augment-environment)
  (:export #:define-polymorphic-function
           #:undefine-polymorphic-function
           #:defpolymorph
           #:defpolymorph-compiler-macro
           #:undefpolymorph
           #:find-polymorph
           #:polymorph-apropos-list-type

           ;; Unstable API
           #:polymorphic-function
           #:polymorph
           #:no-applicable-polymorph
           #:polymorphic-function-type-lists
           #:inline-pf
           #:notinline-pf
           #:pf-defined-before-use
           #:not-pf-defined-before-use
           #:*compiler-macro-expanding-p*
           #:*disable-static-dispatch*

           #:*parametric-type-symbol-predicates*
           #:parametric-type-run-time-lambda-body
           #:parametric-type-compile-time-lambda-body

           #:%deparameterize-type

           #:suboptimal-polymorph-note
           #:more-optimal-polymorph-inapplicable))

(in-package :polymorphic-functions)

(5am:def-suite :polymorphic-functions)

(defmacro catch-condition (form)
  `(handler-case ,form
     (condition (condition) condition)))

(defmacro is-error (form)
  `(5am:signals error ,form))

(defmacro list-named-lambda (name package lambda-list &body body &environment env)
  (declare (type list name)
           (ignorable env package))
  #+sbcl
  `(sb-int:named-lambda ,name ,lambda-list
     ,@body)
  #+ccl
  `(ccl:nfunction ,name
                  #+extensible-compound-types
                  (cl:lambda ,@(rest (macroexpand-1 `(lambda ,lambda-list ,@body) env)))
                  #-extensible-compound-types
                  (cl:lambda ,lambda-list ,@body))
  #-(or sbcl ccl)
  (let ((function-name (intern (write-to-string name) package)))
    `(flet ((,function-name ,lambda-list ,@body))
       #',function-name)))

(define-symbol-macro optim-safety (= 3 (policy-quality 'safety env)))

(define-symbol-macro optim-debug (or (= 3 (policy-quality 'debug env))
                                     (> (policy-quality 'debug env)
                                        (policy-quality 'speed env))))
(define-symbol-macro optim-speed (and (/= 3 (policy-quality 'debug env))
                                      (= 3 (policy-quality 'speed env))))
(define-symbol-macro optim-slight-speed (and (/= 3 (policy-quality 'debug env))
                                             (/= 3 (policy-quality 'speed env))
                                             (<= (policy-quality 'debug env)
                                                 (policy-quality 'speed env))))

#-extensible-compound-types
(defun typexpand (type-specifier &optional env)
  (if (cl-type-specifier-p type-specifier)
      (ctype::typexpand type-specifier env)
      type-specifier))

(defun policy-quality (quality &optional env)
  (second (assoc quality (declaration-information 'optimize env))))

(defun macroexpand-all (form &optional env)
  (cl-form-types.walker:walk-form (lambda (form env)
                                    (optima:match form
                                      ((list* name _)
                                       (cond ((listp name)
                                              form)
                                             ((compiler-macro-function name env)
                                              (funcall (compiler-macro-function name env)
                                                       form
                                                       env))
                                             (t
                                              form)))
                                      (_
                                       form)))
                                  form
                                  env))

(defun cl-type-specifier-p (type-specifier)
  "Returns true if TYPE-SPECIFIER is a valid type specfiier."
  (block nil
    #+sbcl (return (ignore-some-conditions (sb-kernel:parse-unknown-type)
                     (sb-ext:valid-type-specifier-p type-specifier)))
    #+openmcl (return (ccl:type-specifier-p type-specifier))
    #+ecl (return (c::valid-type-specifier type-specifier))
    #+clisp (return (null
                     (nth-value 1 (ignore-errors
                                   (ext:type-expand type-specifier)))))
    (or (when (symbolp type-specifier)
          (documentation type-specifier 'type))
        (error "TYPE-SPECIFIER-P not available for this implementation"))))

(defun setf-function-name-p (object)
  (and (listp object)
       (null (cddr object))
       (eq 'setf (car object))
       (symbolp (cadr object))))

(deftype function-name ()
  ;; Doesn't work great with subtypep
  "Ref: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#function_name"
  `(or symbol (satisfies setf-function-name-p)))
