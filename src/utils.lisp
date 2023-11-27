(in-package #:polymorphic-functions)

(defun expand-macros-and-non-cl-compiler-macros (form env)
  ;; If the second return value is T, then the returned forms
  ;; are not walked further by CL-FORM-TYPES.WALKER:WALK-FORM.
  ;; In other words, return T to signal a "stop expansion".
  (optima:match form
    ((list* name _)
     (cond ((listp name)
            form)
           ((and (compiler-macro-function name env)
                 (not (eq (find-package :cl)
                          (symbol-package name))))
            (funcall (compiler-macro-function name env)
                     form
                     env))
           (t
            form)))
    (_
     form)))

(defun macroexpand-all (form &optional env)
  (cl-form-types.walker:walk-form 'expand-macros-and-non-cl-compiler-macros
                                  form
                                  env))

(defmacro catch-condition (form)
  `(handler-case ,form
     (condition (condition) condition)))

(defmacro is-error (form)
  `(5am:signals error ,form))

(defmacro named-lambda (name lambda-list &body body)
  `(flet ((,name ,lambda-list ,@body))
     (function ,name)))

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

(defmacro with-eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

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

(defun policy-quality (quality &optional env)
  (second (assoc quality (declaration-information 'optimize env))))

(defun env-speed (environment)
  (second (assoc 'speed (declaration-information 'optimize environment))))

(defun env-debug (environment)
  (second (assoc 'debug (declaration-information 'optimize environment))))

(defun env-safety (environment)
  (second (assoc 'safety (declaration-information 'optimize environment))))

(defun type-specifier-p (type-specifier)
  "Returns true if TYPE-SPECIFIER is a valid type specfiier."
  (block nil
    #+sbcl (return (ignore-some-conditions (sb-kernel:parse-unknown-type)
                     (sb-ext:valid-type-specifier-p type-specifier)))
    #+openmcl (return (ccl:type-specifier-p type-specifier))
    #+ecl (return (c::valid-type-specifier type-specifier))
    #+clisp (return (null
                     (nth-value 1 (ignore-errors
                                   (ext:type-expand type-specifier)))))
    #-(or sbcl openmcl ecl lisp)
    (or (when (symbolp type-specifier)
          (documentation type-specifier 'type))
        (error "TYPE-SPECIFIER-P not available for this implementation"))))

(defun form-type (form env &key (return-default-type t)
                             expand-compiler-macros constant-eql-types)
  (or (ignore-errors
       (handler-bind ((cl-form-types:unknown-special-operator
                        (lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'cl-form-types:return-default-type
                                          return-default-type))))
         (cl-form-types:form-type form env
                                  :expand-compiler-macros expand-compiler-macros
                                  :constant-eql-types constant-eql-types)))
      t))

(defun nth-form-type (form env n
                      &optional
                        constant-eql-types expand-compiler-macros (return-default-type t))
  (or (ignore-errors
       (handler-bind ((cl-form-types:unknown-special-operator
                        (lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'cl-form-types:return-default-type
                                          return-default-type))))
         (cl-form-types:nth-form-type form env n constant-eql-types expand-compiler-macros)))
      t))

(defun find-class (name &optional errorp environment)
  #-sbcl
  (if errorp
      (cl:find-class name t environment)
      (ignore-errors (cl:find-class name nil environment)))
  #+sbcl
  (cl:find-class name errorp environment))

(defun traverse-tree (tree &optional (function #'identity))
  "Traverses TREE and calls function on each subtree and node of TREE.
If FUNCTION returns a list, then traversing the list can be avoided if
the second return value is non-NIL. If FUNCTION returns a list, traverses
the list only if the second return value is NIL."
  (multiple-value-bind (new-tree traversal-complete-p)
      (funcall function tree)
    (if (and (proper-list-p new-tree)
             (not traversal-complete-p))
        (loop :for node :in new-tree
              :collect (traverse-tree node function))
        (funcall function new-tree))))

(defun values-subtypep (type1 type2 &optional env)
  (loop :for i :from 0
        :for t1 := (cl-form-types:nth-value-type type1 i)
        :for t2 := (cl-form-types:nth-value-type type2 i)
        :while (or t1 t2)
        :always (subtypep t1 t2 env)))
