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

(defun env-speed (environment)
  (second (assoc 'speed (declaration-information 'optimize environment))))

(defun env-debug (environment)
  (second (assoc 'debug (declaration-information 'optimize environment))))

(defun env-safety (environment)
  (second (assoc 'safety (declaration-information 'optimize environment))))

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
