(in-package #:polymorphic-functions)

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

(defmacro with-eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

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

(deftype function-name ()
  `(or (and symbol (not (member t nil)))
       (cons (eql setf)
             (cons (and symbol (not (member t nil)))
                   null))))

(defmacro let+ (bindings &body body)
  (if (null bindings)
      `(locally ,@body)
      (optima:ematch (car bindings)
        ((list (list* '&values vars) value-form)
         `(multiple-value-bind ,vars ,value-form
            (let+ ,(cdr bindings)
              ,@body)))
        ((list variable value-form)
         `(let ((,variable ,value-form))
            (let+ ,(cdr bindings)
              ,@body))))))
