(in-package :polymorphic-functions)

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

(defun translate-body (body translation-alist)
  (flet ((translate (node)
           (if (listp node)
               node
               (or (cdr (assoc node translation-alist))
                   node))))
    (traverse-tree body #'translate)))

;;; Interface for extended type system:
;;; - ctype class and constructor
;;; - ctype::cons-specifier-type
;;; - ctype::unparse
;;; - ctype:subctypep
;;; - ctype:ctypep
;;; - ctype:ctype=
;;; - "upgraded-extended-type"
;;; - "extended-type-specifier-p"
;;; Shadowed: subtypep typep type=

(defun extended-type-specifier-p (object &optional env)
  "Returns T if OBJECT is a non-CL type specifier. This means that
(UPGRADED-CL-TYPE OBJECT ENV) returns a different value than OBJECT."
  (when (type-specifier-p object env)
    (not (extensible-compound-types:type= (upgraded-cl-type object env)
                                          object
                                          env))))

(defun type-specifier-p (object &optional env)
  (or (extensible-compound-types:type-specifier-p object)
      (parametric-type-specifier-p object)))

(defun subtypep (type1 type2 &optional environment)
  "Like EXTENDED-TYPE-SPECIFIER:SUBTYPEP but allows PARAMETRIC-TYPE-SPECIFIER."
  (let ((type1 (deparameterize-type type1))
        (type2 (deparameterize-type type2)))
    (extensible-compound-types:subtypep type1 type2 environment)))

;; (define-compiler-macro subtypep (&whole form type1 type2 &optional env-form &environment env)
;;   (if (and (constantp type1 env) (constantp type2 env))
;;       (let ((type1 (deparameterize-type type1))
;;             (type2 (deparameterize-type type2)))
;;         `(extensible-compound-types:subtypep ,type1 ,type2 ,env-form))
;;       form))

(defvar *subtypep-alist* nil
  "An ALIST mapping a (CONS TYPE1 TYPE2) to a boolean indicating whether TYPE1
is a subtype of TYPE2.")

(defun subtypep-using-subtypep-alist (type1 type2 &optional environment)
  (declare (ignore environment))
  (let ((subtypep-value (assoc (cons type1 type2) *subtypep-alist*
                               :test (lambda (type-pair-1 type-pair-2)
                                       (and (type= (car type-pair-1)
                                                   (car type-pair-2))
                                            (type= (cdr type-pair-1)
                                                   (cdr type-pair-2)))))))
    (if subtypep-value
        (values (cdr subtypep-value) t)
        (values nil nil))))

(defvar *extended-subtypep-functions* nil
  "A list of function-designators that will be called by EXTENDED-SUBTYPEP.")

(defun extended-subtypep (type1 type2 &optional environment)
  (loop :for fn :in *extended-subtypep-functions*
        :for (subtypep knownp)
          := (multiple-value-list (funcall fn type1 type2 environment))
        :until knownp
        :finally
           (return (values subtypep knownp))))

(define-condition subtypep-not-known (condition)
  ((type1 :initarg :type1)
   (type2 :initarg :type2))
  (:report (lambda (c s)
             (with-slots (type1 type2) c
               (format s "Not known whether ~S is a subtype of ~S"
                       type1 type2)))))

(defun type-pair-= (type-pair-1 type-pair-2 &optional env)
  "Each pair is a CONS of two types."
  (and (type= (car type-pair-1)
              (car type-pair-2)
              env)
       (type= (cdr type-pair-1)
              (cdr type-pair-2)
              env)))

(defun definitive-subtypep (type1 type2 &optional environment)
  "Like POLYMORPHIC-FUNCTIONS.EXTENDED-TYPES:SUBTYPEP but uses *SUBTYPEP-ALIST*
and *EXTENDED-SUBTYPEP-FUNCTIONS* and when the second value is NIL raises a
restartable error to allow the user to specify whether the TYPE1 is
a definite subtype of TYPE2.

While using non-interactively, recommended way is to modify *SUBTYPEP-ALIST*
and *EXTENDED-SUBTYPEP-FUNCTIONS* rather than invoking-restarts.

The function-order for determining the SUBTYPEP functions is undefined."
  (let ((*extended-subtypep-functions*
          (append '(subtypep
                    subtypep-using-subtypep-alist)
                  *extended-subtypep-functions*)))
    (restart-case
        (multiple-value-bind (subtypep knownp)
            (extended-subtypep type1 type2 environment)
          (if knownp
              subtypep
              (error 'subtypep-not-known :type1 type1 :type2 type2)))
      (subtypep-t ()
        :report (lambda (s)
                  (format s "Treat TYPE1 as a subtype of TYPE2"))
        (setf (assoc-value *subtypep-alist* (cons type1 type2) :test #'type-pair-=) t)
        t)
      (subtypep-nil ()
        :report (lambda (s)
                  (format s "Treat TYPE1 as NOT a subtype of TYPE2"))
        (setf (assoc-value *subtypep-alist* (cons type1 type2) :test #'type-pair-=) nil)
        nil))))

(defun supertypep (type1 type2 &optional environment)
  (subtypep type2 type1 environment))
(define-compiler-macro supertypep (type1 type2 &optional environment)
  `(subtypep ,type2 ,type1 ,environment))

;;; FIXME: https://github.com/s-expressionists/ctype/issues/6
;;; TYPEP should return two values

(defun typep (object type &optional environment)
  "Like EXTENSIBLE-COMPOUND-TYPES:TYPEP but allows TYPE to be a PARAMETRIC-TYPE-SPECIFIER.
COMPILER-MACROEXPANDs to EXTENSIBLE-COMPOUND-TYPES:TYPEP if TYPE is a constant object."
  (let ((type (deparameterize-type type)))
    (extensible-compound-types:typep object type environment)))

(define-compiler-macro typep (&whole form object type &optional env-form &environment env)
  (if (constantp type env)
      (let* ((type (deparameterize-type type))
             (type (constant-form-value type env)))
        `(extensible-compound-types:typep ,object ',type ,env-form))
      form))

(defun type= (type1 type2 &optional env)
  (multiple-value-bind (s1 k1) (subtypep type1 type2 env)
    (multiple-value-bind (s2 k2) (subtypep type2 type1 env)
      (cond ((and s1 k1 s2 k2)
             (values t t))
            ((and k1 k2)
             (values nil t))
            (t
             (values nil nil))))))
