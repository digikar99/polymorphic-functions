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

(defvar *extended-type-specifiers* nil)
;; (declaim (inline extended-type-specifier-p))
(defun extended-type-specifier-p (object &optional env)
  "Returns T if OBJECT is a type specifier implemented using CTYPE and is a
tree containing a list starting with an element in *EXTENDED-TYPE-SPECIFIERS*"
  (declare (ignore env))
  (if (parametric-type-specifier-p object)
      nil
      (progn
        (traverse-tree object
                       (lambda (node)
                         (typecase node
                           (list (if (member (car node) *extended-type-specifiers*)
                                     (return-from extended-type-specifier-p t)
                                     node))
                           (t node))))
        nil)))

(deftype extended-type-specifier ()
  "These type specifiers are implemented using CTYPE and are a tree
containing a list starting with an element in *EXTENDED-TYPE-SPECIFIERS*"
  `(satisfies extended-type-specifier-p))

(defun type-specifier-p (object &optional env)
  (or (cl-type-specifier-p object)
      (extended-type-specifier-p object env)
      (parametric-type-specifier-p object)))

(defgeneric upgraded-extended-type (type-car)
  (:documentation "Used within POLYMORPHIC-FUNCTIONS to prepare a (CL:DECLARE (CL:TYPE ...))
statement for further type-based optimization by the compiler. This is similar to
CL:UPGRADED-ARRAY-ELEMENT-TYPE."))


(defmethod upgraded-extended-type ((type-car (eql 'type-like)))
  ;; FIXME: Use FUNCTION-TYPE or something to extract appropriate type from TYPE-CDR
  t
  ;; (destructuring-bind (var type-parameterizer) type-cdr
  ;;   (cdr (assoc 'cl:type (nth-value 2 (cltl2:variable-information var env)))))
  )

(defun upgrade-extended-type (extended-type-specifier &optional env)
  (declare (ignore env))
  (let ((upgradeable-cars (cons 'type-like *extended-type-specifiers*)))
    (traverse-tree extended-type-specifier
                   (lambda (node)
                     (typecase node
                       (list (if (member (car node) upgradeable-cars)
                                 (upgraded-extended-type (car node))
                                 node))
                       (t node))))))

(defun subtypep (type1 type2 &optional environment)
  "Like CL:SUBTYPEP but allows PARAMETRIC-TYPE-SPECIFIER as well as EXTENDED-TYPE-SPECIFIERs
COMPILER-MACROEXPANDs to CL:SUBTYPEP if both types are constant objects and
neither is a EXTENDED-TYPE-SPECIFIER."
  (let* ((type1 (deparameterize-type type1))
         (type2 (deparameterize-type type2))
         (values1p (and (listp type1) (eq 'cl:values (first type1))))
         (values2p (and (listp type2) (eq 'cl:values (first type2)))))
    (cond ((or values1p values2p)
           (loop :for i :from 0
                 :for ith-v1 := (cl-form-types:nth-value-type type1 i)
                 :for ith-v2 := (cl-form-types:nth-value-type type2 i)
                 :while (and ith-v1 ith-v2)
                 :always (subtypep ith-v1 ith-v2 environment)))
          ((and (cl-type-specifier-p type1)
                (cl-type-specifier-p type2))
           (cl:subtypep type1 type2 environment))
          (t
           (extensible-compound-types:subtypep type1 type2 environment)))))

(define-compiler-macro subtypep
    (&whole form type1-form type2-form &optional env-form &environment env)
  (if (and (constantp type1-form env) (constantp type2-form env))
      (let* ((type1-form (deparameterize-type type1-form))
             (type2-form (deparameterize-type type2-form))
             (type1 (constant-form-value type1-form env))
             (type2 (constant-form-value type2-form env))
             (values1p (and (listp type1) (eq 'cl:values (first type1))))
             (values2p (and (listp type2) (eq 'cl:values (first type2)))))
        (cond ((or values1p values2p)
               form)
              ((and (cl-type-specifier-p type1)
                    (cl-type-specifier-p type2))
               `(cl:subtypep ,type1-form ,type2-form ,env-form))
              ((and (extended-type-specifier-p (constant-form-value type1-form env))
                    (extended-type-specifier-p (constant-form-value type2-form env)))
               (once-only (env-form)
                 `(extensible-compound-types:subtypep type1 type2 environment)))
              (t
               form)))
      form))

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
  "Like CL:TYPEP but allows TYPE to be a PARAMETRIC-TYPE-SPECIFIER or EXTENDED-TYPE-SPECIFIER.
COMPILER-MACROEXPANDs to CL:TYPEP if TYPE is a constant object not a EXTENDED-TYPE-SPECIFIER."
  (let ((type (deparameterize-type type)))
    (if (cl-type-specifier-p type)
        (cl:typep object type)
        (ctype:ctypep object (ctype:specifier-ctype type environment)))))

(define-compiler-macro typep (&whole form object type &optional env-form &environment env)
  (if (constantp type env)
      (let* ((type (deparameterize-type type))
             (type (constant-form-value type env)))
        (cond ((cl-type-specifier-p type)
               `(cl:typep ,object ',type ,env-form))
              ((extended-type-specifier-p type)
               `(ctype:ctypep ,object (ctype:specifier-ctype ',type ,env-form)))
              (t
               form)))
      form))

(defun type= (type1 type2 &optional env)
  (and (subtypep type1 type2 env)
       (subtypep type2 type1 env)))
