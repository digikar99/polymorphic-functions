(in-package #:polymorphic-functions)

(defun translate-body (body translation-alist)
  (flet ((translate (node)
           (if (listp node)
               node
               (or (cdr (assoc node translation-alist))
                   node))))
    (traverse-tree body #'translate)))

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
  (declare (ignore env))
  (and (type= (car type-pair-1)
              (car type-pair-2))
       (type= (cdr type-pair-1)
              (cdr type-pair-2))))

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
