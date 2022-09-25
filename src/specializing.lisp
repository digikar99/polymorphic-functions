(in-package :polymorphic-functions)

(define-polymorphic-function specializing-type-of (object) :overwrite t
  :documentation "A clean wrapper around CL:TYPE-OF to deal with overspecialized
  types returned by CL:TYPE-OF. For instance, often times knowing an array is
  (ARRAY SINGLE-FLOAT) can be enough for optimization, (ARRAY SINGLE-FLOAT (2 3 4))
  is an overspecialized type in this sense.")

(defpolymorph specializing-type-of (object) (or list symbol)
  (type-of object))

(defpolymorph specializing-type-of ((object array)) (or list symbol)
  `(array ,(array-element-type object)))

(defpolymorph specializing-type-of ((object simple-array)) (or list symbol)
  `(simple-array ,(array-element-type object)))

(defpolymorph specializing-type-of ((object fixnum)) (or list symbol)
  (declare (ignore object))
  'fixnum)

(defpolymorph specializing-type-of ((object (signed-byte 32))) (or list symbol)
  (declare (ignore object))
  '(signed-byte 32))

(defun add-specialization (new-specialization table)
  "NEW-SPECIALIZATION should be a list of three elements
    SPECIALIZATION-APPLICABLE-P-LAMBDA SPECIALIZED-LAMBDA TYPE-LIST"
  (let* ((new-type-list (third new-specialization))
         (specialization-position
           (loop :for position :from 0
                 :for old-specialization :in table
                 :for old-type-list := (third old-specialization)
                 :while (type-list-more-specific-p old-type-list new-type-list)
                 :finally (return position))))
    (nconc (subseq table 0 specialization-position)
           (list new-specialization)
           (subseq table specialization-position))))

(defun add-specialization (new-specialization table &rest arg-values)
  "NEW-SPECIALIZATION should be a list of two elements
     TYPE-LIST SPECIALIZED-LAMBDA"
  (let* ((new-type-list (first new-specialization))
         (specialization-position
           (loop :for position :from 0
                 :for old-specialization :in table
                 :for old-type-list := (first old-specialization)
                 :while (type-list-more-specific-p old-type-list new-type-list)
                 :finally (return position)))
         (all-specializations
           (nconc (subseq table 0 specialization-position)
                  (list new-specialization)
                  (subseq table specialization-position)))
         (args (make-gensym-list (length arg-values))))
    (cons (compile nil
                   `(lambda (,@args)
                      (declare (optimize speed)
                               #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                               (compiler-macro-notes:muffle
                                compiler-macro-notes:optimization-failure-note))
                      (cond ,@(mapcar (lambda (specialization)
                                        (destructuring-bind
                                          (type-list specialized-lambda)
                                            specialization
                                          `((and ,@(loop :for type :in type-list
                                                         :for arg :in args
                                                         :collect `(typep ,arg ',type)))
                                            ,specialized-lambda)))
                                      all-specializations))))
          all-specializations)))

(defvar *specialization-table* (trivial-garbage:make-weak-hash-table)
  "A weak-hash-table mapping a global specialization-idx counter to
a CONS containing the specialization-dispatcher-lambda as the CAR
and the specializations as the CDR.")

(declaim (type hash-table *specialization-table*))
(defvar *specialization-count* 0)

(defmacro specializing (vars &body body)
  (assert (every #'symbolp vars))
  (let ((specialization-idx *specialization-count*))
    (incf *specialization-count*)
    (with-gensyms (specialized-lambda var-types var-type var add-new-specialization)
      `(flet ((,add-new-specialization ()
                (let* ((,var-types (mapcar #'specializing-type-of (list ,@vars)))
                       (,specialized-lambda
                         (compile nil `(lambda ,',vars
                                         (declare (optimize speed))
                                         (declare ,@(loop :for ,var :in ',vars
                                                          :for ,var-type :in ,var-types
                                                          :collect (list
                                                                    'type
                                                                    ,var-type
                                                                    ,var)))
                                         ,@',body))))
                  (setf (gethash ,specialization-idx *specialization-table*)
                        (add-specialization
                         (list
                          (list ,@(loop :For var :in vars
                                        :collect `(specializing-type-of ,var)))
                          ,specialized-lambda)
                         (cdr (gethash ,specialization-idx *specialization-table*))
                         ,@vars))
                  ,specialized-lambda)))
         (let* ((,specialized-lambda
                  (if (gethash ,specialization-idx *specialization-table*)
                      (or (funcall (car (gethash ,specialization-idx *specialization-table*)) ,@vars)
                          (,add-new-specialization))
                      (,add-new-specialization))))
           (funcall ,specialized-lambda ,@vars))))))
