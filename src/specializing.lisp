(in-package :polymorphic-functions)

(define-polymorphic-function specializing-type-of (object) :overwrite t
  :documentation "A clean wrapper around CL:TYPE-OF to deal with overspecialized
  types returned by CL:TYPE-OF. For instance, often times knowing an array is
  (ARRAY SINGLE-FLOAT) can be enough for optimization, (ARRAY SINGLE-FLOAT (2 3 4))
  is an overspecialized type in this sense.")
(declaim (notinline specializing-type-of))

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
                   `(cl:lambda (,@args)
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
  "Analogous to SPECIALIZED-FUNCTION:SPECIALIZING.

At runtime, compiles and caches a function corresponding to the
runtime types of VARS, with (OPTIMIZE SPEED) declaration.
Uses SPECIALIZING-TYPE-OF to avoid overspecializing types.
The function is compiled in a null lexical environment, with
only access to variables specified in VARS.

    POLYMORPHIC-FUNCTIONS> (defun dot-original (a b c)
                             (declare (optimize (speed 3)))
                             (loop
                               for ai across a
                               for bi across b
                               do (incf c (* ai bi)))
                             c)
    DOT-ORIGINAL
    POLYMORPHIC-FUNCTIONS> (let ((a (aops:rand* 'single-float 10000))
                                 (b (aops:rand* 'single-float 10000)))
                             (time (loop repeat 1000 do (dot-original a b 0.0f0))))
    Evaluation took:
      0.516 seconds of real time
      0.515704 seconds of total run time (0.515704 user, 0.000000 system)
      100.00% CPU
      1,138,873,226 processor cycles
      0 bytes consed

    NIL
    POLYMORPHIC-FUNCTIONS> (defun dot-specialized (a b c)
                             (specializing (a b c)
                               (declare (optimize (speed 3)))
                               (loop
                                 for ai across a
                                 for bi across b
                                 do (incf c (* ai bi)))
                               c))
    DOT-SPECIALIZED
    POLYMORPHIC-FUNCTIONS> (let ((a (aops:rand* 'single-float 10000))
                                 (b (aops:rand* 'single-float 10000)))
                             (time (loop repeat 1000 do (dot-specialized a b 0.0f0))))
    Evaluation took:
      0.076 seconds of real time
      0.076194 seconds of total run time (0.076194 user, 0.000000 system)
      100.00% CPU
      4 forms interpreted
      27 lambdas converted
      168,267,912 processor cycles
      1,502,576 bytes consed ; runtime compilation overhead on first call

    NIL
    POLYMORPHIC-FUNCTIONS> (let ((a (aops:rand* 'single-float 10000))
                                 (b (aops:rand* 'single-float 10000)))
                             (time (loop repeat 1000 do (dot-specialized a b 0.0f0))))
    Evaluation took:
      0.080 seconds of real time
      0.078954 seconds of total run time (0.078954 user, 0.000000 system)
      98.75% CPU
      174,478,140 processor cycles
      0 bytes consed

    NIL

Note that as of this writing, compiling a specialized variant still
requires at least one runtime dispatch to take place; as such this
is only useful if the specialized variant offsets the cost of dispatch,
and may not be useful for wrapping around simple functions such as addition
of two numbers, but only for more expensive functions such as element-wise
addition of two 10000-sized vectors.

In addition, this is not suitable for mutating variables outside the
SPECIALIZING form.
"
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
                      (or (funcall (cl:the cl:function
                                        (car (gethash ,specialization-idx *specialization-table*)))
                                   ,@vars)
                          (,add-new-specialization))
                      (,add-new-specialization))))
           (declare (type cl:function ,specialized-lambda))
           (funcall ,specialized-lambda ,@vars))))))
