(in-package :typed-dispatch)

(define-condition compiler-note (condition)
  ((form :reader form :initarg :form)
   (reason :reader reason :initarg :reason)
   (reason-args :reader reason-args :initarg :args))
  (:report (lambda (condition stream)
             (format stream
                     "~%; Unable to optimize call to ~D because:~%;   ~D"
                     (form condition)
                     (apply #'format nil
                            (reason condition)
                            (reason-args condition))))))

(defmacro with-compiler-note (&body body)
  `(handler-bind ((compiler-note (lambda (condition)
                                   (format t "~D" condition))))
     ,@body))

