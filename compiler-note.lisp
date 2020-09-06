(in-package :typed-dispatch)

(define-condition compiler-note (condition)
  ((reason :reader reason :initarg :reason)
   (reason-args :reader reason-args :initarg :args :initform nil))
  (:report (lambda (condition stream)
             (apply #'format stream
                    (reason condition)
                    (reason-args condition)))))

(define-condition optimize-speed-note (compiler-note)
  ((form :reader form :initarg :form))
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

