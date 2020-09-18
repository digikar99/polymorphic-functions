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
                     "~%; Unable to optimize call to ~S because:~%;   ~A"
                     (form condition)
                     (apply #'format nil
                            (reason condition)
                            (reason-args condition))))))

(define-condition undeclared-type (compiler-note)
  ((var :initarg :var :reader var))
  (:report (lambda (condition stream)
             (format stream "~%Type of ~S is not declared" (var condition)))))



