(in-package :typed-dispatch)

(defmethod %lambda-list-type ((type (eql 'required)) (lambda-list list))
  (every (lambda (elt)
           (and (symbolp elt)
                (not (member elt lambda-list-keywords))))
         lambda-list))

(defmethod %defun-lambda-list ((type (eql 'required)) (lambda-list list))
  (copy-list lambda-list))

(defmethod %defun-body ((type (eql 'required)) (defun-lambda-list list))
  `(funcall (nth-value 1 (retrieve-typed-function ',*name* ,@defun-lambda-list))
            ,@defun-lambda-list))
