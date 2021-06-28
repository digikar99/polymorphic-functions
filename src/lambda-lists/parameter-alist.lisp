(in-package :polymorphic-functions)

;; May be struct is an overkill?
(defstruct parameter local-name form-in-pf type)

(defun typed-lambda-list-parameter-alist (untyped-lambda-list typed-lambda-list)
  "Returns a ALIst mapping local parameters (from TYPED-LAMBDA-LIST)
to the appropriate PARAMETER."
  (let ((state           :required)
        (parameter-alist ())
        (rest-idx        0)
        (rest-arg        nil))
    (declare (optimize debug))
    (loop :for parameter-specifier :in typed-lambda-list
          ;; :do (print (list parameter-specifier
          ;;                  (car untyped-lambda-list)
          ;;                  parameter-alist))
          :do (setq state
                    (case (car untyped-lambda-list)
                      (&optional '&optional)
                      (&key '&key)
                      (&rest
                       (setq untyped-lambda-list (cdr untyped-lambda-list))
                       (setq rest-arg (car untyped-lambda-list))
                       '&rest)
                      (t state)))
              (unless (member parameter-specifier lambda-list-keywords)
                (ecase state
                  (:required
                   (destructuring-bind (name type) parameter-specifier
                     (setf (assoc-value parameter-alist name)
                           (make-parameter :local-name name
                                           :form-in-pf (car untyped-lambda-list)
                                           :type type))
                     (setq untyped-lambda-list (cdr untyped-lambda-list))))
                  (&optional
                   (destructuring-bind ((name type) default) parameter-specifier
                     (declare (ignore default))
                     (setf (assoc-value parameter-alist name)
                           (make-parameter :local-name name
                                           :form-in-pf (car untyped-lambda-list)
                                           :type type))
                     (setq untyped-lambda-list (cdr untyped-lambda-list))))
                  (&rest
                   (when (listp parameter-specifier)
                     (let (name type)
                       (if (listp (car parameter-specifier))
                           (setq name (first  (car parameter-specifier))
                                 type (second (car parameter-specifier)))
                           (setq name (car parameter-specifier)
                                 type (car parameter-specifier)))
                       (setf (assoc-value parameter-alist name)
                             (make-parameter :local-name name
                                             :form-in-pf `(nth ,rest-idx
                                                               ,rest-arg)
                                             :type type))
                       (setq untyped-lambda-list (cdr untyped-lambda-list))
                       (incf rest-idx))))
                  (&key
                   (destructuring-bind ((name type) default) parameter-specifier
                     (declare (ignore default))
                     (setf (assoc-value parameter-alist name)
                           (make-parameter :local-name name
                                           :form-in-pf (car untyped-lambda-list)
                                           :type type))
                     (setq untyped-lambda-list (cdr untyped-lambda-list)))))))
    parameter-alist))
