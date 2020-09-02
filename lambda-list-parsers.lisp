(in-package typed-dispatch)

(defun remove-untyped-args (lambda-list &key typed)
  "Examples
  (remove-untyped-args '(a b c &key d))      ;=> (A B C)
  (remove-untyped-args '(a b c &optional d)) ;=> (A B C D)
  (remove-untyped-args '((a string) &optional ((d integer)) &rest args) :typed t)
    ;=> (A D), (integer string)

If TYPED is non-NIL, the second return value is a TYPE-LIST corresponding to the first return value."
  (let* ((type-list    nil)
         (untyped-args (append (loop :while lambda-list
                                     :for arg := (first lambda-list)
                                     :until (member arg lambda-list-keywords)
                                     :collect (if typed
                                                  (progn
                                                    (push (second arg) type-list)
                                                    (first arg))
                                                  arg)
                                     :do (setq lambda-list (rest lambda-list))) ; required args
                               (progn
                                 (loop :while lambda-list
                                       :until (eq '&optional (first lambda-list))
                                       :do (setq lambda-list (rest lambda-list)))
                                 (setq lambda-list (rest lambda-list))
                                 (loop :while lambda-list
                                       :for arg := (first lambda-list)
                                       :until (member arg lambda-list-keywords)                 
                                       :collect (if typed
                                                    (progn
                                                      (push (second (first arg))
                                                            type-list)
                                                      (first (first arg)))
                                                    arg)
                                       :do (setq lambda-list (rest lambda-list))))))) ; optional args
    (if typed
        (values untyped-args (nreverse type-list))
        untyped-args))) 

(defun parse-lambda-list (lambda-list &key typed)
  (multiple-value-bind (ordinary-args optional-args rest keyword-args)
      (parse-ordinary-lambda-list lambda-list :normalize-optional nil
                                              :normalize-keyword nil
                                              :allow-specializers typed)
    (declare (ignore rest))
    (append (if typed
                (mapcar #'first ordinary-args)
                ordinary-args)
            (if typed
                (mapcar (compose #'first #'first) optional-args)
                (mapcar #'first optional-args)) 
            (flatten (mapcar (lambda (keyword)
                               (let ((keyword (etypecase keyword
                                                (symbol keyword)
                                                (list   (caar keyword)))))
                                 (list (intern (symbol-name keyword) :keyword)
                                               keyword)))
                             keyword-args)))))
