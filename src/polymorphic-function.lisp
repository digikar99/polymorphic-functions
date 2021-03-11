(in-package adhoc-polymorphic-functions)

(defun type-list-p (list)
  ;; TODO: what parameter-names are valid?
  (let ((valid-p t))
    (loop :for elt := (first list)
          :while (and list valid-p) ; we don't want list to be empty
          :until (eq '&key elt)
          :do (setq valid-p
                    (and valid-p
                         (cond ((eq '&optional elt)
                                t)
                               ((type-specifier-p elt)
                                t)
                               (t
                                nil))))
              (setq list (rest list)))
    (cond ((and valid-p list (eq '&key (first list)))
           (loop :for (param type) :in (rest list)
                 :do (setq valid-p (type-specifier-p type))))
          ((and valid-p list)
           (setq valid-p nil)))
    valid-p))

(def-test type-list (:suite :adhoc-polymorphic-functions)
  (5am:is-true (type-list-p '()))
  (5am:is-true (type-list-p '(number string)))
  (5am:is-true (type-list-p '(&optional)))
  (5am:is-true (type-list-p '(&key)))
  (5am:is-true (type-list-p '(number &optional string)))
  (5am:is-true (type-list-p '(number &key (:a string)))))

(deftype type-list () `(satisfies type-list-p))

;; equalp: need to allow for symbols and (setf symbol) "function-name" more strictly
(defvar *polymorphic-function-table* (make-hash-table :test 'equalp))

(defstruct (polymorph)
  (type-list nil)
  (applicable-p-function)
  (body)
  ;; We need the FUNCTION-BODY due to compiler macros, and "objects of type FUNCTION can't be dumped into fasl files.
  (function)
  (compiler-macro))

(defun polymorph (polymorph)
  "Returns the LAMBDA associated with POLYMORPH"
  (declare (type polymorph polymorph)
           (optimize speed))
  (polymorph-function polymorph))

(declaim (notinline polymorph-applicable-p))
(defun polymorph-applicable-p (polymorph args)
  (declare (type polymorph polymorph)
           (type list args)
           (optimize speed))
  (apply (polymorph-applicable-p-function polymorph) args))

(defstruct polymorph-wrapper
  "HASH-TABLE maps a TYPE-LIST to a POLYMORPH. A TYPE-LIST is simply a LIST of TYPEs."
  ;; TODO: Check if a symbol / list denotes a type
  (name (error "NAME must be supplied.") :type function-name)
  (lambda-list (error "LAMBDA-LIST must be supplied.") :type list)
  (type-lists nil :type list)
  (lambda-list-type (error "LAMBDA-LIST-TYPE must be supplied.") :type lambda-list-type)
  (hash-table (make-hash-table :test 'equalp) :type hash-table))

(defun register-polymorph-wrapper (name lambda-list &key overwrite)
  (declare (type function-name name)
           (type list   lambda-list))
  (unless overwrite
    (when-let (wrapper (gethash name *polymorphic-function-table*))
      (cerror "Yes, delete existing POLYMORPHs and associate new ones"
              "There already exists POLYMORPHs associated with NAME ~S corresponding~%to the following TYPE-LISTS~%~{~^    ~S~%~}Do you want to delete these POLYMORPHs and associate a new~%POLYMORPHIC-FUNCTION with NAME ~S?"
              name
              (polymorph-wrapper-type-lists wrapper)
              name)))
  (setf (gethash name *polymorphic-function-table*)
        (make-polymorph-wrapper :name name
                                :lambda-list lambda-list
                                :lambda-list-type (lambda-list-type lambda-list))))

(defun retrieve-polymorph-wrapper (name)
  (gethash name *polymorphic-function-table*))

(defun register-polymorph (name type-list function-body function)
  ;; TODO: Get rid of APPLICABLE-P-FUNCTION: construct it from type-list
  (declare (type function-name name)
           (type function  function)
           (type type-list type-list)
           (type list      function-body))
  (let* ((polymorph-wrapper (gethash name *polymorphic-function-table*))
         (table             (polymorph-wrapper-hash-table polymorph-wrapper))
         (lambda-list-type  (polymorph-wrapper-lambda-list-type polymorph-wrapper)))
    (with-slots (type-lists) polymorph-wrapper
      ;; FIXME: Use a type-list equality check, not EQUALP
      (unless (member type-list type-lists :test 'equalp)
        (setf type-lists (cons type-list type-lists))))
    (multiple-value-bind (polymorph exists)
        (gethash type-list table)
      (if exists
          (setf (polymorph-function polymorph) function
                (polymorph-body     polymorph) function-body)
          (setf (gethash type-list table)
                (make-polymorph :type-list type-list
                                :applicable-p-function
                                (compile nil (applicable-p-function lambda-list-type
                                                                    type-list))
                                :body function-body
                                :function function))))))

(defun retrieve-polymorph (name &rest arg-list)
  "If successful, returns 3 values:
  the first object is the function body,
  the second is the function itself,
  the third is the type list corresponding to the polymorph that will be used for dispatch"
  (declare (type function-name name))
  (let* ((function-wrapper       (gethash name *polymorphic-function-table*))
         (wrapper-hash-table     (polymorph-wrapper-hash-table function-wrapper)))
    (loop :for polymorph :being the hash-values :of wrapper-hash-table
          :do (when (apply (polymorph-applicable-p-function polymorph)
                           arg-list)
                (return-from retrieve-polymorph
                  (values (polymorph-body      polymorph)
                          (the function (polymorph-function  polymorph))
                          (polymorph-type-list polymorph)))))
    (error 'no-applicable-polymorph
           :arg-list arg-list
           :type-lists (polymorph-wrapper-type-lists function-wrapper))))

(defun remove-polymorph (name type-list)
  (let ((wrapper (retrieve-polymorph-wrapper name)))
    (when wrapper
      (remhash type-list
               (polymorph-wrapper-hash-table wrapper))
      (removef (polymorph-wrapper-type-lists wrapper) type-list :test 'equalp))))

(define-compiler-macro retrieve-polymorph (&whole form name &rest arg-list
                                                       &environment env)
  (cond ((= 3 (policy-quality 'debug env))
         form)
        ((and (listp name)
              (eq 'quote (first name))
              (null (third name)))
         (when (eq 'funcall (first form))
           (setq form (rest form)))
         (if (eq 'retrieve-polymorph (first form))
             (let ((gensyms (make-gensym-list (length arg-list))))
               `(block retrieve-polymorph
                  ;; - TYPE_LISTS and WRAPPER-HASH-TABLE will change after the
                  ;;   "call to RETRIEVE-POLYMORPH" is compiled
                  ;; - While FUNCTION-WRAPPER can be precompiled, completely dumping it requires
                  ;;   knowledge of WRAPPER-HASH-TABLE that would only be available after
                  ;;   the call to RETRIEVE-POLYMORPH is compiled. (Am I missing something?)
                  ;;   An interested person may look up this link for MAKE-LOAD-FORM
                  ;;   https://blog.cneufeld.ca/2014/03/the-less-familiar-parts-of-lisp-for-beginners-make-load-form/
                  (let*
                      ((function-wrapper   (gethash ,name *polymorphic-function-table*))
                       (wrapper-hash-table (polymorph-wrapper-hash-table function-wrapper))
                       ,@(mapcar #'list gensyms arg-list))
                    (declare (optimize speed)
                             (type hash-table wrapper-hash-table)
                             (type polymorph-wrapper function-wrapper))
                    (loop :for polymorph :being the hash-values :of wrapper-hash-table
                          :do (when (funcall (the function
                                                  (polymorph-applicable-p-function polymorph))
                                             ,@gensyms)
                                (return-from retrieve-polymorph
                                  (values (polymorph-body polymorph)
                                          (polymorph-function polymorph)))))
                    (error 'no-applicable-polymorph
                           :arg-list (list ,@gensyms)
                           :type-lists (polymorph-wrapper-type-lists function-wrapper)))))
             form))
        (t form)))

(defun register-polymorph-compiler-macro (name type-list function)
  (declare (type function-name name)
           (type type-list type-list)
           (type function function))
  (let* ((wrapper (retrieve-polymorph-wrapper name))
         (table   (polymorph-wrapper-hash-table wrapper))
         (lambda-list-type  (polymorph-wrapper-lambda-list-type wrapper)))
    (multiple-value-bind (polymorph exists)
        (gethash type-list table)
      (if exists
          (setf (polymorph-compiler-macro polymorph) function)
          ;; Need below instead of error-ing to define the compiler macro simultaneously
          (setf (gethash type-list table)
                (make-polymorph :type-list type-list
                                :applicable-p-function
                                (compile nil (applicable-p-function lambda-list-type
                                                                    type-list))
                                :compiler-macro function))))))

(defun retrieve-polymorph-compiler-macro (name &rest arg-list)
  ;; TODO: Update this function
  (declare (type function-name name))
  (let* ((function-wrapper       (gethash name *polymorphic-function-table*))
         (wrapper-hash-table     (polymorph-wrapper-hash-table function-wrapper))
         (type-lists             (polymorph-wrapper-type-lists function-wrapper))
         (applicable-function-type-lists
            (loop :for polymorph :being the hash-values :of wrapper-hash-table
                  :if (polymorph-applicable-p polymorph arg-list)
                    :collect (polymorph-type-list polymorph))))
    (case (length applicable-function-type-lists)
      (1 (polymorph-compiler-macro
          (gethash (first applicable-function-type-lists) wrapper-hash-table)))
      (0 (error 'no-applicable-polymorph :arg-list arg-list :type-lists type-lists))
      (t (error "Multiple applicable POLYMORPHs discovered for ARG-LIST ~S:~%~{~S~^    ~%~}"
                arg-list
                applicable-function-type-lists)))))

(defun find-polymorphs (name &optional (type-list nil type-list-p))
  "Returns two values:
If a POLYMORPHIC-FUNCTION by NAME does not exist, returns NIL NIL.
If it exists, the second value is T and the first value is a possibly empty
  list of POLYMORPHs associated with NAME."
  (declare (type function-name name))
  (let* ((function-wrapper       (gethash name *polymorphic-function-table*))
         (wrapper-hash-table     (when function-wrapper
                                   (polymorph-wrapper-hash-table function-wrapper))))
    (cond ((null function-wrapper)
           (values nil nil))
          ((null type-list-p)
           (values (hash-table-values wrapper-hash-table) t))
          (t
           ;; FIXME: Use a type-list equality check, not EQUALP
           (loop :for polymorph :being the hash-values :of wrapper-hash-table
                 :do (when (equalp type-list
                                   (polymorph-type-list polymorph))
                       (return-from find-polymorphs
                         (values (list polymorph) t))))
           (values nil t)))))
