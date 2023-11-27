(in-package #:polymorphic-functions)

(defmacro the* (type-specifier form)
  "CL:THE does not guarantee FORM to be of TYPE-SPECIFIER on all the
implementations. But, THE* guarantees this."
  (once-only (form)
    `(if (typep ,form ',type-specifier)
         ,form
         (error 'type-error :expected-type ',type-specifier :datum ,form))))

(defmethod cl-form-types:custom-form-type ((op (eql 'the*)) args env)
  (declare (ignore op))
  (let ((type1 (first args))
        (type2 (cl-form-types:form-type (second args) env)))
    (declare (ignorable type2))
    ;; TODO: Implement a SUBTYPEP that allows VALUES types.
    type1))
