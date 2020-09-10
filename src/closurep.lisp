(in-package :typed-dispatch)

(defun closurep (function)
  "Returns true if FUNCTION is bound in a non-null lexical environment."
  (declare (type function function))
  #+sbcl (sb-kernel:closurep function)
  #+ccl (eq (type-of function) 'ccl:compiled-lexical-closure))
