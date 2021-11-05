(in-package :polymorphic-functions)

;; LAMBDA-LIST-TYPE ============================================================

(define-constant +lambda-list-type-doc+
  "Returns the type of LAMBDA-LIST from amongst +LAMBDA-LIST-TYPES+.
Raises an ERROR otherwise."
  :test 'string=)

;; EFFECTIVE-LAMBDA-LIST =======================================================

(define-constant +compute-effective-lambda-list-doc+
  "Processes LAMBDA-LIST to return another lambda-list suitable for the lambda
generated in REGISTER-POLYMORPHIC-FUNCTION and UPDATE-POLYMORPHIC-FUNCTION-LAMBDA.
Raises an error if %LAMBDA-LIST-TYPE fails on *POTENTIAL-TYPE*."
  :test 'string=)

(define-constant +effective-lambda-list-doc-helper+
  "Processes LAMBDA-LIST assuming it is of type TYPE, and returns another lambda-list
that is suitable for the lambda generated in REGISTER-POLYMORPHIC-FUNCTION and
UPDATE-POLYMORPHIC-FUNCTION-LAMBDA.
  IF *LAMBDA-LIST-TYPED-P* is T,
the second value is the type-list corresponding to the LAMBDA-LIST,
and the third value is the effective-type-list."
  :test 'string=)

;; SBCL-TRANSFORM-ARG-LVARS-FROM-LAMBDA-LIST-FORM ==============================

(define-constant +sbcl-transform-arg-lvars-from-lambda-list-form+
  "Returns a FORM that can process the parameters of SB-C:DEFTRANSFORM to form an
argument lvar alist list suitable for further processing."
  :test 'string=)

;; POLYMORPHIC-FUNCTION-BODY ===================================================

(define-constant +compute-polymorphic-function-lambda-body-doc+
  "Processes LAMBDA-LIST to return the body for the lambda constructed in
UPDATE-POLYMORPHIC-FUNCTION-LAMBDA.
  Raises an error if %LAMBDA-LIST-TYPE fails on *POTENTIAL-TYPE*.
  If INVALIDATED-P is non-NIL, then emits a dummy body that will first call
UPDATE-POLYMORPHIC-FUNCTION-LAMBDA with INVALIDATE as NIL, and then recall the function.
  Can contain CTYPE::CTYPE objects that are not expected dumped."
  :test 'string=)

;; LAMBDA-DECLARATIONS =========================================================

(define-constant +lambda-declarations-doc+
  "Returns the list of declarations given the LAMBDA-LIST. the LAMBDA-LIST should
be a TYPED-LAMBDA-LIST"
  :test 'string=)

;; TYPE-LIST-SUBTYPE-P =========================================================

(define-constant +type-list-subtype-p+
  "Returns T if TYPE-LIST-1 is more specialized than TYPE-LIST-2"
  :test 'string=)

;; TYPE-LIST-CAUSES-AMBIGUOUS-CALL-P ===========================================

(define-constant +type-list-intersection-null-p+
  "Returns T if there exist argument lists that are compatible with both
TYPE-LIST-1 and TYPE-LIST-2"
  :test 'string=)



