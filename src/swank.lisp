(in-package :polymorphic-functions)

(defparameter *swank-find-standard-definitions* #'swank/backend:find-definitions)
(defparameter *swank-find-other-definitions* ()
  "
Each symbol or funcallable object in the list should take the NAME (symbol)
as an argument Should return a list, such that each entry should in-turn be a
list in the form

  (DESCRIPTION SOURCE)

For SBCL, source can be captured using (SB-C:SOURCE-LOCATION) and needs to
be stored in the appropriate object at the time of the object definition.

To use the captured information, it must first be translated to
SB-INTROSPECT:DEFINITION-SOURCE using SB-INTROSPECT::TRANSLATE-SOURCE-LOCATION,
and then be passed through SWANK/SBCL::DEFINITION-SOURCE-FOR-EMACS to obtain
SOURCE.

It might be instructive to see the source code for
  (DEFIMPLEMENTATION FIND-DEFINITIONS ...) 
in swank/sbcl.lisp, as well as to trace SWANK/BACKEND:FIND-DEFINITIONS itself
while it gets invoked for standard definitions.
")

(defun extend-swank ()
  (eval
   `(defun swank/backend:find-definitions (name)
      #.(documentation 'swank/backend:find-definitions 'cl:function)
      (apply #'append
             (funcall *swank-find-standard-definitions* name)
             (mapcar (lambda (deffn) (funcall deffn name))
                     *swank-find-other-definitions*))))
  (pushnew 'find-polymorph-sources *swank-find-other-definitions*))

(defun find-polymorph-sources (name)
  (when (and (fboundp name)
             (typep (fdefinition name)
                    'polymorphic-function))
    (let ((pf (fdefinition name)))
      (cons (list (list 'polymorphic-function name)
                  (swank/sbcl::definition-source-for-emacs
                   (sb-introspect::translate-source-location
                    (polymorphic-function-source pf))
                   'polymorphic-function
                   name))
            (loop :for polymorph :in (polymorphic-function-polymorphs pf)
                  :appending
                  (list (list (list* 'polymorph
                                     name
                                     (polymorph-type-list polymorph))
                              (let ((source (sb-introspect::translate-source-location
                                             (polymorph-source polymorph))))
                                (swank/sbcl::definition-source-for-emacs
                                 source
                                 'polymorph
                                 name))))
                  :if (polymorph-compiler-macro-lambda polymorph)
                    :appending 
                    (list (list (list* 'polymorph-compiler-macro
                                       name
                                       (polymorph-type-list polymorph))
                                (let ((source (sb-introspect::translate-source-location
                                               (polymorph-compiler-macro-source polymorph))))
                                  (swank/sbcl::definition-source-for-emacs
                                   source
                                   'polymorph-compiler-macro
                                   name)))))))))
