(cl:defpackage #:polymorphic-functions.defpackage
  (:use :cl :alexandria)
  (:shadow #:defpackage)
  (:export #:defpackage))

(in-package #:polymorphic-functions.defpackage)

;;; I didn't want to personally extend DEFPACKAGE-PLUS (someone do it please!)
;;; In addition, it doesn't integrate well with SLIME's M-.
;;; CONDUITS-PACKAGE doesn't provide this option (someone issue a PR please!)

;;; Default UIOP:DEFINE-PACKAGE doesn't work as correctly on CCL
;;; Latest ASDF does not work as reliably on *all* the implementations,
;;; at least not ECL
(defmacro defpackage (package &body options)
  "Like CL:DEFPACKAGE but provides a (:SHADOWING-IMPORT-EXPORTED-SYMBOLS {package}*) option.
Expects such package to be already defined."
  (let ((shadowing-import-symbols
          (loop :for option :in options
                :if (eq :shadowing-import-from (car option))
                  :appending (cddr option))))
    `(cl:defpackage ,package
       ,@(loop :for option :in options
               :if (eq :shadowing-import-exported-symbols (car option))
                 :appending (mappend (lambda (package)
                                       (let* ((exported-symbols
                                                (set-difference
                                                 (let (list)
                                                   (do-external-symbols (s package)
                                                     (push s list))
                                                   list)
                                                 shadowing-import-symbols
                                                 :test #'string=)))
                                         `((:shadowing-import-from ,package
                                                                   ,@exported-symbols))))
                                     (cdr option))
               :else
                 :collect option))))
