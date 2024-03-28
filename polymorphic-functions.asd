(asdf:defsystem "polymorphic-functions"
  :license "MIT"
  :version "0.5.0"                      ; beta
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("polymorphic-functions-lite"
               "cl-form-types"
               "compiler-macro-notes")
  :pathname #P"src/nonlite/"
  :components ((:file "package")
               (:file "utils"                      :depends-on ("package"))
               (:file "ensure-type-form"           :depends-on ("utils"))
               (:file "polymorph-compiler-macro"   :depends-on ("utils"))
               (:file "conditions"                 :depends-on ("polymorph-compiler-macro"))
               (:file "compiler-macro"             :depends-on ("conditions"))
               #+sbcl
               (:file "sbcl-transform"             :depends-on ("conditions"))
               (:file "dispatch"                   :depends-on ("conditions"
                                                                "compiler-macro"
                                                                #+sbcl "sbcl-transform"))
               (:file "misc-tests"                 :depends-on ("dispatch"))
               (:file "benchmark"                  :depends-on ("misc-tests")))
  :perform (test-op (o c)
             (eval (with-standard-io-syntax
                     (read-from-string "(LET ((5AM:*ON-FAILURE* :DEBUG)
                                              (5AM:*ON-ERROR* :DEBUG)
                                              (CL:*COMPILE-VERBOSE* NIL))
                                          (FIVEAM:RUN! :POLYMORPHIC-FUNCTIONS))")))))

(defsystem "polymorphic-functions/specializing"
  :depends-on ("polymorphic-functions")
  :description "Defines the polymorphic-functions:specializing macro"
  :pathname "src/nonlite/"
  :components ((:file "specializing")))

(defsystem "polymorphic-functions/swank"
  :depends-on ("polymorphic-functions-lite/swank")
  :description "slime/swank integration for polymorphic-functions")
