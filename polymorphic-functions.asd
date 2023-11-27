(asdf:defsystem "polymorphic-functions"
  :license "MIT"
  :version "0.5.0"                      ; beta
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("alexandria"
               "cl-form-types"
               "compiler-macro-notes"
               "fiveam" ;; just keep tests together!
               "introspect-environment"
               "let-plus"
               "optima"
               "split-sequence"
               "trivial-types")
  :pathname #P"src/"
  :components ((:file "package")
               (:file "utils"                      :depends-on ("package"))
               (:file "types"                      :depends-on ("utils"))
               (:file "type-tools"                 :depends-on ("utils"))
               (:file "ensure-type-form"           :depends-on ("utils"))
               (:module "lambda-lists"             :depends-on ("ensure-type-form"
                                                                "type-tools"
                                                                "types")
                :components ((:file "doc")
                             (:file "parameters")
                             (:file "base"         :depends-on ("doc"
                                                                "parameters"))
                             (:file "required"     :depends-on ("base"))
                             (:file "required-optional" :depends-on ("base"))
                             (:file "required-key" :depends-on ("base"))
                             (:file "rest"         :depends-on ("base"))))
               (:file "polymorphic-function"       :depends-on ("lambda-lists"))
               (:file "conditions"                 :depends-on ("package"))
               (:file "compiler-macro"             :depends-on ("polymorphic-function"
                                                                "lambda-lists"
                                                                "conditions"))
               #+sbcl
               (:file "sbcl-transform"             :depends-on ("polymorphic-function"
                                                                "lambda-lists"
                                                                "conditions"))
               (:file "dispatch"                   :depends-on ("polymorphic-function"
                                                                "lambda-lists"
                                                                "conditions"
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
  :pathname "src"
  :components ((:file "specializing")))

(defsystem "polymorphic-functions/swank"
  :depends-on ("polymorphic-functions"
               "swank")
  :description "slime/swank integration for polymorphic-functions"
  :pathname "src"
  :components ((:file "swank")))
