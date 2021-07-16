(asdf:defsystem "polymorphic-functions"
  :license "MIT"
  :version "0.0.0" ; alpha
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("alexandria"
               "closer-mop"
               "compiler-macro-notes"
               "ctype"
               "fiveam" ;; just keep things together!
               "cl-form-types"
               "trivial-types" ; some updates at https://github.com/digikar99/trivial-types
               "trivial-macroexpand-all"
               "trivial-package-local-nicknames"
               "introspect-environment")
  :pathname #P"src/"
  :components ((:file "pre-package")
               (:file "package"                    :depends-on ("pre-package"))
               (:module "extended-types"           :depends-on ("package")
                :components ((:file "core")
                             (:file "supertypep"   :depends-on ("core"))
                             (:file "type="        :depends-on ("core"))
                             (:file "subtypep"     :depends-on ("core"))))
               (:module "lambda-lists"             :depends-on ("extended-types")
                :components ((:file "doc")
                             (:file "parameters")
                             (:file "base"         :depends-on ("doc"
                                                                "parameters"))
                             (:file "required"     :depends-on ("base"))
                             (:file "required-optional" :depends-on ("base"))
                             (:file "required-key" :depends-on ("base"))
                             (:file "rest"         :depends-on ("base"))))
               (:file "polymorphic-function"       :depends-on ("extended-types"
                                                                "lambda-lists"))
               (:file "conditions"                 :depends-on ("extended-types"))
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
                    (eval (read-from-string "(LET ((5AM:*ON-FAILURE* :DEBUG)
                                                   (5AM:*ON-ERROR* :DEBUG)
                                                   (CL:*COMPILE-VERBOSE* NIL))
                                               (FIVEAM:RUN! :POLYMORPHIC-FUNCTIONS))"))))
