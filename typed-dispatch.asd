(asdf:defsystem "typed-dispatch"
  :license "MIT"
  :version "0.0.0" ; alpha
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("alexandria"
               "introspect-environment"
               "trivial-types" ; some updates at https://github.com/digikar99/trivial-types
               "trivial-package-local-nicknames"
               "compiler-macro" ; https://github.com/Bike/compiler-macro
               "hu.dwim.walker"
               "fiveam" ;; just keep things together!
               "str")
  :pathname #P"src/"
  :components ((:file "package")
               (:file "typed-function"      :depends-on ("package"))
               (:module "lambda-lists"      :depends-on ("package")
                :components ((:file "base")
                             (:file "required")
                             (:file "required-optional")
                             (:file "required-key")))
               (:file "dispatch"            :depends-on ("typed-function"
                                                         "lambda-lists"))
               (:file "misc-tests"          :depends-on ("dispatch")))
  :perform (test-op (o c)
                    (eval (read-from-string "(FIVEAM:RUN :TYPED-DISPATCH)"))))
