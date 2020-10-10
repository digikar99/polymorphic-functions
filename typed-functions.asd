(asdf:defsystem "typed-functions"
  :license "MIT"
  :version "0.0.0" ; alpha
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("alexandria"
               "introspect-environment"
               "trivial-types" ; some updates at https://github.com/digikar99/trivial-types
               "hu.dwim.walker"
               "fiveam" ;; just keep things together!
               "str")
  :pathname #P"src/"
  :components ((:file "package")
               (:file "typed-function"      :depends-on ("package"))
               (:module "lambda-lists"      :depends-on ("package")
                :components ((:file "doc")
                             (:file "form-type")
                             (:file "base"  :depends-on ("doc" "form-type"))
                             (:file "required")
                             (:file "required-optional")
                             (:file "required-key")
                             (:file "required-untyped-rest")))
               (:file "dispatch"            :depends-on ("typed-function"
                                                         "lambda-lists"))
               (:file "misc-tests"          :depends-on ("dispatch")))
  :perform (test-op (o c)
                    (eval (read-from-string "(LET ((5AM:*ON-FAILURE* :DEBUG)
                                                   (5AM:*ON-ERROR* :DEBUG))
                                               (FIVEAM:RUN :TYPED-FUNCTIONS))"))))
