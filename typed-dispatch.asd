(asdf:defsystem "typed-dispatch"
  :license "MIT"
  :version "0.0.0" ; alpha
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("alexandria"
               "introspect-environment"
               "trivial-types"
               "str")
  :pathname #P"src/"
  :components ((:file "package")
               (:file "typed-function"      :depends-on ("package"))
               (:file "lambda-list"         :depends-on ("package"))
               (:file "lambda-list-parsers" :depends-on ("lambda-list"))
               (:file "compiler-note"       :depends-on ("package"))
               (:file "dispatch" :depends-on ("typed-function"
                                              "lambda-list-parsers"
                                              "compiler-note"))))

(asdf:defsystem "typed-dispatch/tests"
  :depends-on ("typed-dispatch"
               "fiveam")
  :pathname #P"tests/"
  :components ((:file "package")))
