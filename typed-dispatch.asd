(asdf:defsystem "typed-dispatch"
  :license "MIT"
  :version "0.0.0" ; alpha
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("alexandria"
               "introspect-environment"
               "trivial-types")
  :components ((:file "package")
               (:file "typed-function"      :depends-on ("package"))
               (:file "lambda-list"         :depends-on ("package"))
               (:file "lambda-list-parsers" :depends-on ("lambda-list"))
               (:file "dispatch" :depends-on ("typed-function" "lambda-list-parsers"))))

