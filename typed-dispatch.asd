(asdf:defsystem "typed-dispatch"
  :serial t
  :license "MIT"
  :version "0.0.0" ; alpha
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("alexandria"
               "introspect-environment")
  :components ((:file "package")
               (:file "typed-function")
               (:file "lambda-list-parsers")
               (:file "dispatch")))

