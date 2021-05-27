(asdf:defsystem "polymorphic-functions"
  :license "MIT"
  :version "0.0.0" ; alpha
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("alexandria"
               "closer-mop"
               "compiler-macro-notes"
               "fiveam" ;; just keep things together!
               "cl-form-types"
               "trivial-types" ; some updates at https://github.com/digikar99/trivial-types
               "trivial-macroexpand-all"
               "trivial-package-local-nicknames"
               "introspect-environment")
  :pathname #P"src/"
  :components ((:file "package")
               (:file "polymorphic-function"       :depends-on ("package"))
               (:module "lambda-lists"             :depends-on ("package")
                :components ((:file "doc")
                             (:file "base"         :depends-on ("doc"))
                             (:file "required")
                             (:file "required-optional")
                             (:file "required-key")
                             (:file "rest")))
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
                    (eval (read-from-string "(LET ((5AM:*ON-FAILURE* :DEBUG)
                                                   (5AM:*ON-ERROR* :DEBUG))
                                               (FIVEAM:RUN :POLYMORPHIC-FUNCTIONS))"))))
