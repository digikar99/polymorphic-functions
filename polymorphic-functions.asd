(asdf:defsystem "polymorphic-functions"
  :license "MIT"
  :version "0.2.4" ; beta
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Type based dispatch for Common Lisp"
  :depends-on ("alexandria"
               "closer-mop"
               "compiler-macro-notes"
               (:feature :extensible-compound-types "extensible-compound-types-cl")
               (:feature (:not :extensible-compound-types) "ctype")
               "fiveam" ;; just keep tests together!
               "cl-form-types"
               "introspect-environment"
               "optima"
               "split-sequence"
               "trivial-garbage")
  :pathname #P"src/"
  :components ((:file "pre-package")
               (:file "package"                    :depends-on ("pre-package"))
               (:file "types"                      :depends-on ("package"))
               (:module "extended-types"           :depends-on ("package")
                :pathname
                #+extensible-compound-types #P"extended-types-exct/"
                #-extensible-compound-types #P"extended-types-cl/"
                :components ((:file "parametric-types")
                             (:file "ensure-type-form" :depends-on ("parametric-types"))
                             (:file "core"         :depends-on ("parametric-types"))
                             (:file "deparameterize-type" :depends-on ("parametric-types"))
                             (:file "supertypep"   :depends-on ("core"))
                             (:file "type="        :depends-on ("core"))
                             (:file "subtypep"     :depends-on ("core"))))
               (:module "lambda-lists"             :depends-on ("extended-types" "types")
                :components ((:file "doc")
                             (:file "parameters")
                             (:file "base"         :depends-on ("doc"
                                                                "parameters"))
                             (:file "required"     :depends-on ("base"))
                             (:file "required-optional" :depends-on ("base"))
                             (:file "required-key" :depends-on ("base"))
                             (:file "rest"         :depends-on ("base"))))
               (:file "polymorphic-function"       :depends-on ("types"
                                                                "extended-types"
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
               (:file "specializing"               :depends-on ("dispatch"))
               (:file "misc-tests"                 :depends-on ("dispatch"))
               (:file "benchmark"                  :depends-on ("misc-tests")))
  :perform (test-op (o c)
             (eval (with-standard-io-syntax
                     (read-from-string "(LET ((5AM:*ON-FAILURE* :DEBUG)
                                              (5AM:*ON-ERROR* :DEBUG)
                                              (CL:*COMPILE-VERBOSE* NIL))
                                          (FIVEAM:RUN! :POLYMORPHIC-FUNCTIONS))")))))

(defsystem "polymorphic-functions/swank"
  :depends-on ("polymorphic-functions"
               "swank")
  :description "slime/swank integration for polymorphic-functions"
  :pathname "src"
  :components ((:file "swank")))
