(defsystem "lcc"
  :version "0.0.2"
  :author  "Saman H. Pasha"
  :license "MIT License"
  :depends-on ()
  :components ((:file "lcc-compile" :depends-on ("lcc-config"))
	       (:file "lcc-config" :depends-on ("lcc-package"))
	       (:file "lcc-package"))
  :description "Lisp C Compiler"
  :in-order-to ((test-op (test-op "lcc-test"))))
