(defsystem "lcc"
  :version "0.0.4"
  :author  "Saman H. Pasha"
  :license "MIT License"
  :depends-on ()
  :components ((:file "compiler"   :depends-on ("target" "class"))
	       (:file "class"      :depends-on ("backend"))
	       (:file "target"     :depends-on ("backend"))
	       (:file "backend"    :depends-on ("specifier"))
	       (:file "specifier"  :depends-on ("core"))
	       (:file "core"       :depends-on ("config"))
	       (:file "config"     :depends-on ("package"))
	       (:file "package"))
  :description "Lisp C Compiler"
  :in-order-to ((test-op (test-op "test"))))
