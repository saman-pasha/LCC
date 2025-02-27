(defsystem "lcc"
  :version "0.0.5"
  :author  "Saman H. Pasha (saman.h.pasha@gmail.com)"
  :license "MIT License"
  :depends-on ("str")
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
