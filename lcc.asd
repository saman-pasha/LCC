(defsystem "lcc"
  :version "0.0.1"
  :author  "Saman H. Pasha"
  :license "MIT License"
  :depends-on ()
  :components ((:file "lcc"))
  :description "Lisp C Compiler"
  :in-order-to ((test-op (test-op "lcc-tests"))))
