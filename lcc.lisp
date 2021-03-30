(in-package :cl-user)

(require "asdf")
(asdf:load-system "lcc")

(let ((argv (uiop:command-line-arguments)))
  (when (> (length argv) 0)
    (lcc:compile-lcc-file (first argv))))
