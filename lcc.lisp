(in-package :cl-user)

(require "asdf")
(asdf:load-system "lcc")

(when (> (length sb-ext:*posix-argv*) 1)
  (lcc:compile-lcc-file (second sb-ext:*posix-argv*)))
