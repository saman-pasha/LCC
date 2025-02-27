(in-package :cl-user)

(require "asdf")

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system "lcc")

(let ((argv (uiop:command-line-arguments)))
  (when (> (length argv) 0)
    (lcc:compile-lcc-file (first argv))))
