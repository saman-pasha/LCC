(in-package :cl-user)

(defpackage :lcc
  (:use :cl)
  (:export
    :compile-ast
    :compile-lcc-file))

(defpackage :|c|
  (:use :cl)
  (:export
    :|int|
    :|uint|))
