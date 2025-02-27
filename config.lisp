(in-package :lcc)

;;;; ubuntu libtool
;;;; mac glibtool
(defparameter *configs*
  (list
   'dumper   '("-Xclang" "-ast-dump")
   'compiler '("glibtool" "--tag=CC" "--mode=compile" "clang" "-g" "-O")
   'linker   '("glibtool" "--tag=CC" "--mode=link" "clang" "-g" "-O")))
