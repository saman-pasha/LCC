(in-package :lcc)

(defparameter *configs*
  (list
   'class    "~A"
   'compiler '("libtool" "--mode=compile" "gcc" "-g" "-O")
   'linker   '("libtool" "--mode=link" "gcc" "-g" "-O")
   'object   "~A.lo"
   'library  "lib~A.la"))
