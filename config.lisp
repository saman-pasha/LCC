(in-package :lcc)

(defparameter *configs*
  (list
   'compiler '("libtool" "--mode=compile" "gcc" "-g" "-O")
   'linker   '("libtool" "--mode=link" "gcc" "-g" "-O")
   'object   "~A.lo"    ; applied class name will replaced ~A for linker
   'library  "lib~A.la" ; applied class name will replaced ~A for linker
   ))
