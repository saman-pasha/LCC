(in-package :lcc)

(defparameter *configs*
  (list
   'class    "~A"       ; class name will replaced ~A
   'compiler '("libtool" "--mode=compile" "gcc" "-g" "-O")
   'linker   '("libtool" "--mode=link" "gcc" "-g" "-O")
   'object   "~A.lo"    ; applied class name will replaced ~A
   'library  "lib~A.la" ; applied class name will replaced ~A
   ))
