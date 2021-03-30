(in-package :lcc)

(defparameter *configs*
  (list 'compiler '("libtool" "--mode=compile" "gcc" "-g" "-O")
	'linker   '("libtool" "--mode=link" "gcc" "-g" "-O")))
