
(assert (string= (compile-type< '(|long|)) "long") nil "long")

(assert (string= (compile-type< '(|const| |long|)) "const long") nil "const long")
(assert (string= (compile-type< '(|long| *)) "long *") nil "long *")
(assert (string= (compile-type< '(|long| &)) "long &") nil "long &")
(assert (string= (compile-type< '(|long| [])) "long []") nil "long []")
(assert (string= (compile-type< '(|long| |x|)) "long x") nil "long x")

(assert (string= (compile-type< '(|const| |long| *)) "const long *") nil "const long *")
(assert (string= (compile-type< '(|const| |long| &)) "const long &") nil "const long &")
(assert (string= (compile-type< '(|const| |long| [])) "const long []") nil "const long []")
(assert (string= (compile-type< '(|const| |long| |x|)) "const long x") nil "const long x")
(assert (string= (compile-type< '(|long| * |const|)) "long * const") nil "long * const")
(assert (string= (compile-type< '(|long| & [])) "long & []") nil "long & []")
(assert (string= (compile-type< '(|long| & |x|)) "long & x") nil "long & x")
(assert (string= (compile-type< '(|long| |x| [])) "long x []") nil "long x []")

(assert (string= (compile-type< '(|const| |long| * |const|)) "const long * const") nil "const long * const")
(assert (string= (compile-type< '(|const| |long| & [])) "const long & []") nil "const long & []")
(assert (string= (compile-type< '(|const| |long| & |x|)) "const long & x") nil "const long & x")
(assert (string= (compile-type< '(|const| |long| |x| [])) "const long x []") nil "const long x []")
(assert (string= (compile-type< '(|long| * |const| [])) "long * const []") nil "long * const []")
(assert (string= (compile-type< '(|long| * |const| |x|)) "long * const x") nil "long * const x")
(assert (string= (compile-type< '(|long| & |x| [])) "long & x []") nil "long & x []")

(assert (string= (compile-type< '(|const| |long| * |const| [])) "const long * const []") nil "const long * const []")
(assert (string= (compile-type< '(|const| |long| * |const| |x|)) "const long * const x") nil "const long * const x")

(assert (string= (compile-type< '(|const| |long| * |const| |x| [])) "const long * const x []") nil "const long * const x []")

(assert (string= (compile-unary< '(! 1)) "!1") nil "!1")
(assert (string= (compile-unary< '(|++#| |x|)) "x++") nil "x++")
(assert (string= (compile-operator< '(* 1 2 3)) "(1 * 2 * 3)") nil "(1 * 2 * 3)")
(assert (string= (compile-form< '(|?| (> 2 1) (* |x| 2) (/ |x| 2)))
		 "(((2 > 1)) ? (x * 2) : (x / 2))") nil "(((2 > 1)) ? (x * 2) : (x / 2))")
(assert (string= (compile-form< '(|cast| |uint| (* 2 2))) "((unsigned int)(2 * 2))") nil "((unsigned int) ((2 * 2)))")

(assert (null (compile-function '(|function| |square| ((|int|)) (|returns| |int|)) '({|declare|}) 0)) nil
	"{declare} (function square ((int)) (returns int))")
(assert (null (compile-function '(|function| |sum| ((|long| |x|) (|long| |y| . 2)) (-> |long|)
				  (|return| (+ |x| |y|))) '() 0)) nil
				  "(function sum ((long x) (long y . 2)) (-> long) (return (+ x y)))")
(assert (null (compile-function '(|function| |square| ((|int| |x|)) (|->| |int|)
				  (|return| (* |x| |x|))) '({|inline|} {|static|}) 0)) nil
				  "{inline} {static} (function square ((int x)) (-> int) (return (* x x)))")
(assert (null (compile-function '(|function| |main| ((|int| |argc|) (|char| ** |argv|))
				  (|return| 0)) '() 0)) nil
				  "(function main ((int argc) (char ** argv)) (return 0))")

(assert (null (compile-preprocessor '(|@define| (|code| "HW \"Hello World!\"")) '() 0)) nil
	"(@define $\"HW \"Hello World!\"\"$)")
(assert (null (compile-preprocessor '(|@define| (|code| "SQUARE (x) x * x")) '() 0)) nil
	"(@define $\"SQUARE (x) x * x\"S)")

(assert (null (compile-variable '(|variable| |long| |var1|) '() 0)) nil
	"(variable long var1)")
(assert (null (compile-variable '(|variable| |long| |var2| . 12) '() 0)) nil
	"(variable long var2 . 12)")
(assert (null (compile-variable '(|variable| |long| |arr| []. '{1 2}) '() 0)) nil
	"(variable long arr [] . '{1 2})")
(assert (null (compile-variable '(|variable| |long| |var3| . 24) '({|static|}) 0)) nil
	"{static} (variable long var3 . 24)")

(assert (null (compile-enum '(|enum| (CONST1 . 0) (CONST2)) '() 0)) nil
	"(enum (CONST1 . 0) (CONST2))")
(assert (null (compile-enum '(|enum| |STATES| (STATE1) (STATE2)) '() 0)) nil
	"(enum STATES (STATE1) (STATE2))")

(assert (null (compile-guard '(|guard| _GUARD_H_) '() 0)) nil
	"(guard _GUARD_H_)")
