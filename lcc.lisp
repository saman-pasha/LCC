
(defvar *output* t)

(defvar *unary* '(+ - ++ |++#| -- |--#| ~ ! |not| * |contentof| & |addressof|))
(defvar *operators* '(+ - * / % == != > < >= <= ^ |xor| << >> && |and| |or| & |bitand| |bitor| |->| $))
(defvar *assignments* '(= += -= *= /= %= <<= >>=))

(defvar *modifiers* '(& * **))

(set-dispatch-macro-character
 #\# #\t #'(lambda (stream char1 char2)
		   (declare (ignore stream char1 char2))
		   (read-from-string "|#t|")))

(set-dispatch-macro-character
 #\# #\f #'(lambda (stream char1 char2)
		   (declare (ignore stream char1 char2))
		   (read-from-string "|#f|")))

(set-macro-character
 #\{ #'(lambda (stream char)
	 (declare (ignore char))
	 (read-delimited-list #\} stream t)))

(set-macro-character #\} (get-macro-character #\)) nil)

(defun reving (list result)
  (cond ((consp list) (reving (cdr list) (cons (car list) result)))
        ((null list) result)
        (t (cons list result))))

(defun without-last(list)
  (reving (cdr (reving list '())) '()))

(defun replace-all (string part replacement &key (test #'char=))
  (with-output-to-string (out)
			 (loop with part-length = (length part)
			       for old-pos = 0 then (+ pos part-length)
			       for pos = (search part string
						 :start2 old-pos
						 :test test)
			       do (write-string string out
						:start old-pos
						:end (or pos (length string)))
			       when pos do (write-string replacement out)
			       while pos)))

(defun indent (lvl)
  (make-string (* lvl 2) :initial-element #\Space))

(defun display (&rest args)
  (format t "~{~a~^ ~}" args))

(defun read-file (path)
  (let ((targets '()))
    (with-open-file (file path)
		    (let ((*readtable* (copy-readtable)))
		      (setf (readtable-case *readtable*) :preserve)
		      (DO ((target (READ file) (READ file NIL NIL)))
			  ((NULL target) T)
			  (PUSH target targets))))
    (nreverse targets)))

(defun is-name (name)
  (let ((name (symbol-name name)))
    (cond ((string= name "const") nil)
	  ((not (find (char name 0) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")) nil)
	  (t (progn
	       (dotimes (i (- (length name) 1))
		 (unless (find (char name (+ i 1)) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890")
		   (return-from is-name nil)))
	       t)))))

(defun is-symbol (name)
  (let ((name (symbol-name name)))
    (cond ((string= name "const") nil)
	  (t (progn
	       (dotimes (i (length name))
		 (unless (find (char name i) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890")
		   (return-from is-symbol nil)))
	       t)))))

(defun compile-name< (name)
  (if (is-name name) (symbol-name name)
    (error (format nil "wrong name ~S" name))))

(defun compile-type-name< (name)
  (cond ((eq name '|uchar|) "unsigned char")
	((eq name '|ushort|) "unsigned short")
	((eq name '|uint|) "unsigned int")
	((eq name '|ulong|) "unsigned long")
	((eq name '|llong|) "long long")
	((eq name '|ullong|) "unsigned long long")
	((eq name '|i8|) "int8_t")
	((eq name '|u8|) "uint8_t")
	((eq name '|i16|) "int16_t")
	((eq name '|u16|) "uint16_t")
	((eq name '|i32|) "int32_t")
	((eq name '|u32|) "uint32_t")
	((eq name '|i64|) "int64_t")
	((eq name '|u64|) "uint64_t")
	((eq name '|i128|) "__int128")
	((eq name '|u128|) "unsigned __int128")
	((eq name '|real|) "long double")
	(t (compile-name< name))))

(defun is-array (desc)
  (when (and (listp desc) (eq (first desc) '[) (eq (car (last desc)) '])) t))

(set-macro-character
 #\[ #'(lambda (stream char)
	 (declare (ignore char))
	 (list '[ (car (read-delimited-list #\] stream t)) '])))

(set-macro-character #\] (get-macro-character #\)) nil)

(defun compile-array< (desc)
  (cond ((null desc) "")
	((not (is-array desc)) (format nil "wrong array description ~A" desc))
	((null (nth 1 desc)) "[]")
	(t (format nil "[~A]" (compile-form< (nth 1 desc))))))

(defun format-type (const type modifier const-ptr variable array)
  (format nil "~:[~;const ~]~A~:[~; ~:*~A~]~:[~; const~]~:[~*~; ~A~]~:[~; ~A~]"
	  const (compile-type-name< type) modifier const-ptr
	  variable (compile-name< variable) array (compile-array< array)))

(defun compile-type< (desc &optional no-text)
  (let ((len (if (listp desc) (length desc) 1))
	(const nil)
	(type nil)
	(modifier nil)
	(const-ptr nil)
	(variable nil)
	(array nil)
	(status 0))
    (cond ((symbolp desc) (setq type desc))
	  ((= len 1) (setq type (nth 0 desc)))
	  ((= len 2) (if (eq (nth 0 desc) '|const|)
			 (progn
			   (setq const (nth 0 desc))
			   (setq type (nth 1 desc)))
		       (if (find (nth 1 desc) *modifiers*)
			   (progn
			     (setq type (nth 0 desc))
			     (setq modifier (nth 1 desc)))
			 (if (is-array (nth 1 desc))
			     (progn
			       (setq type (nth 0 desc))
			       (setq array (nth 1 desc)))
			   (progn
			     (setq type (nth 0 desc))
			     (setq variable (nth 1 desc)))))))
	  ((= len 3) (if (eq (nth 0 desc) '|const|)
			 (if (find (nth 2 desc) *modifiers*) 
			     (progn
			       (setq const (nth 0 desc))
			       (setq type (nth 1 desc))
			       (setq modifier (nth 2 desc)))
			   (if (is-array (nth 2 desc))
			       (progn
				 (setq const (nth 0 desc))
				 (setq type (nth 1 desc))
				 (setq array (nth 2 desc)))
			     (progn
			       (setq const (nth 0 desc))
			       (setq type (nth 1 desc))
			       (setq variable (nth 2 desc)))))
		       (if (find (nth 1 desc) *modifiers*)
			   (if (eq (nth 2 desc) '|const|)
			       (progn
				 (setq type (nth 0 desc))
				 (setq modifier (nth 1 desc))
				 (setq const-ptr (nth 2 desc)))
			     (if (is-array (nth 2 desc))
				 (progn
				   (setq type (nth 0 desc))
				   (setq modifier (nth 1 desc))
				   (setq array (nth 2 desc)))
			       (progn
				 (setq type (nth 0 desc))
				 (setq modifier (nth 1 desc))
				 (setq variable (nth 2 desc)))))
			 (progn
			   (setq type (nth 0 desc))
			   (setq variable (nth 1 desc))
			   (setq array (nth 2 desc))))))
	  ((= len 4) (if (eq (nth 0 desc) '|const|)
			 (if (find (nth 2 desc) *modifiers*)
			     (if (eq (nth 3 desc) '|const|)
				 (progn
				   (setq const (nth 0 desc))
				   (setq type (nth 1 desc))
				   (setq modifier (nth 2 desc))
				   (setq const-ptr (nth 3 desc)))
			       (if (is-array (nth 3 desc))
				   (progn
				     (setq const (nth 0 desc))
				     (setq type (nth 1 desc))
				     (setq modifier (nth 2 desc))
				     (setq array (nth 3 desc)))
				 (progn
				   (setq const (nth 0 desc))
				   (setq type (nth 1 desc))
				   (setq modifier (nth 2 desc))
				   (setq variable (nth 3 desc)))))
			   (progn
			     (setq const (nth 0 desc))
			     (setq type (nth 1 desc))
			     (setq variable (nth 2 desc))
			     (setq array (nth 3 desc))))
		       (if (eq (nth 2 desc) '|const|)
			   (if (is-array (nth 3 desc))
			       (progn
				 (setq type (nth 0 desc))
				 (setq modifier (nth 1 desc))
				 (setq const-ptr (nth 2 desc))
				 (setq array (nth 3 desc)))
			     (progn
			       (setq type (nth 0 desc))
			       (setq modifier (nth 1 desc))
			       (setq const-ptr (nth 2 desc))
			       (setq variable (nth 3 desc))))
			 (progn
			   (setq type (nth 0 desc))
			   (setq modifier (nth 1 desc))
			   (setq variable (nth 2 desc))
			   (setq array (nth 3 desc))))))
	  ((= len 5) (if (is-array (nth 4 desc))
			 (progn
			   (setq const (nth 0 desc))
			   (setq type (nth 1 desc))
			   (setq modifier (nth 2 desc))
			   (setq const-ptr (nth 3 desc))
			   (setq array (nth 4 desc)))
		       (progn
			 (setq const (nth 0 desc))
			 (setq type (nth 1 desc))
			 (setq modifier (nth 2 desc))
			 (setq const-ptr (nth 3 desc))
			 (setq variable (nth 4 desc)))))
	  ((= len 6) (progn
		       (setq const (nth 0 desc))
		       (setq type (nth 1 desc))
		       (setq modifier (nth 2 desc))
		       (setq const-ptr (nth 3 desc))
		       (setq variable (nth 4 desc))
		       (setq array (nth 5 desc))))
	  (t (setq status -1)))
    (unless (or (null const) (eq const '|const|)) (setq status -2))
    (unless (or (null modifier) (eq modifier '&) (eq modifier '*) (eq modifier '**)) (setq status -3))
    (unless (or (null const-ptr) (eq const-ptr '|const|)) (setq status -4))
    (unless (or (null const-ptr) (eq modifier '*) (eq modifier '**)) (setq status -5))
    (unless (or (null array) (is-array array)) (setq status -6))
    (when (< status 0) (error (format nil "wrong type descriptor ~D ~A" status desc)))
    (values (if no-text nil (format-type const type modifier const-ptr variable array))
	    const type modifier const-ptr variable array)))

(defun format-type-value (const type modifier const-ptr variable array value)
  (let ((cvalue (compile-form< value)))
    (format nil "~A~:[~; = ~A~]" (format-type const type modifier const-ptr variable array)
	    (not (null cvalue)) cvalue)))

(defun compile-type-value< (desc &optional no-text)
  (let ((l (cdr (last desc)))
	(wl (without-last desc)))
    (if (and (listp l) (> (length desc) 2) (eq (nth (- (length desc) 2) desc) 'FUNCTION))
	(progn (setq l (nth (- (length desc) 1) desc))
	       (multiple-value-bind (text const type modifier const-ptr variable array)
		 (compile-type< (without-last wl) no-text)
		 (values (if no-text nil (format-type-value const type modifier const-ptr variable array l))
			 const type modifier const-ptr variable array l)))
      (if (and (listp l) (> (length desc) 2) (eq (nth (- (length desc) 2) desc) 'QUOTE))
	  (progn (setq l (nthcdr (- (length desc) 2) desc))
		 (multiple-value-bind (text const type modifier const-ptr variable array)
		   (compile-type< (without-last wl) no-text)
		   (values (if no-text nil (format-type-value const type modifier const-ptr variable array l))
			   const type modifier const-ptr variable array l)))
	(if (listp l)
	    (compile-type< desc)
	  (multiple-value-bind (text const type modifier const-ptr variable array)
	    (compile-type< wl no-text)
	    (values (if no-text nil (format-type-value const type modifier const-ptr variable array l))
		    const type modifier const-ptr variable array l)))))))

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

(defun compile-atom< (obj)
  (cond ((null obj) nil)
	((eq '|nil| obj) "NULL")
	((numberp obj) (format nil "~A" obj))
	((characterp obj) (format nil "'~C'" obj))
	((stringp obj) (format nil "~S" obj))
	((and (symbolp obj) (is-symbol obj))
	 (cond ((eq obj '|#t|) "true")
	       ((eq obj '|#f|) "false")
	       (t (format nil "~A" obj))))
	(t (error (format nil "syntax error \"~A\"" obj)))))

(defun compile-unary< (form)
  (unless (= (length form) 2) (error (format nil "wrong unary form ~A" form)))
  (let* ((oprt (car form))
	 (name (symbol-name oprt))
	 (is-postfix nil))
    (when (eql (char name (- (length name) 1)) #\#)
      (setq oprt (subseq name 0 (- (length name) 1)))
      (setq is-postfix t))
    (cond ((eq oprt '|not|)       (setq oprt '|!|))
	  ((eq oprt '|contentof|) (setq oprt '|*|))
	  ((eq oprt '|addressof|) (setq oprt '|&|)))
    (if is-postfix
	(format nil "~A~A" (compile-form< (cadr form)) oprt)
      (format nil "~A~A" oprt (compile-form< (cadr form))))))

(defun compile-operator< (form)
  (let ((opr (car form))
	(oprnds '()))
    (cond ((eq opr '|and|)    (setq opr '|&&|))
	  ((eq opr '|or|)     (setq opr '|\|\||))
	  ((eq opr '|bitand|) (setq opr '|&|))
	  ((eq opr '|bitor|)  (setq opr '|\||))
	  ((eq opr '|xor|)    (setq opr '|^|))
	  ((eq opr '|$|)      (setq opr '|.|)))
    (dolist (oprnd (cdr form))
      (push opr oprnds)
      (push (compile-form< oprnd) oprnds))
    (if (or (eq opr '|->|) (eq opr '|.|))
	(format nil "~{~A~^~A~}" (cdr (nreverse oprnds)))
      (format nil "(~{~A~^ ~A ~})" (cdr (nreverse oprnds))))))

(defun compile-assignment< (form)
  (let ((opr (car form))
	(oprnds '()))
    (dolist (oprnd (cdr form))
      (push opr oprnds)
      (push (compile-form< oprnd) oprnds))
    (format nil "~{~A~^ ~A ~}" (cdr (nreverse oprnds)))))

(defun compile-nth-form< (form)
  (unless (= (length form) 3) (error (format nil "wrong nth form ~A" form)))
  (format nil "~A[~A]" (compile-form< (nth 2 form)) (compile-form< (nth 1 form))))

(defun compile-?-form< (form)
  (unless (= (length form) 4) (error (format nil "wrong ? form ~A" form)))
  (format nil "((~A) ? ~A : ~A)"
	  (compile-form< (nth 1 form))
	  (compile-form< (nth 2 form))
	  (compile-form< (nth 3 form))))

(defun compile-cast-form< (form)
  (unless (= (length form) 3) (error (format nil "wrong cast form ~A" form)))
  (format nil "((~A)~A)"
	  (compile-type< (nth 1 form))
	  (compile-form< (nth 2 form))))

(defun compile-form< (form)
  (handler-case
      (if (atom form) (compile-atom< form)
	(let ((func (car form)))
	  (cond ((listp func) (error (format nil "function name or operator is missing ~A" form)))
		((eq func '|code|)   (format nil "~A" (cadr form)))
		((eq func 'QUOTE)    (format nil "{~{~A~^, ~}}" (mapcar #'compile-form< (cadr form))))
		((and (> (length form) 2) (eq func '\|) (eq (cadr form) '\|)) (compile-operator< (push '\|\| (cddr form))))
		((and (> (length form) 2) (eq func '\|)) (compile-operator< form))
		((and (= (length form) 2) (find func *unary*))     (compile-unary< form))
		((and (> (length form) 2) (find func *operators*)) (compile-operator< form))
		((eq func '|nth|)    (compile-nth-form< form)) 
		((eq func '|?|)      (compile-?-form< form)) 
		((eq func '|cast|)   (compile-cast-form< form)) 
		(t (concatenate 'string
				(format nil "~A" func)
				(format nil "(~{~A~^, ~})" (mapcar #'compile-form< (cdr form))))))))
    (error (ex)
	   (error (format nil "~&~A -> ~A~%" ex form)))))

(assert (string= (compile-unary< '(! 1)) "!1") nil "!1")
(assert (string= (compile-unary< '(|++#| |x|)) "x++") nil "x++")
(assert (string= (compile-operator< '(* 1 2 3)) "(1 * 2 * 3)") nil "(1 * 2 * 3)")
(assert (string= (compile-form< '(|?| (> 2 1) (* |x| 2) (/ |x| 2)))
		 "(((2 > 1)) ? (x * 2) : (x / 2))") nil "(((2 > 1)) ? (x * 2) : (x / 2))")
(assert (string= (compile-form< '(|cast| |uint| (* 2 2))) "((unsigned int)(2 * 2))") nil "((unsigned int) ((2 * 2)))")

(defun compile-return-form (form lvl)
  (unless (= (length form) 2) (error (format nil "wrong return form ~A" form)))
  (format *output* "~&~Areturn ~A;~%" (indent lvl) (compile-form< (cadr form))))

(defun compile-set-form (form lvl)
  (when (= (rem (length (cdr form)) 2) 1) (error (format nil "wrong set form ~A" form)))
  (let ((items '()))
    (dolist (item (cdr form))
      (push (compile-form< item) items))
    (format *output* "~&~A~{~A = ~A~^, ~};~%" (indent lvl) (nreverse items))))

(defun compile-progn-form (form lvl)
  (when (< (length form) 2) (error (format nil "wrong progn form ~A" form)))
  (compile-body (cdr form) lvl))

(defun compile-if-form (form lvl)
  (when (or (< (length form) 3) (> (length form) 4)) (error (format nil "wrong if form ~A" form)))
  (format *output* "~&~Aif (~A) {~%" (indent lvl) (compile-form< (nth 1 form)))
  (compile-body (list (nth 2 form)) (+ lvl 1))
  (if (= (length form) 3)
      (format *output* "~&~A}~%" (indent lvl))
    (progn
      (format *output* "~&~A} else {~%" (indent lvl))
      (compile-body (list (nth 3 form)) (+ lvl 1))
      (format *output* "~&~A}~%" (indent lvl)))))

(defun compile-switch-form (form lvl)
  (when (< (length form) 2) (error (format nil "wrong switch form ~A" form)))
  (format *output* "~&~Aswitch (~A) {~%" (indent lvl) (compile-form< (nth 1 form)))
  (dolist (ch-form (nthcdr 2 form))
    (cond ((eq (car ch-form) '|case|)
	   (format *output* "~&~Acase ~A:~%" (indent (+ lvl 1)) (compile-form< (cadr ch-form)))
	   (compile-body (nthcdr 2 ch-form) (+ lvl 2)))
	  ((eq (car ch-form) '|default|)
	   (format *output* "~&~Adefault:~%" (indent (+ lvl 1)))
	   (compile-body (nthcdr 1 ch-form) (+ lvl 2)))
	  (t (error (format nil "only case or default form ~A" form)))))
  (format *output* "~&~A}~%" (indent lvl)))

(defun compile-while-form (form lvl)
  (when (< (length form) 2) (error (format nil "wrong while form ~A" form)))
  (format *output* "~&~Awhile (~A) {~%" (indent lvl) (compile-form< (nth 1 form)))
  (compile-body (nthcdr 2 form) (+ lvl 1))
  (format *output* "~&~A}~%" (indent lvl)))

(defun compile-do-form (form lvl)
  (when (< (length form) 2) (error (format nil "wrong do form ~A" form)))
  (format *output* "~&~Ado {~%" (indent lvl))
  (compile-body (nthcdr 2 form) (+ lvl 1))
  (format *output* "~&~A} while (~A);~%" (indent lvl) (compile-form< (nth 1 form))))

(defun compile-for-form (form lvl)
  (when (or (< (length form) 3) (not (listp (nth 1 form)))) (error (format nil "wrong for form ~A" form)))
  (format *output* "~&~Afor (" (indent lvl))
  (let ((inits '()))
    (dolist (type (nth 1 form))
      (unless (and (not (null type)) (listp type)) (error (format nil "wrong variable definition form ~A" form)))
      (push (compile-type-value< type) inits))
    (format *output* "~{~A~^, ~}; ~A;) {~%" (nreverse inits) (compile-form< (nth 2 form))))
  (compile-body (nthcdr 3 form) (+ lvl 1))
  (format *output* "~&~A}~%" (indent lvl)))

(defun compile-for-each-form (form lvl)
  (when (or (< (length form) 4) (not (listp (nth 1 form)))) (error (format nil "wrong for each form ~A" form)))
  (let ((counter (gensym)))
    (format *output* "~&~Afor (int ~A = 0; ~A < ~A; ~A++) {~%" (indent lvl)
	    counter counter (compile-form< (nth 3 form)) counter)
    (format *output* "~&~A~A = ~A;~%" (indent (+ lvl 1)) (compile-type< (nth 1 form)) (nth 2 form)))
  (compile-body (nthcdr 4 form) (+ lvl 1))
  (format *output* "~&~A}~%" (indent lvl)))

(defun compile-let-form (form lvl)
  (when (or (< (length form) 2) (not (listp (nth 1 form)))) (error (format nil "wrong let form ~A" form)))
  (let ((is-auto     nil)
	(is-register nil)
	(is-static   nil)
	(dynamics '())
	(scope (gensym)))
    (format *output* "~&~A{ // Scope ~A ~%" (indent lvl) scope)
    (dolist (type-desc (nth 1 form))
      (unless (and (not (null type-desc)) (listp type-desc)) (error (format nil "wrong variable definition form ~A" form)))
      (cond ((and (eq (car type-desc) '|auto|)     (= (length (cdr type-desc)) 0)) (setq is-auto t))
	    ((and (eq (car type-desc) '|register|) (= (length (cdr type-desc)) 0)) (setq is-register t))
	    ((and (eq (car type-desc) '|static|)   (= (length (cdr type-desc)) 0)) (setq is-static t))
	    (t (multiple-value-bind (text const type modifier const-ptr variable array value)
		 (compile-type-value< type-desc t)
		 (when (and (listp value) (eq (first value) '|new|))
		   (when (or (= (length value) 1) (> (length value) 3))
		     (error (format nil "wrong new form ~A" value)))
		   (push variable dynamics)
		   (if (= (length value) 2)
		       (setq value (list '|cast| (list const type modifier const-ptr array)
					 (list '|malloc| (nth 1 value))))
		     (setq value (list '|cast| (list const type modifier const-ptr array)
				       (list '|calloc| (nth 1 value) (nth 2 value))))))
		 (format *output* "~&~A~:[~;static ~]~:[~;register ~]~:[~;auto ~]~A;" (indent (+ lvl 1))
			 is-static is-register is-auto
			 (format-type-value const type modifier const-ptr variable array value)))
	       (setq is-static nil))))
    (compile-body (nthcdr 2 form) (+ lvl 1))
    (dolist (variable dynamics)
      (format *output* "~&~Afree(~A);~%"(indent (+ lvl 1)) variable))
    (format *output* "~&~A} // ~A ~%"(indent lvl) scope)))

(defun compile-body (body lvl)
  (dolist (form body)
    (cond ((eq form '|nil|) (format *output* "~&~ANULL;" (indent lvl)))
	  ((symbolp form)   (format *output* "~&~A~A;" (indent lvl) (compile-form< form)))
	  (t (let ((func (car form)))
	       (cond ((listp func) (error (format nil "function name or operator is missing ~A" form)))
		     ((and (= (length form) 2) (find func *unary*))
		      (format *output* "~&~A~A;" (indent lvl) (compile-unary< form)))
		     ((and (= (length form) 3) (find func *assignments*))
		      (format *output* "~&~A~A;" (indent lvl) (compile-assignment< form)))
		     ((eq func '|break|)    (format *output* "~&~Abreak;"    (indent lvl)))
		     ((eq func '|continue|) (format *output* "~&~Acontinue;" (indent lvl)))
		     ((eq func '|return|)   (compile-return-form   form lvl))
		     ((eq func '|set|)      (compile-set-form      form lvl))
		     ((eq func '|let|)      (compile-let-form      form lvl)) 
		     ((eq func '|progn|)    (compile-progn-form    form lvl)) 
		     ((eq func '|if|)       (compile-if-form       form lvl)) 
		     ((eq func '|switch|)   (compile-switch-form   form lvl)) 
		     ((eq func '|while|)    (compile-while-form    form lvl)) 
		     ((eq func '|do|)       (compile-do-form       form lvl)) 
		     ((eq func '|for|)      (compile-for-form      form lvl)) 
		     ((eq func '|for-each|) (compile-for-each-form form lvl)) 
		     (t (concatenate 'string
				     (format *output* "~&~A~A" (indent lvl) func)
				     (format *output* "(~{~A~^, ~});" (mapcar #'compile-form< (cdr form)))))))))))

(defun compile-function (def attrs lvl)
  (let* ((is-static nil)
	 (is-declare nil)
	 (is-inline nil)
	 (is-extern nil)
	 (name (nth 1 def))
	 (params (nth 2 def))
	 (r-> (nth 3 def))
	 (has-returns (and (consp r->) (eq (car r->) '|returns|)))
	 (returns (if has-returns r->
		    (if (eq name '|main|) '(|returns| |int|) '(|returns| |void|))))
	 (body (if has-returns (nthcdr 4 def) (nthcdr 3 def))))
    (dolist (attr attrs)
      (let ((name (car attr)))
	(cond ((eq name '|static|)  (setq is-static t))
	      ((eq name '|declare|) (setq is-declare t))
	      ((eq name '|inline|)  (setq is-inline t))
	      ((eq name '|extern|)  (setq is-extern t))
	      (t (error (format nil "unknown function attribute ~A" attr))))))
    (when (< (length def) 3) (error (format nil "wrong function definition ~A" def)))
    (format *output* "~&~A~:[~;extern ~]~:[~;inline ~]~:[~;static ~]~A ~A ("
	    (indent lvl) is-extern is-inline is-static (compile-type< (cdr returns)) (compile-name< name))
    (let ((cparams '()))
      (dolist (param params)
	(push (compile-type-value< param) cparams))
      (format *output* "~{~A~^, ~})~:[ {~;;~]~%" (nreverse cparams) is-declare))
    (when (and is-declare body) (error (format nil "function declaration with body ~A" body)))
    (if is-declare
	(progn
	  (format *output* "~&typedef ~A (*~A_t)" (compile-type< (cdr returns)) (compile-name< name))
	  (let ((cparams '()))
	    (dolist (param params)
	      (multiple-value-bind (text const type modifier const-ptr variable array value)
		(compile-type-value< param)
		(push (format-type const type modifier const-ptr variable array) cparams)))
	    (format *output* " (~{~A~^, ~});~%" (nreverse cparams))))
      (progn
	(compile-body body (+ lvl 1))
	(format *output* "~&~A}~%" (indent lvl))))))

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

(defun compile-include (def attrs lvl)
  (when (> (length attrs) 0) (error (format nil "wrong attributes ~A" attrs)))
  (let ((heads (cdr def)))
    (dolist (head heads)
      (cond ((symbolp head) (format *output* "~&#include ~A" head))
	    ((stringp head) (format *output* "~&#include ~S" head))
	    (t (error "wrong inclusion"))))))

(defun compile-preprocessor (def attrs lvl)
  (when (> (length attrs) 0) (error (format nil "wrong attributes ~A" attrs)))
  (when (> (length def) 2) (error (format nil "wrong preprocessor definition ~A" def)))
  (let ((name (symbol-name (nth 0 def))))
    (setf (char name 0) #\#)
    (format *output* "~&~A~:[~; ~A~]~%" name (> (length def) 1) (compile-form< (nth 1 def)))))

(assert (null (compile-preprocessor '(|@define| (|code| "HW \"Hello World!\"")) '() 0)) nil
	"(@define $\"HW \"Hello World!\"\"$)")
(assert (null (compile-preprocessor '(|@define| (|code| "SQUARE (x) x * x")) '() 0)) nil
	"(@define $\"SQUARE (x) x * x\"S)")

(defun compile-variable (def attrs lvl)
  (let* ((is-auto     nil)
	 (is-register nil)
	 (is-static   nil)
	 (is-extern   nil)
	 (type (cdr def)))
    (dolist (attr attrs)
      (let ((name (car attr)))
	(cond ((eq name '|auto|)     (setq is-auto t))
	      ((eq name '|register|) (setq is-register t))
	      ((eq name '|static|)   (setq is-static t))
	      ((eq name '|extern|)   (setq is-extern t))
	      (t (error (format nil "unknown variable attribute ~A" attr))))))
    (format *output* "~&~A~:[~;extern ~]~:[~;static ~]~:[~;register ~]~:[~;auto ~]~A;~%" (indent lvl)
	    is-extern is-static is-register is-auto
	      (compile-type-value< type))))

(assert (null (compile-variable '(|variable| |long| |var1|) '() 0)) nil
	"(variable long var1)")
(assert (null (compile-variable '(|variable| |long| |var2| . 12) '() 0)) nil
	"(variable long var2 . 12)")
(assert (null (compile-variable '(|variable| |long| |arr| []. '{1 2}) '() 0)) nil
	"(variable long arr [] . '{1 2})")
(assert (null (compile-variable '(|variable| |long| |var3| . 24) '({|static|}) 0)) nil
	"{static} (variable long var3 . 24)")

(defun compile-enum (def attrs lvl)
  (when (> (length attrs) 0) (error (format nil "wrong attributes ~A" attrs)))
  (let* ((is-anonymous (or (= (length def) 1) (not (symbolp (nth 1 def)))))
	 (name (if is-anonymous nil (compile-name< (nth 1 def))))
	 (constants (if is-anonymous (nthcdr 1 def) (nthcdr 2 def))))
    (format *output* "~&~A~:[typedef enum ~A~;enum~] {~%" (indent lvl) is-anonymous name)
    (loop for const in constants
	  with l = (length constants)
	  for i from 0 to l
	  do (progn
	       (unless (and (consp const) (symbolp (car const))) (error (format nil "syntax error ~A" const)))
	       (let ((key (car const))
		     (value (cdr const)))
		 (unless (or (null value) (numberp value) (symbolp value)) (error (format nil "syntax error ~A" const)))
		 (format *output* "~&~A~A~:[ = ~A~;~*~]~:[~;,~]~%" (indent (+ lvl 1))
			 key (null value) value (< i (- l 1))))))
    (format *output* "~&}~:[ ~A~;~];~%" is-anonymous name)))

(assert (null (compile-enum '(|enum| (CONST1 . 0) (CONST2)) '() 0)) nil
	"(enum (CONST1 . 0) (CONST2))")
(assert (null (compile-enum '(|enum| |STATES| (STATE1) (STATE2)) '() 0)) nil
	"(enum STATES (STATE1) (STATE2))")

(defun compile-struct (def attrs lvl)
  (when (> (length attrs) 0) (error (format nil "wrong attributes ~A" attrs)))
  (let* ((is-anonymous (or (= (length def) 1) (not (symbolp (nth 1 def)))))
	 (name (if is-anonymous nil (compile-name< (nth 1 def))))
	 (clauses (if is-anonymous (nthcdr 1 def) (nthcdr 2 def))))
    (format *output* "~&~A~:[typedef struct ~A~;struct~] {~%" (indent lvl) is-anonymous name)
    (let ((attributes '())
	  (declares '()))
      (dolist (clause clauses)
	(if (consp clause)
	    (let ((construct (car clause)))
	      (cond ((eql (char (symbol-name construct) 0) #\@) (compile-preprocessor clause attributes (+ lvl 1)))
		    ((eq construct '|code|)     (format *output* "~&~A~A~%" (indent lvl) (compile-form< clause)))
		    ((eq construct '|static|)   (push clause attributes))
		    ((eq construct '|declare|)  (push clause attributes))
		    ((eq construct '|inline|)   (push clause attributes))
		    ((eq construct '|auto|)     (push clause attributes))
		    ((eq construct '|register|) (push clause attributes))
		    ((eq construct '|extern|)   (push clause attributes))
		    ((eq construct '|member|)   (compile-variable clause attributes (+ lvl 1)) (setq attributes '()))
		    ((eq construct '|method|)   (compile-function clause attributes (+ lvl 1)) (setq attributes '()))
		    ((eq construct '|enum|)     (compile-enum     clause attributes (+ lvl 1)) (setq attributes '()))
		    ((eq construct '|struct|)   (compile-struct   clause attributes (+ lvl 1)) (setq attributes '()))
		    ((eq construct '|union|)    (compile-union    clause attributes (+ lvl 1)) (setq attributes '()))
		    ((eq construct '|declares|) (push (nth 1 clause) declares))
		    (t (error (format nil "unknown clause ~A in struct ~A" construct name)))))
	  (error (format nil "syntax error ~A" clause))))
      (when (and (not is-anonymous) (> (length declares) 0))
	(error (format nil "declares must be inside anonymous struct ~A" name)))
      (format *output* "~&~A}~:[ ~A~;~]~:[ ~;~]" (indent lvl) is-anonymous name (null declares))
      (format *output* "~{~A~^, ~};~%" declares))))

(defun compile-union (def attrs lvl)
  (when (> (length attrs) 0) (error (format nil "wrong attributes ~A" attrs)))
  (let* ((is-anonymous (or (= (length def) 1) (not (symbolp (nth 1 def)))))
	 (name (if is-anonymous nil (compile-name< (nth 1 def))))
	 (clauses (if is-anonymous (nthcdr 1 def) (nthcdr 2 def))))
    (format *output* "~&~A~:[typedef union ~A~;union~] {~%" (indent lvl) is-anonymous name)
    (let ((attributes '())
	  (declares '()))
      (dolist (clause clauses)
	(if (consp clause)
	    (let ((construct (car clause)))
	      (cond ((eql (char (symbol-name construct) 0) #\@) (compile-preprocessor clause attributes (+ lvl 1)))
		    ((eq construct '|code|)     (format *output* "~&~A~A~%" (indent lvl) (compile-form< clause)))
		    ((eq construct '|member|)   (compile-variable clause attributes (+ lvl 1)) (setq attributes '()))
		    ((eq construct '|enum|)     (compile-enum     clause attributes (+ lvl 1)) (setq attributes '()))
		    ((eq construct '|struct|)   (compile-struct   clause attributes (+ lvl 1)) (setq attributes '()))
		    ((eq construct '|union|)    (compile-union    clause attributes (+ lvl 1)) (setq attributes '()))
		    ((eq construct '|declares|) (push (nth 1 clause) declares))
		    (t (error (format nil "unknown clause ~A in union ~A" construct name)))))
	  (error (format nil "syntax error ~A" clause))))
      (when (and (not is-anonymous) (> (length declares) 0))
	(error (format nil "declares must be inside anonymous union ~A" name)))
      (format *output* "~&~A}~:[ ~A~;~]~:[ ~;~]" (indent lvl) is-anonymous name (null declares))
      (format *output* "~{~A~^, ~};~%" declares))))

(defun compile-typedef (def attrs lvl)
  (when (> (length attrs) 0) (error (format nil "wrong attributes ~A" attrs)))
  (when (< (length def) 3) (error (format nil "syntax error ~A" def)))
  (multiple-value-bind (text const type modifier const-ptr variable array) (compile-type< (nthcdr 1 def))
		       (if (null variable)
			   (error (format nil "syntax error ~A" def))
			 (format *output* "~&typedef ~A;~%" text))))
  
(defun compile-guard (def attrs lvl)
  (when (> (length attrs) 0) (error (format nil "wrong attributes ~A" attrs)))
  (let ((name (compile-name< (nth 1 def)))
	(clauses (nthcdr 2 def)))
    (format *output* "~&#ifndef ~A~%" name)
    (format *output* "~&#define ~A~%" name)
    (let ((attributes '()))
      (dolist (clause clauses)
	(if (consp clause)
	    (let ((construct (car clause)))
	      (cond ((eql (char (symbol-name construct) 0) #\@) (compile-preprocessor clause attributes lvl))
		    ((eq construct '|code|)     (format *output* "~&~A~A~%" (indent lvl) (compile-form< clause)))
		    ((eq construct '|static|)   (push clause attributes))
		    ((eq construct '|declare|)  (push clause attributes))
		    ((eq construct '|inline|)   (push clause attributes))
		    ((eq construct '|auto|)     (push clause attributes))
		    ((eq construct '|register|) (push clause attributes))
		    ((eq construct '|extern|)   (push clause attributes))
		    ((eq construct '|guard|)    (compile-guard    clause attributes lvl))
		    ((eq construct '|include|)  (compile-include  clause attributes lvl))
		    ((eq construct '|variable|) (compile-variable clause attributes lvl) (setq attributes '()))
		    ((eq construct '|function|) (compile-function clause attributes lvl) (setq attributes '()))
		    ((eq construct '|enum|)     (compile-enum     clause attributes lvl) (setq attributes '()))
		    ((eq construct '|struct|)   (compile-struct   clause attributes lvl) (setq attributes '()))
		    ((eq construct '|union|)    (compile-union    clause attributes lvl) (setq attributes '()))
		    ((eq construct '|typedef|)  (compile-typedef  clause attributes lvl) (setq attributes '()))
		    (t (error (format nil "unknown clause ~A in guard ~A" construct name)))))
	  (error (format nil "syntax error ~A" clause))))
      (format *output* "~&#endif /* ~A */ ~%" name))))

(assert (null (compile-guard '(|guard| _GUARD_H_) '() 0)) nil
	"(guard _GUARD_H_)")

(defun compile-target (target)
  (let* ((file (nth 1 target))
	 (args (nth 2 target))
	 (clauses (nthcdr 3 target)))
    (case file
	  ((|t|) (setq *output* t))
	  (otherwise (setq *output*
			   (open file
				 :direction :output
				 :if-does-not-exist :create
				 :if-exists :supersede))))
    (when (> (length args) 0)
      (when (eq (car args) ':|std|)
	(format *output* "#include <stdio.h>~%")
	(format *output* "#include <stddef.h>~%")
	(format *output* "#include <stdint.h>~%")
	(format *output* "#include <stdlib.h>~%")
	(format *output* "#include <stdbool.h>~%")))
    (unwind-protect
	(let ((attributes '()))
	  (dolist (clause clauses)
	    (if (consp clause)
		(let ((construct (car clause)))
		  (cond ((or (eql (char (symbol-name construct) 0) #\@)
			     (eql (char (symbol-name construct) 0) #\#))
			 (compile-preprocessor clause attributes 0))
			((eq construct '|code|)     (format *output* "~&~A~%" (compile-form< clause)))
			((eq construct '|static|)   (push clause attributes))
			((eq construct '|declare|)  (push clause attributes))
			((eq construct '|inline|)   (push clause attributes))
			((eq construct '|auto|)     (push clause attributes))
			((eq construct '|register|) (push clause attributes))
			((eq construct '|extern|)   (push clause attributes))
			((eq construct '|guard|)    (compile-guard    clause attributes 0))
			((eq construct '|include|)  (compile-include  clause attributes 0))
			((eq construct '|variable|) (compile-variable clause attributes 0) (setq attributes '()))
			((eq construct '|function|) (compile-function clause attributes 0) (setq attributes '()))
			((eq construct '|enum|)     (compile-enum     clause attributes 0) (setq attributes '()))
			((eq construct '|struct|)   (compile-struct   clause attributes 0) (setq attributes '()))
			((eq construct '|union|)    (compile-union    clause attributes 0) (setq attributes '()))
			((eq construct '|typedef|)  (compile-typedef  clause attributes 0) (setq attributes '()))
			(t (error (format nil "unknown clause ~A in target ~A" construct file)))))
	      (error (format nil "syntax error ~A" clause))))
	  (terpri *output*)
	  (close *output*))
      (progn
	(case file
	    ((|t|) (setq *output* t))
	    (otherwise (close *output*)))))
    (setq *output* t)))

(defun compile-lcc-file (targets)
  (dolist (target targets)
    (let ((name (car target)))
      (cond ((eq name '|target|) (compile-target target))
	    (t (error (format nil "target is missing for ~A" name)))))))

(let ((rt (copy-readtable nil)))
  (multiple-value-bind (function non-terminating-p)
    (get-macro-character #\| rt)
    (set-macro-character #\| nil nil)
    (compile-lcc-file (read-file (second *posix-argv*)))
    (set-macro-character #\| function non-terminating-p)))
