(in-package :lcc)

(defun compile-name< (name)
  (if (is-name name) (symbol-name name)
    (error (format nil "wrong name ~S" name))))

(defun compile-type-name< (name)
  (cond ((key-eq name '|uchar|) "unsigned char")
	((key-eq name '|ushort|) "unsigned short")
	((key-eq name '|uint|) "unsigned int")
	((key-eq name '|ulong|) "unsigned long")
	((key-eq name '|llong|) "long long")
	((key-eq name '|ullong|) "unsigned long long")
	((key-eq name '|i8|) "int8_t")
	((key-eq name '|u8|) "uint8_t")
	((key-eq name '|i16|) "int16_t")
	((key-eq name '|u16|) "uint16_t")
	((key-eq name '|i32|) "int32_t")
	((key-eq name '|u32|) "uint32_t")
	((key-eq name '|i64|) "int64_t")
	((key-eq name '|u64|) "uint64_t")
	((key-eq name '|i128|) "__int128")
	((key-eq name '|u128|) "unsigned __int128")
	((key-eq name '|real|) "long double")
	(t (compile-name< name))))

(defun compile-array< (desc globals)
  (cond ((null desc) "")
	((not (is-array desc)) (format nil "wrong array description ~A" desc))
	((null (nth 1 desc)) "[]")
	(t (format nil "[~A]" (compile-form< (nth 1 desc) globals)))))

(defun format-type< (const typeof modifier const-ptr name array-def &optional anonymous)
  (when anonymous (setq name (format nil "/* ~A */" name)))
  (format nil "~:[~;const ~]~A~:[~; ~:*~A~]~:[~; const~]~:[~*~; ~A~]~:[~; ~A~]"
	  const typeof modifier const-ptr name name (if (null array-def) nil (> (length array-def) 0)) array-def))

(defun compile-type< (desc globals &optional no-text)
  (multiple-value-bind (const typeof modifier const-ptr name array-def)
    (specify-type< desc globals)
    (values (if no-text nil (format-type< const typeof modifier const-ptr name
					  (compile-array< array-def globals)))
	      const typeof modifier const-ptr name array-def)))

(defun compile-spec-type< (spec globals &optional no-text)
  (let ((const     (const     spec))
	(typeof    (typeof    spec))
	(modifier  (modifier  spec))
	(const-ptr (const-ptr spec))
	(name      (name      spec))
	(array-def (array-def spec))
	(anonymous (anonymous spec)))
    (values (if no-text nil (format-type< const typeof modifier const-ptr name
					  (compile-array< array-def globals) anonymous))
	    const typeof modifier const-ptr name array-def)))

(defun format-type-value< (const typeof modifier const-ptr name array-def default globals &optional anonymous)
  (when anonymous (setq name (format nil "/* ~A */" name)))
  (let ((cvalue (compile-form< default globals)))
    (format nil "~A~:[~; = ~A~]" (format-type< const typeof modifier const-ptr name array-def)
	    (not (null cvalue)) cvalue)))

(defun compile-type-value< (desc globals &optional no-text)
  (multiple-value-bind (const typeof modifier const-ptr name array-def default)
    (specify-type-value< desc globals)
    (values (if no-text nil (format-type-value< const typeof modifier const-ptr name
						(compile-array< array-def globals) default globals))
	      const typeof modifier const-ptr name array-def default)))

(defun compile-spec-type-value< (spec globals &optional no-text)
  (let ((const     (const     spec))
	(typeof    (typeof    spec))
	(modifier  (modifier  spec))
	(const-ptr (const-ptr spec))
	(name      (name      spec))
	(array-def (array-def spec))
	(default   (default   spec))
	(anonymous (anonymous spec)))
    (values (if no-text nil (format-type-value< const typeof modifier const-ptr name
						(compile-array< array-def globals) default globals anonymous))
	    const typeof modifier const-ptr name array-def default)))

(defun compile-atom< (obj globals)
  (cond ((null obj) nil)
	((key-eq '|nil| obj) "NULL")
	((numberp obj) (format nil "~A" obj))
	((characterp obj) (if (eql obj #\Null) (format nil "'\\0'" obj) (format nil "'~C'" obj)))
	((stringp obj) (format nil "\"~A\"" obj))
	((listp obj)
	 (if (every #'symbolp obj)
	     (progn
	       (unless (gethash obj globals nil)
		 (error (format nil "undefined class ~A~%" obj)))
	       (format nil "~A *" (class-name< obj)))
	   (error (format nil "syntax error \"~A\"" obj))))
	((and (symbolp obj) (is-symbol obj))
	 (if (eql (char (symbol-name obj) 0) #\0)
	     (format nil "~A" obj)
	   (let ((spec (gethash obj globals nil)))
	     (if spec
		 (if (or (eql (construct spec) '|@CLASS|) (eql (construct spec) '|@IMPORT|))
		     (format nil "~A *" (class-name< (default spec)))
		   (format nil "~A" obj))
	       (progn
		 (format t "lcc: [warning] undefined variable ~A~%" obj)
		 (format nil "~A" obj))))))
	(t (error (format nil "syntax error \"~A\"" obj)))))

(defun compile-type-decl< (typeof globals)
  (let ((type-spec (gethash typeof globals nil)))
    (if type-spec
	(if (or (eql (construct type-spec) '|@CLASS|)
	      (eql (construct type-spec) '|@IMPORT|))
	    (format nil "~A *" (class-name< (default type-spec)))
	  (error (format t "it is not a type ~A~%" typeof)))
      (progn
	(format t "lcc: [warning] undefined type ~A~%" typeof)
	(format nil "~A" typeof)))))
  
(defun compile-unary< (form globals)
  (unless (= (length form) 2) (error (format nil "wrong unary form ~A" form)))
  (let* ((oprt (car form))
	 (name (symbol-name oprt))
	 (is-postfix nil))
    (when (eql (char name (- (length name) 1)) #\#)
      (setq oprt (subseq name 0 (- (length name) 1)))
      (setq is-postfix t))
    (cond ((key-eq oprt '|not|)       (setq oprt '|!|))
	  ((key-eq oprt '|contentof|) (setq oprt '|*|))
	  ((key-eq oprt '|addressof|) (setq oprt '|&|)))
    (if is-postfix
	(format nil "~A~A" (compile-form< (cadr form) globals) oprt)
      (format nil "~A~A" oprt (compile-form< (cadr form) globals)))))

(defun compile-operator< (form globals)
  (let ((opr (car form))
	(oprnds '()))
    (cond ((key-eq opr '|and|)    (setq opr '|&&|))
	  ((key-eq opr '|or|)     (setq opr '|\|\||))
	  ((key-eq opr '|bitand|) (setq opr '|&|))
	  ((key-eq opr '|bitor|)  (setq opr '|\||))
	  ((key-eq opr '|xor|)    (setq opr '|^|))
	  ((key-eq opr '|$|)      (setq opr '|.|)))
    (dolist (oprnd (cdr form))
      (push opr oprnds)
      (push (compile-form< oprnd globals) oprnds))
    (if (or (key-eq opr '|->|) (key-eq opr '|.|))
	(format nil "~{~A~^~A~}" (cdr (nreverse oprnds)))
      (format nil "(~{~A~^ ~A ~})" (cdr (nreverse oprnds))))))

(defun compile-assignment< (form globals)
  (let ((opr (car form))
	(oprnds '()))
    (dolist (oprnd (cdr form))
      (push opr oprnds)
      (push (compile-form< oprnd globals) oprnds))
    (format nil "~{~A~^ ~A ~}" (cdr (nreverse oprnds)))))

(defun compile-nth-form< (form globals)
  (unless (= (length form) 3) (error (format nil "wrong nth form ~A" form)))
  (format nil "~A[~A]" (compile-form< (nth 2 form) globals) (compile-form< (nth 1 form) globals)))

(defun compile-?-form< (form globals)
  (unless (= (length form) 4) (error (format nil "wrong ? form ~A" form)))
  (format nil "((~A) ? ~A : ~A)"
	  (compile-form< (nth 1 form) globals)
	  (compile-form< (nth 2 form) globals)
	  (compile-form< (nth 3 form) globals)))

(defun compile-cast-form< (form globals)
  (unless (= (length form) 3) (error (format nil "wrong cast form ~A" form)))
  (format nil "((~A)~A)"
	  (compile-type< (nth 1 form) globals)
	  (compile-form< (nth 2 form) globals)))

(defun compile-new-form< (form globals)
  (when (< (length form) 2) (error (format nil "wrong new form ~A" form)))
  (let ((cls   (gethash (nth 1 form) globals nil))
	(ctors '()))
    (when (null cls) (error (format nil "undefined class ~A in form ~A" (nth 1 form) form)))
    (maphash #'(lambda (name spec)
		 (when (and (eql (construct spec) '|@FUNCTION|) (eql (default spec) :ctor))
		   (push spec ctors)))
	     (inners cls))
    (dolist (ctor ctors)
      (when (= (hash-table-count (params ctor)) (- (length form) 2))
	(let ((args '()))
	  (dolist (arg (cddr form))
	    (push (compile-form< arg globals) args))
	(return-from compile-new-form< (format nil "~A(~{~A~^, ~})"
					       (method-name< (default cls) (name ctor))
					       (reverse args))))))
    (error (format nil "not found matched costructor for class ~A in form ~A" (name cls) form))))

(defun compile-call-form< (form globals)
  (when (< (length form) 2) (error (format nil "wrong call form ~A" form)))
  (let* ((var (nth 1 form))
	 (method (car form))
	 (var-spec (gethash var globals nil)))
    (if (listp var)
	(progn
	  (format nil "~A(~{~A~^, ~})" method (mapcar #'(lambda (f) (compile-form< f globals)) (cdr form))))
      (progn
	(if (or (not (is-symbol var)) (null var-spec))
	    (progn
	      (format t "lcc: [warning] undefined function ~A~%" method)
	      (format nil "~A(~{~A~^, ~})" method (mapcar #'(lambda (f) (compile-form< f globals)) (cdr form))))
	  (if (eql (construct var-spec) '|@CLASS|)
	      (let ((fun-spec (gethash method (inners var-spec) nil)))
		(when (or (null fun-spec) (not (eql (construct fun-spec) '|@FUNCTION|)) (find '|static| (attrs fun-spec)))
		  (error (format nil "class ~A does not have static method ~A in ~A" (default var-spec) method form)))
		(format nil "~A(" (static-method-name< (default var-spec) (name fun-spec))))
	    (let ((type-spec (gethash (typeof var-spec) globals nil)))
	      (if (null type-spec)
		  (progn
		    (format t "lcc: [warning] undefined type ~A~%" (typeof var-spec) form)
		    (format nil "~A(~{~A~^, ~})" method (mapcar #'(lambda (f) (compile-form< f globals)) (cdr form))))
		(let ((fun-spec (gethash method (inners type-spec) nil)))
		  (when (or (null fun-spec) (not (eql (construct fun-spec) '|@FUNCTION|)))
		    (error (format nil "class ~A does not have method ~A in ~A" (default var-spec) method form)))
		  (format nil "~A(~A~:[, ~;~]~{~A~^, ~})"
			  (method-name< (default type-spec) (name fun-spec))
			  (name var-spec) (null (cddr form))
			  (mapcar #'(lambda (f) (compile-form< f globals)) (cddr form)))
		  )))))))))

(defun compile-form< (form globals)
  (handler-case
      (if (atom form) (compile-atom< form globals)
	(let ((func (car form)))
	  (cond ((listp func) (error (format nil "function name or operator is missing ~A" form)))
		((key-eq func '|code|) (format nil "~A" (cadr form)))
		((key-eq func 'QUOTE)  (format nil "{~{~A~^, ~}}" (mapcar #'(lambda (f) (compile-form< f globals)) (cadr form))))
		((and (> (length form) 2) (key-eq func '\|) (key-eq (cadr form) '\|))
		 (compile-operator< (push '\|\| (cddr form)) globals))
		((and (> (length form) 2) (key-eq func '\|)) (compile-operator< form globals))
		((and (= (length form) 2) (find func *unaries* :test #'key-eq))   (compile-unary< form globals))
		((and (> (length form) 2) (find func *operators* :test #'key-eq)) (compile-operator< form globals))
		((key-eq func '|nth|)    (compile-nth-form<  form globals)) 
		((key-eq func '|?|)      (compile-?-form<    form globals)) 
		((key-eq func '|cast|)   (compile-cast-form< form globals))
		((key-eq func '|new|)    (compile-new-form<  form globals))
		(t                       (compile-call-form< form globals))))) 
    (error (ex)
	   (error (format nil "~&~A -> ~A~%" ex form)))))

(defun compile-return-form (form lvl globals)
  (unless (= (length form) 2) (error (format nil "wrong return form ~A" form)))
  (output "~&~Areturn ~A;~%" (indent lvl) (compile-form< (cadr form) globals)))

(defun compile-set-form (form lvl globals)
  (when (= (rem (length (cdr form)) 2) 1) (error (format nil "wrong set form ~A" form)))
  (let ((items '()))
    (dolist (item (cdr form))
      (push (compile-form< item globals) items))
    (output "~&~A~{~A = ~A~^, ~};~%" (indent lvl) (nreverse items))))

(defun compile-block-form (form lvl globals)
  (when (< (length form) 2) (error (format nil "wrong block form ~A" form)))
  (compile-body (cdr form) lvl globals))

(defun compile-if-form (form lvl globals)
  (when (or (< (length form) 3) (> (length form) 4)) (error (format nil "wrong if form ~A" form)))
  (output "~&~Aif (~A) {~%" (indent lvl) (compile-form< (nth 1 form) globals))
  (compile-body (list (nth 2 form)) (+ lvl 1) globals)
  (if (= (length form) 3)
      (output "~&~A}~%" (indent lvl))
    (progn
      (output "~&~A} else {~%" (indent lvl))
      (compile-body (list (nth 3 form)) (+ lvl 1) globals)
      (output "~&~A}~%" (indent lvl)))))

(defun compile-switch-form (form lvl globals)
  (when (< (length form) 2) (error (format nil "wrong switch form ~A" form)))
  (output "~&~Aswitch (~A) {~%" (indent lvl) (compile-form< (nth 1 form) globals))
  (dolist (ch-form (nthcdr 2 form))
    (cond ((key-eq (car ch-form) '|case|)
	   (output "~&~Acase ~A:~%" (indent (+ lvl 1)) (compile-form< (cadr ch-form) globals))
	   (compile-body (nthcdr 2 ch-form) (+ lvl 2) globals))
	  ((key-eq (car ch-form) '|default|)
	   (output "~&~Adefault:~%" (indent (+ lvl 1)))
	   (compile-body (nthcdr 1 ch-form) (+ lvl 2) globals))
	  (t (error (format nil "only case or default form ~A" form)))))
  (output "~&~A}~%" (indent lvl)))

(defun compile-while-form (form lvl globals)
  (when (< (length form) 2) (error (format nil "wrong while form ~A" form)))
  (output "~&~Awhile (~A) {~%" (indent lvl) (compile-form< (nth 1 form) globals))
  (compile-body (nthcdr 2 form) (+ lvl 1) globals)
  (output "~&~A}~%" (indent lvl)))

(defun compile-do-form (form lvl globals)
  (when (< (length form) 2) (error (format nil "wrong do form ~A" form)))
  (output "~&~Ado {~%" (indent lvl))
  (compile-body (nthcdr 2 form) (+ lvl 1) globals)
  (output "~&~A} while (~A);~%" (indent lvl) (compile-form< (nth 1 form) globals)))

(defun compile-for-form (form lvl globals)
  (when (or (< (length form) 3) (not (listp (nth 1 form)))) (error (format nil "wrong for form ~A" form)))
  (output "~&~Afor (" (indent lvl))
  (let ((inits '())
	(locals (copy-specifiers globals)))
    (dolist (type-desc (nth 1 form))
      (unless (and (not (null type-desc)) (listp type-desc)) (error (format nil "wrong for form variable definition ~A" form)))
      (multiple-value-bind (text const typeof modifier const-ptr variable array value)
	(compile-type-value< type-desc globals t)
	(setf (gethash variable locals)
	      (make-specifier variable '|@VARIABLE| const typeof modifier const-ptr array value nil))
	(push (format-type-value< const typeof modifier const-ptr variable array value locals) inits)))
    (output "~{~A~^, ~}; ~A;) {~%" (nreverse inits) (compile-form< (nth 2 form) locals))
    (compile-body (nthcdr 3 form) (+ lvl 1) locals)
    (output "~&~A}~%" (indent lvl))))

(defun compile-for-each-form (form lvl globals)
  (when (or (< (length form) 4) (not (listp (nth 1 form)))) (error (format nil "wrong for each form ~A" form)))
  (let ((counter (gensym)))
    (output "~&~Afor (int ~A = 0; ~A < ~A; ~A++) {~%" (indent lvl)
	    counter counter (compile-form< (nth 3 form) globals) counter)
    (output "~&~A~A = ~A[~A];~%" (indent (+ lvl 1)) (compile-type< (nth 1 form) globals) (nth 2 form) counter))
  (compile-body (nthcdr 4 form) (+ lvl 1) globals)
  (output "~&~A}~%" (indent lvl)))

(defun compile-let-form (form lvl globals)
  (when (or (< (length form) 2) (not (listp (nth 1 form)))) (error (format nil "wrong let form ~A" form)))
  (let ((is-auto     nil)
	(is-register nil)
	(is-static   nil)
	(dynamics '())
	(scope (gensym "SCOPE"))
	(locals       (copy-specifiers globals)))
    (output "~&~A{ /* ~A */~%" (indent lvl) scope)
    (dolist (type-desc (nth 1 form))
      (unless (and (not (null type-desc)) (listp type-desc)) (error (format nil "wrong variable definition form ~A" form)))
      (cond ((and (key-eq (car type-desc) '|auto|)     (= (length (cdr type-desc)) 0)) (setq is-auto t))
	    ((and (key-eq (car type-desc) '|register|) (= (length (cdr type-desc)) 0)) (setq is-register t))
	    ((and (key-eq (car type-desc) '|static|)   (= (length (cdr type-desc)) 0)) (setq is-static t))
	    (t (multiple-value-bind (text const typeof modifier const-ptr variable array value)
		 (compile-type-value< type-desc globals t)
		 (when (and (listp value) (key-eq (first value) '|alloc|))
		   (when (or (= (length value) 1) (> (length value) 3))
		     (error (format nil "wrong alloc form ~A" value)))
		   (push variable dynamics)
		   (if (= (length value) 2)
		       (setq value (list '|cast| (list const typeof modifier const-ptr array)
					 (list '|malloc| (nth 1 value))))
		     (setq value (list '|cast| (list const typeof modifier const-ptr array)
				       (list '|calloc| (nth 1 value) (nth 2 value))))))
		 (let ((attributes '()))
		   (when is-static   (push '|static|   attributes))
		   (when is-register (push '|register| attributes))
		   (when is-auto     (push '|auto|     attributes))
		   (setf (gethash variable locals)
			 (make-specifier variable '|@VARIABLE| const typeof modifier const-ptr array value attributes)))
		 (output "~&~A~:[~;static ~]~:[~;register ~]~:[~;auto ~]~A;" (indent (+ lvl 1))
			 is-static is-register is-auto
			 (format-type-value< const (compile-type-decl< typeof globals)
					     modifier const-ptr variable array value locals)))
	       (setq is-static nil))))
    (dolist (variable (reverse dynamics))
      (output "~&~Aif (~A == NULL) printf(\"dynamic memory allocation failed! ~A\\n\");~%" (indent (+ lvl 1))
	      variable variable))
    (compile-body (nthcdr 2 form) (+ lvl 1) locals)
    (dolist (variable dynamics)
      (output "~&~Afree(~A);~%"(indent (+ lvl 1)) variable))
    (output "~&~A} /* ~A */~%"(indent lvl) scope)))

(defun compile-body (body lvl globals)
  (dolist (form body)
    (cond ((key-eq form '|nil|) (output "~&~ANULL;" (indent lvl)))
	  ((symbolp form)       (output "~&~A~A;" (indent lvl) (compile-form< form globals)))
	  (t (let ((func (car form))
		   (locals (copy-specifiers globals)))
	       (cond ((listp func) (error (format nil "function name or operator is missing ~A" form)))
		     ((and (= (length form) 2) (find func *unaries* :test #'key-eq))
		      (output "~&~A~A;" (indent lvl) (compile-unary< form locals)))
		     ((and (= (length form) 3) (find func *assignments* :test #'key-eq))
		      (output "~&~A~A;" (indent lvl) (compile-assignment< form locals)))
		     ((key-eq func '|break|)    (output "~&~Abreak;"    (indent lvl)))
		     ((key-eq func '|continue|) (output "~&~Acontinue;" (indent lvl)))
		     ((key-eq func '|return|)   (compile-return-form   form lvl locals))
		     ((key-eq func '|set|)      (compile-set-form      form lvl locals))
		     ((key-eq func '|let|)      (compile-let-form      form lvl locals)) 
		     ((key-eq func '|block|)    (compile-block-form    form lvl locals)) 
		     ((key-eq func '|if|)       (compile-if-form       form lvl locals)) 
		     ((key-eq func '|switch|)   (compile-switch-form   form lvl locals)) 
		     ((key-eq func '|while|)    (compile-while-form    form lvl locals)) 
		     ((key-eq func '|do|)       (compile-do-form       form lvl locals)) 
		     ((key-eq func '|for|)      (compile-for-form      form lvl locals)) 
		     ((key-eq func '|for-each|) (compile-for-each-form form lvl locals)) 
		     (t                         (output "~&~A~A;~%" (indent lvl)
							(compile-form< form locals)))
		     ))))))

(defun compile-variable (spec lvl globals &optional memberof)
  (let ((is-auto     nil)
	(is-register nil)
	(is-static   nil)
	(is-extern   nil))
    (dolist (attr (attrs spec))
      (case attr
	('|auto|     (setq is-auto t))
	('|register| (setq is-register t))
	('|static|   (when (null memberof) (setq is-static t)))
	('|extern|   (setq is-extern t))))
    (let ((text (compile-spec-type-value< spec globals)))
      (output "~&~A~:[~;extern ~]~:[~;static ~]~:[~;register ~]~:[~;auto ~]~A;~%"
	      (indent lvl) is-extern is-static is-register is-auto text))))

(defun compile-function (spec lvl globals &optional methodof &key no-body)
  (let* ((is-static  nil)
	 (is-declare nil)
	 (is-inline  nil)
	 (is-extern  nil)
	 (name       (if (not (null methodof)) (method-name< (default methodof) (name spec)) (name spec)))
	 (is-static-method nil)
	 (params     (params spec))
	 (body       (body   spec))
	 (locals     (copy-specifiers globals))
	 (ret        (format-type< (const spec) (compile-atom< (typeof spec) locals) (modifier spec) nil
				   (const-ptr spec) (array-def spec))))
    (dolist (attr (attrs spec))
      (case attr
	('|static|
	 (unless (null methodof) (setq is-static-method t))
	 (when   (null methodof) (setq is-static t)))
	('|declare| (setq is-declare t))
	('|inline|  (setq is-inline  t))
	('|extern|  (setq is-extern  t))))
    (unless (null methodof)
      (setf (gethash (user-symbol "this") locals)
	    (make-specifier (user-symbol "this") '|@VARIABLE| nil (default methodof)
			    (user-symbol "*") (user-symbol "const") nil nil nil))
      (setf (gethash (user-symbol "class") locals)
	    (make-specifier (user-symbol "class") '|@VARIABLE| nil (default methodof)
			    (user-symbol "*") (user-symbol "const") nil nil nil)))
    (setf (gethash (user-symbol "_LCC_FUNCTION_") locals)
	  (make-specifier (user-symbol "_LCC_FUNCTION_") '|@VARIABLE| (user-symbol "const")
			  (user-symbol "char") (user-symbol "*") (user-symbol "const") nil (name spec) '(|static|)))
    (maphash #'(lambda (in-name in-spec)
		 (case (construct in-spec)
		   ('|@PARAMETER| (setf (gethash in-name locals) in-spec))
		   (otherwise nil)))
	     (params spec))
    (output "~&~A~:[~;extern ~]~:[~;inline ~]~:[~;static ~]~A ~A ("
	    (indent lvl) is-extern is-inline is-static ret
	    (if (and is-static-method (eql (name spec) (user-symbol "main"))) '|main| name))
    (let ((cparams '()))
      (maphash #'(lambda (name param)
		   (push (compile-spec-type-value< param locals) cparams))
	       params)
      (unless (or (null methodof) (eql (default spec) :ctor))
	(unless is-static-method
	  (output "~A * const this~:[~;, ~]" (class-name< (default methodof)) (> (length cparams) 0))))
      (output "~{~A~^, ~})~:[ {~;;~]~%" (nreverse cparams) (or is-declare no-body)))
    (if (and is-declare (null methodof))
	(progn
	  (output "~&typedef ~A (*~A_t)" ret name)
	  (let ((cparams '()))
	    (maphash #'(lambda (name param)
			 (push (format-type< (const spec) (typeof spec) (modifier spec) nil
					     (const-ptr spec) (array-def spec)) cparams))
		     params)
	    (output " (~{~A~^, ~});~%" (nreverse cparams))))
      (unless no-body
	(unless (null methodof)
	  (output "~&~Astruct ~A * const class = &~A;" (indent (+ 1 lvl))
		  (static-class-name< (default methodof)) (static-variable-name< (default methodof)))
	  (when (eql (default spec) :ctor)
	    (let ((cls (class-name< (default methodof))))
	      (output "~&~A~A * const this = (~A *)(malloc(sizeof(~A)));" (indent (+ 1 lvl)) cls cls cls)
	      (output "~&~Aif (this == NULL) {~%" (indent (+ 1 lvl)))
	      (output "~&~Aprintf(\"dynamic memory allocation failed! ~A\\n\");~%" (indent (+ 2 lvl)) name)
	      (output "~&~Areturn NULL;~%" (indent (+ 2 lvl)))
	      (output "~&~A}~%" (indent (+ 1 lvl)))))
	  (output "~&~Astatic const char * const _LCC_CLASS_ = ~A;~%"
		  (indent (+ 1 lvl)) (static-class-variable-name< (default methodof))))  
	(output "~&~Astatic const char * const _LCC_FUNCTION_ = ~S;~%" (indent (+ 1 lvl)) (symbol-name (name spec)))
	(compile-body body (+ lvl 1) locals)
	(unless (null methodof)
	  (when (eql (default spec) :ctor)
	    (output "~&~Areturn this;~%" (indent (+ 1 lvl)))))
	(output "~&~A}~%" (indent lvl))))))

(defun compile-preprocessor (spec lvl globals)
  (let* ((def (body spec))
	 (dir (symbol-name (car def))))
    (setf (char dir 0) #\#)
    (output "~&~A~:[~; ~A~]~%" dir (> (length def) 1) (compile-form< (cadr def) globals))))

(defun compile-include (spec lvl globals)
  (let ((header (name spec)))
    (cond ((symbolp header) (output "~&#include ~A~%" header))
	  ((stringp header) (output "~&#include ~S~%" header))
	  (t (error "wrong inclusion")))))

(defun compile-import (spec lvl globals)
  (let ((class (default spec)))
    (if (and (listp class) (every #'symbolp class))
	(output "~&#include ~S" (class-header< class))
      (if (symbolp class)
	  (output "~&#include ~S" (class-header< class))
	(error "wrong import")))))

(defun compile-typedef (spec lvl globals)
  (let ((text (compile-spec-type< spec globals)))
    (output "~&typedef ~A;~%" text)))

(defun compile-enum (spec lvl globals)
  (let ((name         (name spec))
	(is-anonymous (anonymous spec))
	(counter      1)
	(count        (hash-table-count (inners spec)))
	(locals       (copy-specifiers globals)))
    (maphash #'(lambda (in-name in-spec)
		 (case (construct in-spec)
		   ('|@VARIABLE| (setf (gethash in-name locals) in-spec))
		   (otherwise nil)))
	     (inners spec))
    (output "~&~A~:[typedef enum ~A~;enum~] {~%" (indent lvl) is-anonymous name)
    (maphash #'(lambda (in-name in-spec)
		 (case (construct in-spec)
		   ('|@VARIABLE|
		    (output "~&~A~A~:[ = ~A~;~*~]~:[~;,~]~%"
		      (indent (+ 1 lvl)) in-name (null (default in-spec))
		      (compile-form< (default in-spec) locals) (< counter count)))
		   (otherwise nil))
		 (incf counter))
	     (inners spec))
    (output "~&}~:[ ~A~;~]; /* ~A */~%" is-anonymous name name)))

(defun compile-struct (spec lvl globals)
  (let ((name         (name spec))
	(is-anonymous (anonymous spec))
	(declares     '())
	(locals       (copy-specifiers globals)))
    (maphash #'(lambda (in-name in-spec)
		 (case (construct in-spec)
		   ('|@VARIABLE| (setf (gethash in-name locals) in-spec))
		   ('|@FUNCTION| (setf (gethash in-name locals) in-spec))
		   ('|@ENUM|
		    (unless (anonymous spec) (setf (gethash in-name locals) in-spec))
		    (maphash #'(lambda (k v) (setf (gethash k locals) v)) (inners in-spec)))
		   ('|@STRUCT|
		    (setf (gethash in-name locals) in-spec)
		    (maphash #'(lambda (k v)
				 (when (eql (construct v) '|@DECLARES|) (setf (gethash k locals) v)))
			     (inners in-spec)))
		   ('|@UNION|
		    (setf (gethash in-name locals) in-spec)
		    (maphash #'(lambda (k v)
				 (when (eql (construct v) '|@DECLARES|) (setf (gethash k locals) v)))
			     (inners in-spec)))
		   (otherwise nil)))
	     (inners spec))
    (output "~&~A~:[typedef struct ~A~;struct~] {~%" (indent lvl) is-anonymous name)
    (maphash #'(lambda (in-name in-spec)
		 (case (construct in-spec)
		   ('|@PREPROC|  (compile-preprocessor in-spec (+ 1 lvl) locals))
		   ('|@VARIABLE| (compile-variable     in-spec (+ 1 lvl) locals))
		   ('|@ENUM|     (compile-enum         in-spec (+ 1 lvl) locals))
		   ('|@STRUCT|   (compile-struct       in-spec (+ 1 lvl) locals))
		   ('|@UNION|    (compile-union        in-spec (+ 1 lvl) locals))
		   ('|@DECLARES| (push (name in-spec) declares))
		   (otherwise nil)))
	     (inners spec))
    (output "~&~A}~:[ ~A~;~*~]~:[ ~;~]" (indent lvl) is-anonymous name (null declares))
    (output "~{~A~^, ~}; /* ~A */~%" declares name)))

(defun compile-union (spec lvl globals)
  (let ((name         (name spec))
	(is-anonymous (anonymous spec))
	(declares     '())
	(locals       (copy-specifiers globals)))
    (maphash #'(lambda (in-name in-spec)
		 (case (construct in-spec)
		   ('|@VARIABLE| (setf (gethash in-name locals) in-spec))
		   ('|@STRUCT|
		    (setf (gethash in-name locals) in-spec)
		    (maphash #'(lambda (k v)
				 (when (eql (construct v) '|@DECLARES|) (setf (gethash k locals) v)))
			     (inners in-spec)))
		   ('|@UNION|
		    (setf (gethash in-name locals) in-spec)
		    (maphash #'(lambda (k v)
				 (when (eql (construct v) '|@DECLARES|) (setf (gethash k locals) v)))
			     (inners in-spec)))
		   (otherwise nil)))
	     (inners spec))
    (output "~&~A~:[typedef union ~A~;union~] {~%" (indent lvl) is-anonymous name)
    (maphash #'(lambda (in-name in-spec)
		 (case (construct in-spec)
		   ('|@PREPROC|  (compile-preprocessor in-spec (+ 1 lvl) locals))
		   ('|@VARIABLE| (compile-variable     in-spec (+ 1 lvl) locals))
		   ('|@STRUCT|   (compile-struct       in-spec (+ 1 lvl) locals))
		   ('|@UNION|    (compile-union        in-spec (+ 1 lvl) locals))
		   ('|@DECLARES| (push (name in-spec) declares))
		   (otherwise nil)))
	     (inners spec))
    (output "~&~A}~:[ ~A~;~*~]~:[ ~;~]" (indent lvl) is-anonymous name (null declares))
    (output "~{~A~^, ~}; /* ~A */~%" declares name)))

(defun compile-guard (spec lvl globals)
  (let ((name (name spec)))
    (output "~&#ifndef ~A~%" name)
    (output "~&#define ~A~%" name)
    (maphash #'(lambda (in-name in-spec)
		 (case (construct in-spec)
		   ('|@PREPROC|  (compile-preprocessor in-spec lvl globals))
		   ('|@INCLUDE|  (compile-include      in-spec lvl globals))
		   ('|@TYPEDEF|  (compile-typedef      in-spec lvl globals))
		   ('|@VARIABLE| (compile-variable     in-spec lvl globals))
		   ('|@FUNCTION| (compile-function     in-spec lvl globals))
		   ('|@ENUM|     (compile-enum         in-spec lvl globals))
		   ('|@STRUCT|   (compile-struct       in-spec lvl globals))
		   ('|@UNION|    (compile-union        in-spec lvl globals))
		   ('|@GUARD|    (compile-guard        in-spec lvl globals))
		   (otherwise nil)))
	     (inners spec))    
    (output "~&#endif /* ~A */ ~%" name)))
