(in-package :lcc)

(defvar *output* t)

(defvar *unaries* '(|+| |-| |++| |++#| |--| |--#| |~| |not| |contentof| |addressof|))
(defvar *operators* '(|+| |-| |*| |/| |%| |==| |!=| |>| |<| |>=| |<=| |^| |xor| |<<| |>>|
		      |and| |or| |bitand| |bitor| |->| |$|))
(defvar *assignments* '(|+=| |-=| |*=| |/=| |%=| |<<=| |>>=|))
(defvar *modifiers* '(|*| |**|))

(defun reving (list result)
  (cond ((consp list) (reving (cdr list) (cons (car list) result)))
        ((null list) result)
        (t (cons list result))))

(defun without-last(list)
  (reving (cdr (reving list '())) '()))

(defun set-nth (list n val)
  (if (> n 0)
      (cons (car list)
            (set-nth (cdr list) (1- n) val))
    (cons val (cdr list))))

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

(defun display (&rest args)
  (format t "~&~{~A~^ ~}~%" args))

(defmacro output (ctrl &rest rest)
  `(format *output* ,ctrl ,@rest))

(defmacro with-lcc-readtable (&rest code)
  `(let ((*readtable* (copy-readtable)))
     (set-dispatch-macro-character
	 #\# #\t #'(lambda (stream char1 char2)
		     (declare (ignore stream char1 char2))
		     (read-from-string "true")))
     (set-dispatch-macro-character
	 #\# #\f #'(lambda (stream char1 char2)
		     (declare (ignore stream char1 char2))
		     (read-from-string "false")))
     (set-macro-character
	 #\{ #'(lambda (stream char)
		 (declare (ignore char))
		 (read-delimited-list #\} stream t)))
     (set-macro-character #\} (get-macro-character #\)) nil)
     (set-macro-character
	 #\" #'(lambda (stream char)
		 (declare (ignore char))
		 (with-output-to-string (out)
					(do ((char (read-char stream nil nil) (read-char stream nil nil)))
					    ((char= char #\") nil)
					    (write char :stream out :escape nil)))))
     (set-macro-character #\| nil nil)
     (setf (readtable-case *readtable*) :preserve)
     ,@code))

(defun read-lcc-file (path)
  (let ((targets '()))
    (with-open-file (file path)
      (with-lcc-readtable
	  (DO ((target (READ file) (READ file NIL NIL)))
	    ((NULL target) T)
	    (PUSH target targets))))
    (nreverse targets)))

(defun user-symbol (symbol)
  (with-lcc-readtable
      (READ-FROM-STRING symbol)))

(defun read-meta-file (path)
  (let ((meta-data nil))
    (with-open-file (file path)
      (setq meta-data (read file nil nil)))
    (unless (null meta-data) (eval meta-data))))

(defun write-meta-file (path meta-data)
  (with-open-file (file path
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
    (write-char #\' file)
    (write meta-data :stream file :pretty t)))

(defun import-spec (spec globals)
  (maphash #'(lambda (k v)
	       (case (construct v)
		 ('|@VARIABLE| (setf (gethash k globals) v))
		 ('|@FUNCTION| (setf (gethash k globals) v))
		 ('|@ENUM|     (setf (gethash k globals) v)
		  (maphash #'(lambda (n m) (setf (gethash n globals) m)) v))
		 ('|@STRUCT|   (setf (gethash k globals) v)
		  (maphash #'(lambda (n m)
			       (when (eql (construct m) '|@DECLARE|)
				 (setf (gethash n globals) m)))
			   v))
		 ('|@UNION|    (setf (gethash k globals) v)
		  (maphash #'(lambda (n m)
			       (when (eql (construct m) '|@DECLARE|)
				 (setf (gethash n globals) m)))
			   v))
		 ('|@GUARD|  (import-spec v globals))
		 (otherwise t)))
	   (slot-value spec 'inners)))

(defun import-meta-file (path globals)
  (let ((spec (load-specifier (read-meta-file path))))
    (case (construct spec)
      ('|@TARGET| (import-spec spec globals))
      ('|@CLASS|  t)
      (otherwise (error (format nil "invalid meta file root speficier ~A in file "
				(construct spec) path))))
    spec))

(defun indent (lvl)
  (make-string (* lvl 2) :initial-element #\Space))

(defun is-name (name)
  (when (symbolp name)
    (let ((name (symbol-name name)))
      (cond ((string= name "const") nil)
	    ((not (find (char name 0) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")) nil)
	    (t (progn
		 (dotimes (i (- (length name) 1))
		   (unless (find (char name (+ i 1)) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890")
		     (return-from is-name nil)))
		 t))))))

(defun is-symbol (name)
  (when (symbolp name)
    (let ((name (symbol-name name)))
      (cond ((string= name "const") nil)
	    (t (progn
		 (dotimes (i (length name))
		   (unless (find (char name i) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890")
		     (return-from is-symbol nil)))
		 t))))))

(defun key-eq (symbol1 symbol2)
  (and (symbolp symbol1) (symbolp symbol2) (string-equal (symbol-name symbol1) (symbol-name symbol2))))

(defun is-array (desc)
  (when (and (listp desc) (key-eq (first desc) '[) (key-eq (car (last desc)) '])) t))

(set-macro-character
 #\[ #'(lambda (stream char)
	 (declare (ignore char))
	 (list '[ (car (read-delimited-list #\] stream t)) '])))

(set-macro-character #\] (get-macro-character #\)) nil)

(defmacro filter (&rest rest)
  `(remove-if-not ,@rest))

(defun extract-include-name< (full-name)
  (if (listp full-name)
      (car (last full-name))
    full-name))

(defun include-header< (full-name)
  (if (listp full-name)
      (format nil "\"~{~A~^/~}\"" full-name)
    (format nil "~A" full-name)))

(defun include-path< (full-name)
  (if (listp full-name)
      (format nil "~{~A~^/~}" (butlast full-name))
    ""))

(defun extract-class-name< (full-name)
  (if (listp full-name)
      (car (last full-name))
    full-name))

(defun class-header< (full-name)
  (if (listp full-name)
      (format nil "~{~A~^/~}.h" full-name)
    (format nil "~A.h" full-name)))

(defun class-path< (full-name)
  (if (listp full-name)
      (format nil "~{~A~^/~}" (butlast full-name))
    ""))

(defun class-lib< (full-name)
  (if (listp full-name)
      (format nil "~{~A~^-~}" full-name)
    (format nil "~A" full-name)))

(defun class-path-lib< (full-name)
  (if (listp full-name)
      (values
       (format nil "~{~A~^/~}" (subseq full-name 0 (- (length full-name) 1)))
       (class-lib< full-name))
    (values
     (format nil "" full-name)
     (class-lib< full-name))))

(defun guard-name< (full-name)
  (if (listp full-name)
      (format nil "__~{~A~^_~}_H__" (mapcar #'string-upcase full-name))
    (format nil "__~A_H__" (string-upcase full-name))))

(defun class-name< (full-name)
  (if (listp full-name)
      (format nil "__~{~A~^_~}__" full-name)
    (format nil "__~A__" full-name)))

(defun method-name< (full-name method-name)
  (if (listp full-name)
      (format nil "__~{~A~^_~}_~A__" full-name method-name)
    (format nil "__~A_~A__" full-name method-name)))

(defun static-method-name< (full-name method-name)
  (if (listp full-name)
      (format nil "__~{~A~^_~}_~A__" full-name method-name)
    (format nil "__~A_~A__" full-name method-name)))

(defun static-class-name< (full-name)
  (if (listp full-name)
      (format nil "__~{~A~^_~}_static__" full-name)
    (format nil "__~A_static__" full-name)))

(defun static-class-variable-name< (full-name)
  (if (listp full-name)
      (format nil "__~{~A~^_~}_class__" full-name)
    (format nil "__~A_class__" full-name)))

(defun static-variable-name< (full-name)
  (if (listp full-name)
      (format nil "__~{~A~^_~}_static_i__" full-name)
    (format nil "__~A_static_i__" full-name)))

(defun unsigned-int-p (sym)
  (when (symbolp sym)
    (let ((str (coerce (symbol-name sym) 'list)))
      (and (every #'digit-char-p (butlast str))
	(eq (car (last str)) #\U)))))

(defun long-p (sym)
  (when (symbolp sym)
    (let ((str (coerce (symbol-name sym) 'list)))
      (and (every #'digit-char-p (butlast str))
	(eq (car (last str)) #\L)))))

(defun unsigned-long-p (sym)
  (when (symbolp sym)
    (let ((str (coerce (symbol-name sym) 'list)))
      (and (every #'digit-char-p (butlast (butlast str)))
	(eq (car (subseq (reverse str) 1 2)) #\U)
	(eq (car (last str)) #\L)))))
