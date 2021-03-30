(in-package :lcc)

(defvar *output* t)

(defvar *unaries* '(|+| |-| |++| |++#| |--| |--#| |~| |!| |not| |*| |contentof| |&| |addressof|))
(defvar *operators* '(|+| |-| |*| |/| |%| |==| |!=| |>| |<| |>=| |<=| |^| |xor| |<<| |>>|
		      |&&| |and| |or| |&| |bitand| |bitor| |->| |$|))
(defvar *assignments* '(|=| |+=| |-=| |*=| |/=| |%=| |<<=| |>>=|))
(defvar *modifiers* '(|&| |*| |**|))

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
  (format t "~{~A~^ ~}" args))

(defmacro output (ctrl &rest rest)
  `(format *output* ,ctrl ,@rest))

(defun read-file (path)
  (let ((targets '()))
    (with-open-file (file path)
		    (let ((*readtable* (copy-readtable)))
		      (setf (readtable-case *readtable*) :preserve)
		      (DO ((target (READ file) (READ file NIL NIL)))
			((NULL target) T)
			(PUSH target targets))))
    (nreverse targets)))

(defun indent (lvl)
  (make-string (* lvl 2) :initial-element #\Space))

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

(defun key-eq (symbol1 symbol2)
  (and (symbolp symbol1) (symbolp symbol2) (string-equal (symbol-name symbol1) (symbol-name symbol2))))

(defun is-array (desc)
  (when (and (listp desc) (key-eq (first desc) '[) (key-eq (car (last desc)) '])) t))

(set-macro-character
 #\[ #'(lambda (stream char)
	 (declare (ignore char))
	 (list '[ (car (read-delimited-list #\] stream t)) '])))

(set-macro-character #\] (get-macro-character #\)) nil)
