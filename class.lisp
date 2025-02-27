(in-package :lcc)

(defun specify-class (class)
  (declare (ignore class))
  nil)

(defun compile-class (class globals)
  (let* ((name      (nth 1 class))
	     (meta-file (format nil "~A.lccmeta" name))
	     (decl-file (format nil "~A.h" name))
	     (defs-file (format nil "~A.c" name))
	     (args      (nth 2 class))
	     (clauses   (nthcdr 3 class)))
    (unless (zerop (mod (length args) 2)) (error (format nil "wrong class features ~A" name)))
    (format t "lcc: compiling class ~A~%" name)
    ;; meta
    (case name
	  ((|t|) (setq *output* t))
	  (otherwise (setq *output*
			           (open meta-file
				             :direction :output
				             :if-does-not-exist :create
				             :if-exists :supersede))))
    ;; decl
    (case name
	  ((|t|) (setq *output* t))
	  (otherwise (setq *output*
			           (open decl-file
				             :direction :output
				             :if-does-not-exist :create
				             :if-exists :supersede))))
    (dotimes (i (length args))
      (when (zerop (mod i 2))
	    (when (key-eq (nth i args) ':|std|)
	      (let ((custom (nth (+ i 1) args)))
	        (when (key-eq custom '|true|)
	          (output "#include <stdio.h>~%")
	          (output "#include <stddef.h>~%")
	          (output "#include <stdint.h>~%")
	          (output "#include <stdlib.h>~%")
	          (output "#include <stdbool.h>~%"))))))
    ;; defs
    (case name
	  ((|t|) (setq *output* t))
	  (otherwise (setq *output*
			           (open defs-file
				             :direction :output
				             :if-does-not-exist :create
				             :if-exists :supersede))))
    (unwind-protect
	     (let ((attributes '()))
	       (dolist (clause clauses)
	         (if (consp clause)
		         (let ((construct (car clause)))
		           (cond ((key-eq construct '|code|)     (output "~&~A~%" (compile-form< clause globals)))
			             ((key-eq construct '|static|)   (push clause attributes))
			             ((key-eq construct '|declare|)  (push clause attributes))
			             ((key-eq construct '|inline|)   (push clause attributes))
			             ((key-eq construct '|auto|)     (push clause attributes))
			             ((key-eq construct '|register|) (push clause attributes))
			             ((key-eq construct '|extern|)   (push clause attributes))
			             ((key-eq construct '|guard|)    (compile-guard    clause attributes 0))
			             ((key-eq construct '|include|)  (compile-include  clause attributes 0))
			             ((key-eq construct '|variable|) (compile-variable clause attributes 0) (setq attributes '()))
			             ((key-eq construct '|function|) (compile-function clause attributes 0) (setq attributes '()))
			             ((key-eq construct '|enum|)     (compile-enum     clause attributes 0) (setq attributes '()))
			             ((key-eq construct '|struct|)   (compile-struct   clause attributes 0) (setq attributes '()))
			             ((key-eq construct '|union|)    (compile-union    clause attributes 0) (setq attributes '()))
			             (t (error (format nil "unknown clause ~A in class ~A" construct meta-file)))))
	             (error (format nil "syntax error ~A" clause))))
	       (terpri *output*)
	       (close *output*))
      (progn
	    (if (key-eq meta-file '|t|)
	        (setq *output* t)
	        (close *output*))))
    (dotimes (i (length args))
      (when (zerop (mod i 2))
	    (when (key-eq (nth i args) ':|compile|)
	      (let* ((command   (getf *configs* 'compiler))
		         (program   (car command))
		         (arguments (cdr command))
		         (custom    (nth (+ i 1) args)))
	        (unless (key-eq custom '|false|)
	          (progn
		        (when (key-eq custom '|true|) (setq custom (list "-c" meta-file)))
		        (uiop:run-program `(,program ,@arguments ,@custom) :input nil :output *standard-output*)))))
	    (when (key-eq (nth i args) ':|link|)
	      (let* ((command   (getf *configs* 'linker))
		         (program   (car command))
		         (arguments (cdr command))
		         (custom    (nth (+ i 1) args)))
	        (unless (key-eq custom '|false|)
	          (uiop:run-program `(,program ,@arguments ,@custom) :input nil :output *standard-output*))))))
    (setq *output* t)))
