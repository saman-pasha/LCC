(in-package :lcc)

(defun specify-target (target)
  (let* ((name    (nth 1 target))
	 (args    (nth 2 target))
	 (clauses (nthcdr 3 target))
	 (target-specifier (make-specifier name '|@TARGET| nil nil nil nil nil nil args)))
    (format t "lcc: specifying target ~A~%" name)
    (unless (zerop (mod (length args) 2)) (error (format nil "wrong target features ~A" name)))
    (let ((attributes '()))
      (dolist (clause clauses)
	(if (consp clause)
	    (let ((construct (car clause)))
	      (cond ((find (char (symbol-name construct) 0) "@#")
		     (specify-preprocessor clause attributes 0 (inners target-specifier))
		     (setq attributes '()))
		    ((key-eq construct '|static|)   (push clause attributes))
		    ((key-eq construct '|declare|)  (push clause attributes))
		    ((key-eq construct '|inline|)   (push clause attributes))
		    ((key-eq construct '|auto|)     (push clause attributes))
		    ((key-eq construct '|register|) (push clause attributes))
		    ((key-eq construct '|extern|)   (push clause attributes))
		    ((key-eq construct '|include|)
		     (specify-include  clause attributes 0 (inners target-specifier))
		     (setq attributes '()))
		    ((key-eq construct '|guard|)
		     (specify-guard    clause attributes 0 (inners target-specifier))
		     (setq attributes '()))
		    ((key-eq construct '|variable|)
		     (specify-variable clause attributes 0 (inners target-specifier))
		     (setq attributes '()))
		    ((key-eq construct '|function|)
		     (specify-function clause attributes 0 (inners target-specifier))
		     (setq attributes '()))
		    ((key-eq construct '|enum|)
		     (specify-enum     clause attributes 0 (inners target-specifier))
		     (setq attributes '()))
		    ((key-eq construct '|struct|)
		     (specify-struct   clause attributes 0 (inners target-specifier))
		     (setq attributes '()))
		    ((key-eq construct '|union|)
		     (specify-union    clause attributes 0 (inners target-specifier))
		     (setq attributes '()))
		    ((key-eq construct '|typedef|)
		     (specify-typedef  clause attributes 0 (inners target-specifier))
		     (setq attributes '()))
		    (t (error (format nil "unknown clause ~A in target ~A" construct name)))))
	  (error (format nil "syntax error ~A" clause)))))
    target-specifier))

(defun compile-target (spec globals)
  (let ((file (format nil "~A" (name spec)))
	(args (attrs spec)))
    (case file
      ((|t|) (setq *output* t))
      (otherwise (setq *output*
		       (open file
			     :direction :output
			     :if-does-not-exist :create
			     :if-exists :supersede))))
    (format t "lcc: compiling target ~A~%" file)
    (unwind-protect
	(progn
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
	  (maphash #'(lambda (in-name in-spec)
		       (case (construct in-spec)
			 ('|@PREPROC|  (compile-preprocessor in-spec 0 globals))
			 ('|@INCLUDE|  (compile-include      in-spec 0 globals))
			 ('|@TYPEDEF|  (compile-typedef      in-spec 0 globals))
			 ('|@VARIABLE| (compile-variable     in-spec 0 globals))
			 ('|@FUNCTION| (compile-function     in-spec 0 globals))
			 ('|@ENUM|     (compile-enum         in-spec 0 globals))
			 ('|@STRUCT|   (compile-struct       in-spec 0 globals))
			 ('|@UNION|    (compile-union        in-spec 0 globals))
			 ('|@GUARD|    (compile-guard        in-spec 0 globals))
			 (otherwise nil)))
		   (inners spec))
	  (output "~%")
	  (close *output*)
	  (dotimes (i (length args))
	    (when (zerop (mod i 2))
	      (when (key-eq (nth i args) ':|compile|)
		(let* ((command   (getf *configs* 'compiler))
		       (program   (car command))
		       (arguments (cdr command))
		       (custom    (nth (+ i 1) args)))
		  (unless (key-eq custom '|false|)
		    (progn
		      (when (key-eq custom '|true|) (setq custom (list "-c" file)))
		      (uiop:run-program `(,program ,@arguments ,@custom) :input nil :output t :error-output t)))))
	      (when (key-eq (nth i args) ':|link|)
		(let* ((command   (getf *configs* 'linker))
		       (program   (car command))
		       (arguments (cdr command))
		       (custom    (nth (+ i 1) args)))
		  (unless (key-eq custom '|false|)
		    (uiop:run-program `(,program ,@arguments ,@custom) :input nil :output t :error-output t)))))))
      (progn
	(if (key-eq file '|t|)
	    (setq *output* t)
	  (close *output*))))
    (setq *output* t)))
