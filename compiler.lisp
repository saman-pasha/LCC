(in-package :lcc)

;; IR Intermediate Representation
(defun create-globals (ir &optional (globals (make-hash-table :test 'eql)))
  (maphash #'(lambda (name spec)
	           (case (construct spec)
		         (|@VARIABLE| (setf (gethash name globals) spec))
		         (|@FUNCTION| (setf (gethash name globals) spec))
		         (|@METHOD|   (setf (gethash name globals) spec))
		         (|@TYPEDEF|  (setf (gethash name globals) spec))
		         (|@ENUM|
		          (unless (anonymous spec) (setf (gethash name globals) spec))
		          (maphash #'(lambda (k v) (setf (gethash k globals) v)) (inners spec)))
		         (|@STRUCT|
		          (setf (gethash name globals) spec)
		          (maphash #'(lambda (k v)
			                   (when (eql (construct v) '|@DECLARES|) (setf (gethash k globals) v)))
			               (inners spec)))
		         (|@UNION|
		          (setf (gethash name globals) spec)
		          (maphash #'(lambda (k v)
			                   (when (eql (construct v) '|@DECLARES|) (setf (gethash k globals) v)))
			               (inners spec)))
		         (|@GUARD| (create-globals spec globals))
		         (otherwise nil)))
	       (inners ir))
  globals)

;; AST Abstract Syntax Tree
(defun compile-ast (targets)
  (dolist (target targets)
    (let ((name    (car target))
	      (ir      nil)
	      (globals nil))
	  (cond ((key-eq name '|target|)
	         (setq ir  (specify-target target))
	         (setq globals (create-globals ir))
             (let ((file (name ir))
                   (reached-translation-unit nil)
                   (reached-file nil)
                   (stdout (make-string-output-stream))
                   (stderr (make-string-output-stream)))
               (compile-target ir globals stdout stderr t)
               (with-input-from-string (out-stream (get-output-stream-string stdout))
                 (do ((s (read-line out-stream nil nil) (read-line out-stream nil nil)))
                     ((eql s nil))
                   (if reached-translation-unit
                       (if reached-file
                           (display "1>" s #\NewLine)  
                           (when (str:containsp file s)
                             (setq reached-file t)
                             (display "1>" s #\NewLine)))
                       (when (str:containsp "TranslationUnitDecl" s) (setq reached-translation-unit t)))
                   ))
               (with-input-from-string (err-stream (get-output-stream-string stderr))
                 (do ((s (read-line err-stream nil nil) (read-line err-stream nil nil)))
                     ((eql s nil))
                   (display "2>" s #\NewLine)
                   )
                 ))
	         (compile-target ir globals *standard-output* *error-output* nil))
	        ((key-eq name '|class|)
	         (setq ir  (specify-class  target))
	         (setq globals (create-globals ir))
	         (format t "lcc: globals in ~A~%" (cadr target))
	         (print-specifiers  globals)
	         (compile-class  ir globals))
	        (t (error (format nil "target or class is missing for ~A" name)))))))

(defun compile-lcc-file (file-name)
  (let ((file-path (make-pathname :directory (pathname-directory file-name)))
        (rt (copy-readtable nil)))
    (ensure-directories-exist file-path)
    ;; (uiop:chdir file-path)
    (uiop:with-current-directory (file-path)
      (multiple-value-bind (function non-terminating-p)
          (get-macro-character #\| rt)
        (set-macro-character #\| nil nil)
        (compile-ast (read-file (file-namestring file-name)))
        (set-macro-character #\| function non-terminating-p)))))

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
 #\[ #'(lambda (stream char)
	     (declare (ignore char))
	     (list (intern "[") (car (read-delimited-list #\] stream t)) (intern "["))))

(set-macro-character #\] (get-macro-character #\)) nil)

(set-macro-character
 #\" #'(lambda (stream char)
	     (declare (ignore char))
	     (with-output-to-string (out)
		   (do ((char (read-char stream nil nil) (read-char stream nil nil)))
			   ((char= char #\") nil)
			 (write char :stream out :escape nil)))))
