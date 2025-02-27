(in-package :lcc)

(defvar *output* t)

(defvar *unaries* '(|+| |-| |++| |++#| |--| |--#| |~| |!| |not| |*| |contentof| |&| |addressof|))
(defvar *operators* '(|+| |-| |*| |/| |%| |==| |!=| |>| |<| |>=| |<=| |^| |xor| |<<| |>>|
		              |&&| |and| |or| |&| |bitand| |bitor|))
(defvar *assignments* '(|=| |+=| |-=| |*=| |/=| |%=| |<<=| |>>=|))
(defvar *modifiers* '(|&| |*| |**|))

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

;; storing line num and col num of target's ASTs
(defparameter *ast-lines* (make-hash-table :test 'equal))

(defun ast-key< (line-n col-n)
  (format nil "~D:~D" line-n col-n))

(defmacro set-ast-line (out)
  (let ((line-n (gensym))
        (col-n  (gensym)))
  `(multiple-value-bind (,line-n ,col-n) ,out
     (setf (gethash (ast-key< ,line-n ,col-n) *ast-lines*) (cdr (backtrace))))))

(defun hash-table-keys< (ht)
  (let ((keys nil))
    (maphash
     #'(lambda (k v)
         (declare (ignore v))
         (push k keys))
     ht)
    keys))

(defun backtrace ()
  (let ((bt (list (or *compile-file-truename* *load-truename*) (uiop:command-line-arguments))))
    (dolist (trace (nthcdr 1 (sb-debug:list-backtrace)))
      (setq bt (append bt
                       (if (hash-table-p (car (last trace)))
                           (without-last trace)
                           trace)))
      (when (eq (car trace) 'COMPILE-TARGET) (return t)))
    bt))

(setf sb-ext:*invoke-debugger-hook*
      #'(lambda (&rest args)
          (format *error-output* ";~%")
          (format *error-output* "; lcc error:~%")
          (format *error-output* ";~%")
          (format *error-output* "; ~A~%" (car args))
          (format *error-output* ";~%")
          (format *error-output* "; compiling ~S ~A ~%" (or *compile-file-truename* *load-truename*) (uiop:command-line-arguments))
          (format *error-output* ";~%")
          (format *error-output* "Backtrace:~%")
          (let ((counter 0))
            (setq *print-pretty* nil)
            (dolist (trace (sb-debug:list-backtrace))
              (format *error-output* "[~A] ~A~%" counter
                      (if (hash-table-p (car (last trace))) (without-last trace) trace))
              (when (eq (car trace) 'COMPILE-TARGET) (return t))
              (setq counter (1+ counter)))
            (setq *print-pretty* t))
          (sb-ext:exit)))

(defun print-trace ()
  (format t "~A" (sb-debug:list-backtrace)))

(defun display (&rest args)
  (format t "~{~A~^ ~}" args))

(defparameter *line-num* (let ((count 1))
                           #'(lambda (step &key reset)
                               (if reset
                                   (setf count 1)
                                   (setf count (+ count step))))))

(defparameter *col-num* (let ((count 1))
                          #'(lambda (step &key reset)
                              (if reset
                                  (setf count 1)
                                  (setf count (+ count step))))))

(defvar *new-line* (format nil "~%"))

(defun output (ctrl &rest rest)
  (let ((line-n (funcall *line-num* 0))
        (col-n  (funcall *col-num* 0))
        (result (apply 'format (append (list nil ctrl) rest))))
    (apply 'format (list *output* result))
    (let* ((trimmed (string-left-trim " " result))
           (space-count (- (length result) (length trimmed)))
           (index (search *new-line* result :from-end t))
          (line-count (str:count-substring *new-line* result)))
      (funcall *line-num* line-count)
      (if index (progn
                  (let ((last-line (str:substring (1+ index) t result)))
                    (setq trimmed (string-left-trim " " last-line))
                    (funcall *col-num* 0 :reset t)
                    (funcall *col-num* (+ (1- (- (length result) index)) (length trimmed)))))
          (funcall *col-num* (length result)))
      (display line-n (+ col-n space-count) result)
      (values line-n col-n))))

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
  (when (and (listp desc) (key-eq (first desc) (intent "[")) (key-eq (car (last desc)) (intent "]"))) t))
