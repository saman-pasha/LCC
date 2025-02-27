
(defun lcc-add-keywords (face-name keyword-rules)
  (let* ((keyword-list (mapcar #'(lambda (x)
				   (symbol-name (cdr x)))
			       keyword-rules))
	 (keyword-regexp (concat "(\\("
				 (regexp-opt keyword-list)
				 "\\)[ \t\n]*")))
    (font-lock-add-keywords 'lisp-mode
			    `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
	    (put (cdr x)
		 'lisp-indent-function
		 (car x)))
	keyword-rules))
 
(lcc-add-keywords
 'font-lock-keyword-face
 '((1 . format)
   (1 . code)
   (1 . target)
   (1 . guard)
   (1 . include)
   (1 . variable)
   (1 . function)
   (1 . returns)
   (1 . enum)
   (1 . struct)
   (1 . union)
   (1 . member)
   (1 . method)
   (1 . declares)
   (1 . typedef)
   (1 . set)
   (1 . nth)
   (1 . not)
   (1 . and)
   (1 . or)
   (1 . bitand)
   (1 . bitor)
   (1 . xor)
   (1 . contentof)
   (1 . addressof)
   (1 . sizeof)
   (1 . cast)
   (1 . switch)
   (1 . case)
   (1 . default)
   (1 . while)
   (1 . for)
   (1 . for-each)
   (1 . new)
   (1 . printf)
   (1 . scanf)
   ))

(defun lcc-add-attributes (face-name keyword-rules)
  (let* ((keyword-list (mapcar #'(lambda (x)
				   (symbol-name (cdr x)))
			       keyword-rules))
	 (keyword-regexp (concat "{\\("
				 (regexp-opt keyword-list)
				 "\\)[ \t\n]*")))
    (font-lock-add-keywords 'lisp-mode
			    `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
	    (put (cdr x)
		 'lisp-indent-function
		 (car x)))
	keyword-rules))
 
(lcc-add-attributes
 'font-lock-preprocessor-face
 '((1 . static)
   (1 . declare)
   (1 . inline)
   (1 . extern)
   (1 . register)
   (1 . auto)
   ))

(defun lcc-add-types (face-name keyword-rules)
  (let* ((keyword-list (mapcar #'(lambda (x)
				   (symbol-name (cdr x)))
			       keyword-rules))
	 (keyword-regexp (concat "[ \t\n(]\\("
				 (regexp-opt keyword-list)
				 "\\)[ \t\n)]")))
    (font-lock-add-keywords 'lisp-mode
			    `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
	    (put (cdr x)
		 'lisp-indent-function
		 (car x)))
	keyword-rules))
 
(lcc-add-types
 'font-lock-type-face
 '((1 . this)
   (1 . void)
   (1 . unsigned)
   (1 . char)
   (1 . uchar)
   (1 . short)
   (1 . ushort)
   (1 . int)
   (1 . uint)
   (1 . long)
   (1 . ulong)
   (1 . llong)
   (1 . ullong)
   (1 . float)
   (1 . double)
   (1 . real)
   (1 . int8_t)
   (1 . uint8_t)
   (1 . int16_t)
   (1 . uint16_t)
   (1 . int32_t)
   (1 . uint32_t)
   (1 . int64_t)
   (1 . uint64_t)
   (1 . int_least8_t)
   (1 . uint_least8_t)
   (1 . int_least16_t)
   (1 . uint_least16_t)
   (1 . int_least32_t)
   (1 . uint_least32_t)
   (1 . int_least64_t)
   (1 . uint_least64_t)
   (1 . int_fast8_t)
   (1 . uint_fast8_t)
   (1 . int_fast16_t)
   (1 . uint_fast16_t)
   (1 . int_fast32_t)
   (1 . uint_fast32_t)
   (1 . int_fast64_t)
   (1 . uint_fast64_t)
   (1 . __int128)
   (1 . i8)
   (1 . u8)
   (1 . i16)
   (1 . u16)
   (1 . i32)
   (1 . u32)
   (1 . i64)
   (1 . u64)
   (1 . i128)
   (1 . u128)
   (1 . intmax_t)
   (1 . intptr_t)
   (1 . bool)
   (1 . true)
   (1 . false)
   (1 . nil)
   ))

(font-lock-add-keywords
 'lisp-mode
 '(("(\\(@\\(\\sw\\|\\s_\\)+\\)[ \t\n]*"
    (1 'font-lock-preprocessor-face))))

(font-lock-add-keywords
 'lisp-mode
 '(("(guard[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)[ \t\n]*"
    (1 'font-lock-preprocessor-face))))

(font-lock-add-keywords
 'lisp-mode
 '(("(function[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)[ \t\n]*"
    (1 'font-lock-function-name-face))))

(font-lock-add-keywords
 'lisp-mode
 '(("(enum[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)[ \t\n]*"
    (1 'font-lock-type-face))))

(font-lock-add-keywords
 'lisp-mode
 '(("(struct[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)[ \t\n]*"
    (1 'font-lock-type-face))))

(font-lock-add-keywords
 'lisp-mode
 '(("(union[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)[ \t\n]*"
    (1 'font-lock-type-face))))
