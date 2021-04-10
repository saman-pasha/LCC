(meta (void))

(meta (bool) ()
      (operator ==  ((bool x)) (returns bool))
      (operator !=  ((bool x)) (returns bool))
      (operator !   () (returns bool))
      (operator bitand ((bool x)) (returns bool))
      (operator bitor  ((bool x)) (returns bool)))

(meta (char) ()
      (operator +   () (returns char))
      (operator ++  () (returns char))
      (operator ++# () (returns char))
      (operator +   ((char x)) (returns char))
      (operator -   () (returns char))
      (operator --  () (returns char))
      (operator --# () (returns char))
      (operator -   ((char x)) (returns char))
      (operator *   ((char x)) (returns char))
      (operator /   ((char x)) (returns char))
      (operator %   ((char x)) (returns char))
      (operator ==  ((char x)) (returns bool))
      (operator !=  ((char x)) (returns bool))
      (operator >   ((char x)) (returns char))
      (operator >=  ((char x)) (returns char))
      (operator <   ((char x)) (returns char))
      (operator <=  ((char x)) (returns char))
      (operator ~   () (returns char))
      (operator and ((char x)) (returns char))
      (operator or  ((char x)) (returns char))
      (operator ^   ((char x)) (returns char))
      (operator <<  ((char x)) (returns char))
      (operator >>  ((char x)) (returns char)))

(meta (int) ()
      (int ((size_t)))
      (operator +   () (returns int))
      (operator ++  () (returns int))
      (operator ++# () (returns int))
      (operator +   ((int x)) (returns int))
      (operator -   () (returns int))
      (operator --  () (returns int))
      (operator --# () (returns int))
      (operator -   ((int x)) (returns int))
      (operator *   ((int x)) (returns int))
      (operator /   ((int x)) (returns int))
      (operator %   ((int x)) (returns int))
      (operator ==  ((int x)) (returns bool))
      (operator !=  ((int x)) (returns bool))
      (operator >   ((int x)) (returns int))
      (operator >=  ((int x)) (returns int))
      (operator <   ((int x)) (returns int))
      (operator <=  ((int x)) (returns int))
      (operator ~   () (returns int))
      (operator and ((char x)) (returns char))
      (operator or  ((char x)) (returns char))
      (operator ^   ((int x)) (returns int))
      (operator <<  ((int x)) (returns int))
      (operator >>  ((int x)) (returns int)))

(meta (size_t) ()
      (size_t ((int)))
      (operator +   () (returns size_t))
      (operator ++  () (returns size_t))
      (operator ++# () (returns size_t))
      (operator +   ((size_t x)) (returns size_t))
      (operator -   () (returns size_t))
      (operator --  () (returns size_t))
      (operator --# () (returns size_t))
      (operator -   ((size_t x)) (returns size_t))
      (operator *   ((size_t x)) (returns size_t))
      (operator /   ((size_t x)) (returns size_t))
      (operator %   ((size_t x)) (returns size_t))
      (operator ==  ((size_t x)) (returns bool))
      (operator !=  ((size_t x)) (returns bool))
      (operator >   ((size_t x)) (returns size_t))
      (operator >=  ((size_t x)) (returns size_t))
      (operator <   ((size_t x)) (returns size_t))
      (operator <=  ((size_t x)) (returns size_t))
      (operator ~   () (returns size_t))
      (operator and ((char x)) (returns char))
      (operator or  ((char x)) (returns char))
      (operator ^   ((size_t x)) (returns size_t))
      (operator <<  ((size_t x)) (returns size_t))
      (operator >>  ((size_t x)) (returns size_t)))

(target ("cstddecl.h") (:std #t)
	(import (void)   :as void)
	(import (char)   :as char)
	(import (int)    :as int)
	(import (size_t) :as size_t)

	(guard __CSTDDECL_H__
	  {declare} (function printf ((const char * format) (***)) (returns int))
	  {declare} (function malloc ((size_t size)) (returns void *))
	  {declare} (function strncpy ((char * to) (const char * from) (size_t count)) (returns char *))
	  {declare} (function free ((void * ptr)) (returns void))
	  {inline}  (function add ((int x) (int y)) (returns int) (return (+ x y)))
	  ) ; __CSTDDECL_H__
  ) ; cstddecl.h

