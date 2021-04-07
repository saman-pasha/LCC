(target ("cstddecl.h") (:std #t)
  {declare} (function printf ((const char * format) (***)) (returns int))
  {declare} (function malloc ((size_t size)) (returns void *))
  
  ) ; cstddecl.h
