;;; methods and receivers

;;;; declaration of sample struct
(target "method.h" ()
        ;; sample struct representing members and methods
        (struct Sample
                (member int AttrA)
                (member char * AttrB))

        ;; receivers of Sample struct
        {declare} (method Sample->PrintAttrA ())
        {declare} (method Sample->PrintAttrB ())
        {declare} (method Sample->PrintBoth ())

        ) ; target

;;;; definition of sample struct methods
(target "method.c" (:std #t :compile #t)
        (include "method.h")

        ;; methods of Sample struct which access to Sample members and other methods

        (method Sample->PrintAttrA ()
                (printf "AttrA: %d" ($ this AttrA)))

        (method Sample->PrintAttrB ()
                (printf "AttrB: %s" ($ this AttrB)))

        (method Sample->PrintBoth ()
                (-> this PrintAttrA)
                (-> this PrintAttrB))

        (function main () (returns int)
                  (let ((Sample s . '{ 100 "domain.com" }))
                    (-> s PrintBoth)
                    (return 0)))
        ) ; target

;; glibtool --tag=CC --mode=compile clang -g -O -c method.c -Xclang -ast-dump
;; glibtool --tag=CC --mode=compile clang -Xclang -ast-dump -g -O -c method.c
;; clang -c method.c -ast-dump
;; clang -c method.c -Xclang -ast-dump | grep -n _PrintAttrA
;; clang -c method.c -Xclang -ast-dump | grep -n line:24:3 -A 3
;; clang -c method.c -Xclang -ast-dump | grep -n line:30:5 -A 4
