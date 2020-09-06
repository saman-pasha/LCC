# lcc
Lisp C Compiler, Lisp-like syntax for writing C code and some pointer managements
## Identifiers
```lisp
(variable int amount)
(variable double total)
```
```c
int amount;
double total;
```
## Constants
```lisp
(variable const int SIDE . 10)
```
```c
const int SIDE = 10;
```
## Operators
### Arithmetic
lcc Operator | C Operator
------------ | ----------
`+`|`+`
`-`|`-`
`*`|`*`
`/`|`/`
`%`|`%`
```lisp
(set total (+ total amount))

(let ((int i . 3)
      (int j . 7)
      (int k))
  (set k (+ i j)))
```
```c
total = total + amount;

{
  int i = 3, j = 7, k;
  k = i + j;
}
```
### Increment and Decrement
lcc Operator | C Operator
------------ | ----------
`++`|`++`
`--`|`--`
`++#`|`++`
`--#`|`--`
```lisp
(target "main.c" 
  ()
  (include <stdio.h>)

  (function main ()
    (let ((int a . 5)
          (int b . 5))
          
      ;; Print them and decrementing each time.
      ;; Use postfix mode for a and prefix mode for b.
      
      (printf "\n%d %d" (++# a) (-- b))
      (printf "\n%d %d" (++# a) (-- b))
      (printf "\n%d %d" (++# a) (-- b))
      (printf "\n%d %d" (++# a) (-- b))
      (printf "\n%d %d" (++# a) (-- b)))))
```
```c
#include <stdio.h>

void main()
{
  {
    int a = 5, b = 5;
    
    //Print them and decrementing each time.
    //Use postfix mode for a and prefix mode for b.
    
    printf("\n%d %d", a--, --b);
    printf("\n%d %d", a--, --b);
    printf("\n%d %d", a--, --b);
    printf("\n%d %d", a--, --b);
    printf("\n%d %d", a--, --b);
  }
}
```
### Relational
lcc Operator | C Operator
------------ | ----------
`==`|`==`
`!=`|`!=`
`>`|`>`
`<`|`<`
`>=`|`>=`
`<=`|`<=`
### Logical
lcc Operator | C Operator
------------ | ----------
`and`|`&&`
`&&`|`&&`
`or`|`\|\|`
`\|\|`|`\|\|`
`not`|`!`
`!`|`!`
### Bitwise
lcc Operator | C Operator
------------ | ----------
`<<`|`<<`
`>>`|`>>`
`~`|`~`
`bitand`|`&`
`&`|`&`
`xor`|`^`
`^`|`^`
`bitor`|`\|`
`\|`|`\|`
### Assignment
lcc Operator | C Operator
------------ | ----------
`=`|`=`
`+=`|`+=`
`-=`|`-=`
`*=`|`*=`
`/=`|`/=`
`%=`|`%=`
`<<=`|`<<=`
`>>=`|`>>=`
### Conditional
lcc Operator | C Operator
------------ | ----------
`?`|`?=`
```lisp
(set a (? (== b 2) 20 30))
```
```c
a = (b == 2) ? 20 : 30;
```
### Special
lcc Operator | C Operator
------------ | ----------
`sizeof`|`sizeof()`
`addressof`|`&`
`contentof`|`*`

