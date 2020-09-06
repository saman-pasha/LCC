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
## Data Types
ANSI C provides three types of data types:

* Primary(Built-in) Data Types: void, int, char, double and float.
* Derived Data Types: Array, References, and Pointers.
* User Defined Data Types: Structure, Union, and Enumeration.

lcc supports declaration and definition of all ANCI C data types.
## Variable
### Free Variable Declaration and Initialization
```lisp
(variable int width)
(variable int height . 5)
(variable char letter . #\A)
(variable float age)
(variable float area)
(variable double d)

;; actual initialization
(set width 10)
(set age 26.5)
```
```c
int    width, height = 5;
char   letter = 'A';
float  age, area;
double d;

/* actual initialization */
width = 10;
age = 26.5;
```
### Scoped Variable Declaration and Initialization
```lisp
(let ((int width . 3)
      (int height . 4))
  (printf "area: %d" (* width height)))
```
```c
{
  int width = 3;
  int height = 4;
  printf("area: %d", width * height);
}
```
### Assignment
```lisp
(set width 60)
(set age 35)
```
```c
width = 60;
age = 31;
```
```lisp
(target "main.c"
  ()
  (include <stdio.h>)
  
  (function main ()
    (let ((int age . 33))
      (printf "I am %d years old.\n" age))))
```
```c
#include <stdio.h>

void main()
{
  {
    int age = 33;
    printf("I am %d years old.\n", age);
  }
}
```
## Type Casting
```lisp
(target "main.c"
  ()
  (include <stdio.h>)
  (function main ()
    (let ((float a))
      (set a (cast float (/ 15 6)))
      (printf "%f" a))))
```
```c
#include <stdio.h>
main ()
{
  {
    float a;
    a = (float) 15 / 6;
    printf("%f", a);
  }
}
```
