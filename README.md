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
`++`|prefix `++`
`--`|prefix `--`
`++#`|postfix `++`
`--#`|postfix `--`
```lisp
(target "main.c" 
  ()
  (include <stdio.h>)

  (function main ()
    (let ((int a . 5)
          (int b . 5))
          
      ;; Print them and decrementing each time.
      ;; Use postfix mode for a and prefix mode for b.
      
      (printf "\n%d %d" (--# a) (-- b))
      (printf "\n%d %d" (--# a) (-- b))
      (printf "\n%d %d" (--# a) (-- b))
      (printf "\n%d %d" (--# a) (-- b))
      (printf "\n%d %d" (--# a) (-- b)))))
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
`set`|`=`
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
lcc Data Type | C Data Type
------------- | -----------
`void`|`void`
`char`|`char`
`uchar`|`unsigned char`
`short`|`short`
`ushort`|`unsigned short`
`int`|`int`
`uint`|`unsigned int`
`long`|`long`
`ulong`|`unsigned long`
`llong`|`long long`
`ullong`|`unsigned long long`
`i8`|`int8_t`
`u8`|`uint8_t`
`i16`|`int16_t`
`u16`|`uint16_t`
`i32`|`int32_t`
`u32`|`uint32_t`
`i64`|`int64_t`
`u64`|`uint64_t`
`i128`|`__int128`
`u128`|`unsigned __int128`
`float`|`float`
`double`|`double`
`real`|`long double`
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
## Program Structure
lcc program involves one or many target form.
targets are translating it's content forms to C code.
each target must has a target c file, and a list of feature arguments.
### Features
* <b>:std</b>: writes standard libraries inclusion at top of target file.
```lisp
(target "main.c"
  (:std)
  ;; some forms
  )
```
```c
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
```
### Sections
* Documentations: starts with semi-colon(s) ";"
```lisp
;;; about a lisp file
;;;; author, licence and/or documentation about each target
(variable long height) ; description of a form
(function sqr ((double a)) 
  (returns double)
  ;; some commented code or documentation inside code
  (return (* a a)))
```
* Preprocessor Forms: a form which starts with at-sign "@" and accepts one argument. code form is used for writing C code inside lcc.
```lisp
(@define (code "SHA1_ROTL(bits, word) (((word) << (bits)) | ((word) >> (32-(bits)))"))

(struct SHA512Context
  (@ifdef USE_32BIT_ONLY)
  (member uint32_t Intermediate_Hash[(/ SHA512HashSize 4)]) ; Message Digest
  (member uint32_t Length[4])                               ; Message length in bits
  (@else)                                                   ; !USE_32BIT_ONLY
  (member uint64_t Intermediate_Hash[(/ SHA512HashSize 8)]) ; Message Digest
  (member uint64_t Length_High)
  (member uint64_t Length_Low)                              ; Message length in bits
  (@endif)                                                  ; USE_32BIT_ONLY
  (member int_least16_t Message_Block_Index)                ; Message_Block array index
  (member uint8_t Message_Block[SHA512_Message_Block_Size]) ; 1024-bit message blocks
  (member int Computed)                                     ; Is the hash computed?
  (member int Corrupted))                                   ; Cumulative corruption code
```
```c
#define SHA1_ROTL(bits, word) (((word) << (bits)) | ((word) >> (32-(bits)))

typedef struct SHA512Context {
#ifdef USE_32BIT_ONLY
  uint32_t Intermediate_Hash [SHA512HashSize / 4];
  uint32_t Length [4];
#else
  uint64_t Intermediate_Hash [SHA512HashSize / 8];
  uint64_t Length_High;
  uint64_t Length_Low;
#endif
  int_least16_t Message_Block_Index;
  uint8_t Message_Block [SHA512_Message_Block_Size];
  int Computed;
  int Corrupted;
} SHA512Context;
```
* Main Function: The main function is where program execution begins. Every lcc program must contain only one main function.
## Decision Making
### if
If form accepts 2 or 3 argument. condition, form for true evaluation of condition and form for false evaluation. third part(else) could be omitted. use progn form if you need more forms in each part.
```lisp
(let ((int a . 5)
      (int b . 6))
  (if (> a b)
     (printf "a is greater")
    (printf "maybe b is greater")))
```
```c
{
  int a = 5;
  int b = 6;
  if (a > b)
    printf("a is greater");
  else
    printf("maybe b is greater");
}
```
```lisp
(let ((int a . 5)
      (int b . 6))
  (if (> a b)
     (progn
       (printf "a is greater")
       (set a (* a b)))
    (progn
      (printf "maybe b is greater")
      (set b (* b a)))))
```
```c
{
  int a = 5;
  int b = 6;
  if (a > b) {
    printf("a is greater");
    a = a * b;
  } else {
    printf("maybe b is greater");
    b = b * a;
  }
}
```
### switch
```lisp
(let ((int a))
  (printf "Please enter a number between 1 and 5: ")
  (scanf "%d" (addressof a))
  
  (switch a
    (case 1 (printf "You chose One")   (break))
    (case 2 (printf "You chose Two")   (break))
    (case 3 (printf "You chose Three") (break))
    (case 4 (printf "You chose Four")  (break))
    (case 5 (printf "You chose Five")  (break))
    (default (printf "Invalid Choice."))))
```
```c
{
  int a;
  printf("Please enter a number between 1 and 5: ");
  scanf("%d", &a);
    
  switch (a) {
    case 1:
      printf("You chose One");
      break;
    case 2:
      printf("You chose Two");
      break;
    case 3:
      printf("You chose Three");
      break;
    case 4:
      printf("You chose Four");
      break;
    case 5:
      printf("You chose Five.");
      break;
    default:
      printf("Invalid Choice");
      break;
  }
}
```
## Loops
### while
```lisp
(let ((int n . 1)
      (int times . 5))
  (while (<= n times)
    (printf "lcc while loops: %d\n" n)
    (++# n)))
```
```c
{
  int n = 1, times = 5;

  while (n <= times) {
      printf("C while loops: %d\n", n);
      n++;
  }
}
```
### do
```lisp
(let ((int n . 1)
      (int times . 5))
  (do (<= n times)
    (printf "lcc do loops: %d\n" n)
    (++# n)))
```
```c
{
  int n = 1, times = 5;

  do {
      printf("C do loops: %d\n", n);
      n++;
  } while (n <= times)
}
```
### for
```lisp
(for ((int n . 1)
      (int times . 5))
  (<= n times)
  (++# n)
  (printf "lcc for loops: %d\n" n))
```
```c
for (int n = 1, int times = 5; (n <= times);) {
  n++;
  printf("lcc for loops: %dn", n);
}
```
## Function
lcc has some points on functions:
* Use returns form for setting the return type. returns form must be first form of a function after arguments list. A fucntion without returns form will returns void instead of main which returns int.
* Function's attributes must set in declaration time. each attribute enclosed in braces "{attribute}".
    * {declare}    
    * {static}
    * {inline}
    * {extern}
* Each declared function defined a function pointer typedef named FunctionName_t.
```lisp
(target "main.c"
  (:std)
  
  ;; function declaration
  {declare} (function addition ((int * a) (int * b)) (returns int))
  
  (function main ()
    ;; local variable definition
    (let ((int answer)
          (int num1 . 10)
          (int num2 . 5))
      
      ;; calling a function to get addition value
      (set answer (addition (addressof num1) (addressof num2)))
      (printf "The addition of two numbers is: %d\n" answer))
    (return 0))
  
  ;; function returning the addition of two numbers
  (function addition ((int * a) (int * b))
    (returns int)
    (return (+ (contentof a) (contentof b)))))
```
```c
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

/* function declaration */
int addition(int *num1, int *num2);

int main()
{
  {
    /* local variable definition */    
    int answer;
    int num1 = 10;
    int num2 = 5;
    
    /* calling a function to get addition value */    
    answer = addition(&num1, &num2);
    printf("The addition of two numbers is: %d\n", answer);
  }
  return 0;
}

/* function returning the addition of two numbers */
int addition(int *a,int *b)
{
    return *a + *b;
}
```
