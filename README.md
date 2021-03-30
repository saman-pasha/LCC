# lcc
Lisp C Compiler, Lisp-like syntax for writing C code in addition of some forms and pointer managements.
## Instruction
* Install [SBCL](www.sbcl.org).
* lcc as default uses [Libtool](https://www.gnu.org/software/libtool) for compiling and linking `C` code. If you like it just install it for your platform and put it in the `PATH` environment variable. Compiler and linker could be set in `lcc-config.lisp` file.
* Download and copy lcc folder to `~/common-lisp` for enabling [ASDF](https://common-lisp.net/project/asdf) access to lcc package.
* Write your own lcc code and save it in `.lcc` or `.lisp` extension.
* Copy `lcc.lisp` file from source folder into your project path.
* Send your file as an argument to lcc.lisp. `sbcl --script lcc.lisp test.lisp`
* If you are using EMACS editor, copy mode.lisp file content into .emacs file for syntax highlighting.
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
`?`|`?:`
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
`bool`|`bool`
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
```lisp
(let ((double price . 500.4)                         ; atom initialization
      (double price_array [] . '{100.2 230.7 924.8}) ; list initialization
      (double price_calc . #'(calculate_price))))    ; initialization by output of a function
```
```c
{
  double price = 500.4;
  double price_array [] = {100.2, 230.7, 924.8};
  double price_calc = calculate_price();
}
```
### Free Variable Declaration and Initialization
A free variable can has some attributes or storage class. each attribute enclosed in braces or parentheses.
* {auto}
* {register}
* {static}
* {extern}
```lisp
{auto} (variable int width)
{register} (variable int height . 5)
(variable char letter . #\A)
(variable float age)
{extern} (variable float area)
{static} (variable double d)

;; actual initialization
(set width 10)
(set age 26.5)
```
```c
auto int width; 
register height = 5;
char   letter = 'A';
float  age;
extern float area;
static double d;

/* actual initialization */
width = 10;
age = 26.5;
```
### Scoped Variable Declaration and Initialization
A scoped variable can has some attributes or storage class. each attribute enclosed in braces or parentheses.
* {auto}
* {register}
* {static}
```lisp
(let ({static} (int width . 3)
      {register} (int height . 4))
  (printf "area: %d" (* width height)))
```
```c
{
  static int width = 3;
  register int height = 4;
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
All features could be omitted or if available accept `#t` for default behaviour or `#f` for do nothing.
* <b>:std</b>: writes standard libraries inclusion at top of target file.
```lisp
(target "main.c"
  (:std #t)
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
* <b>:compile</b>: used for compiling target file. Dafault behaviour is `-c target.c`. Could be a list of arguments that will send to compiler which has been set in `lcc-config.lisp`.
* <b>:link</b>: used for linking and builing target file as library or executable. It has not dfault behaviour. Could be a list of arguments that will send to linker which has been set in `lcc-config.lisp`.
```lisp
;; MyMath library declaration
(target "mymath.h"
  (:compile #f)

  (guard __MYMATH_H__
    {declare} (function obj1_does ((int) (int)) (returns int))
    {declare} (function obj2_does ((int) (int)) (returns int))
    {declare} (function obj3_does ((int) (int)) (returns int))))

;; Default compilation
(target "obj1.c"
  (:compile #t)
  (include "mymath.h")
  (function obj1_does ((int x) (int y)) (returns int)
	    (return (+ x y))))

;; Custom compilation
(target "obj2.c"
  (:compile ("-c" "obj2.c" "-o" "objmul.o"))
  (include "mymath.h")
  (function obj2_does ((int x) (int y)) (returns int)
	    (return (* x y))))

;; Library creation and linking
(target "obj3.c"
  (:compile #t :link ("-o" "libMyMath.la" "obj1.lo" "objmul.lo" "obj3.lo"))
  (include "mymath.h")
  (function obj3_does ((int x) (int y)) (returns int)
	    (return (obj1_does (obj2_does x y) (obj2_does x y)))))

;; Executable creation and linking
(target "main.c"
  (:std #t :compile #t :link ("-o" "CompileTest" "main.lo" "-lMyMath"))
  (include "mymath.h")
  (function main ((int argc)
		  (char * argv []))
	    (if (!= argc 3)
		(block
		 (printf "two digits needed!")
		 (return EXIT_FAILURE)))
	    (let ((int x . #'(atoi (nth 1 argv)))
		  (int y . #'(atoi (nth 2 argv))))
	      (printf "MyMath lib outputs: %d\n" (obj3_does x y)))
	    (return EXIT_SUCCESS)))
```
```
lcc: compiling target mymath.h
lcc: compiling target obj1.c
libtool: compile:  gcc -g -O -c obj1.c  -fPIC -DPIC -o .libs/obj1.o
libtool: compile:  gcc -g -O -c obj1.c -o obj1.o >/dev/null 2>&1
lcc: compiling target obj2.c
libtool: compile:  gcc -g -O -c obj2.c  -fPIC -DPIC -o .libs/objmul.o
libtool: compile:  gcc -g -O -c obj2.c -o objmul.o >/dev/null 2>&1
lcc: compiling target obj3.c
libtool: compile:  gcc -g -O -c obj3.c  -fPIC -DPIC -o .libs/obj3.o
libtool: compile:  gcc -g -O -c obj3.c -o obj3.o >/dev/null 2>&1
libtool: link: rm -fr  .libs/libMyMath.a .libs/libMyMath.la
libtool: link: ar cr .libs/libMyMath.a .libs/obj1.o .libs/objmul.o .libs/obj3.o 
libtool: link: ranlib .libs/libMyMath.a
libtool: link: ( cd ".libs" && rm -f "libMyMath.la" && ln -s "../libMyMath.la" "libMyMath.la" )
lcc: compiling target main.c
libtool: compile:  gcc -g -O -c main.c  -fPIC -DPIC -o .libs/main.o
libtool: compile:  gcc -g -O -c main.c -o main.o >/dev/null 2>&1
libtool: link: gcc -g -O -o CompileTest .libs/main.o  /home/saman/Projects/LCC/trunk/.libs/libMyMath.a
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
If form accepts 2 or 3 argument. condition, form for true evaluation of condition and form for false evaluation. third part(else) could be omitted. use ```block``` form if you need more forms in each part.
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
     (block
       (printf "a is greater")
       (set a (* a b)))
    (block
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
  printf("lcc for loops: %d\n", n);
}
```
### for-each
Static array:
```lisp
(let ((int ages [] . '{20 22 24 26}))
  (for-each (int i) ages (/ (sizeof ages) (sizeof int))
    (printf "each age: %d\n" i)))
```
```c
{
  int ages[] = {20, 22, 24, 26};
  for (int G4321 = 0; G4321 < sizeof(ages) / sizeof(int); G4321++) {
    int i = ages[G4321];
    printf("each age: %d\n", i);
  }
}
```
Dynamic array:
```lisp
(function main ((int argc) (char ** argv))
  (for-each (char * arg) argv argc
    (printf "%s\n" arg)))
```
```c
int main (int argc, char ** argv) 
{
  for (int G4321 = 0; G4321 < argc; G4321++) {
    char * arg = argv[G4321];
    printf("%s\n", arg);
  }
}
```
## Function
lcc has some points on functions:
* Use returns form for setting the return type. returns form must be first form of a function after arguments list. A fucntion without returns form will returns void instead of main which returns int.
* Function's attributes must set in declaration time. each attribute enclosed in braces or parentheses.
    * {declare}    
    * {static}
    * {inline}
    * {extern}
* Each declared function defined a function pointer typedef named FunctionName_t.
```lisp
(target "main.c"
  (:std #t)
  
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
```lisp
{declare} (function function_pointer ((int) (int)))
```
```c
void function_pointer (int, int);
typedef void (*function_pointer_t) (int, int);
```
## Array
### Define
```lisp
(variable double amount [5])
```
```c
double amount[5];
```
### Initialize 
```lisp
(variable int ages [5] . '{22 23 24 25 26})
```
```c
int ages[5] = {22 23 24 25 26};
```
```lisp
(variable int myArray [5])

;; Initializing elements of array seperately
(for ((int n . 0))
  (< n (/ (sizeof myArray) (sizeof int)))
  (++# n)
  (set (nth n myArray) n))
```
```c
int myArray[5];

// Initializing elements of array seperately
for(int n = 0; n < sizeof(myArray) / sizeof(int); n++)
{
  myArray[n] = n;
}
```
## String
```lisp
(variable char name [6] . '{#\C #\l #\o #\u #\d #\Null})
(variable char name []  . "Cloud")
(variable char * name   . "Cloud")
```
```c
char name[6] = {'C', 'l', 'o', 'u', 'd', '\0'};
char name[]  = "Cloud";
char * name  = "Cloud";
```
### Special Characters
`#\Null`
`#\Space`
`#\Newline`
`#\Tab`
`#\Page`
`#\Rubout`
`#\Linefeed`
`#\Return`
`#\Backspace`
## Pointer
```lisp
(variable int * width)
(variable int * letter)
```
```c
int  *width;
char *letter;
```
```lisp
(target "main.c"
  ()
  (include <stdio.h>)
  
  (function main ((int argc) (char * argv []))
    (let ((int n . 20)
          (int * pntr))        ; actual and pointer variable declaration
      (set pntr (addressof n)) ; store address of n in pointer variable
      (printf "Address of n variable: %x\n" (addressof n))
      
      ;; address stored in pointer variable
      (printf "Address stored in pntr variable: %x\n" pntr)

      ;; access the value using the pointer
      (printf "Value of *pntr variable: %d\n" (contentof pntr)))
    (return 0))
```
```c
#include<stdio.h>

int main (int argc, char *argv[])
{
  {
    int n = 20, *pntr;  /* actual and pointer variable declaration */
    pntr = &n;          /* store address of n in pointer variable  */
    printf("Address of n variable: %x\n", &n);

    /* address stored in pointer variable */   
    printf("Address stored in pntr variable: %x\n", pntr);

    /* access the value using the pointer */   
    printf("Value of *pntr variable: %d\n", *pntr);
  }
  return 0;
}
```
## Dynamic Memory Allocation
C dynamic memory allocation functions `malloc()`, `calloc()`, `realloc()`, `free()` are available. Other keyword `alloc` that works in `let` initialization part which automatically is checking pointer and freeing allocated memory at the end of let scope.
```lisp
(let ((char * mem_alloc . #'(malloc (* 15 (sizeof char))))) ; memory allocated dynamically
  (if (== mem_alloc nil) (printf "Couldn't able to allocate requested memory\n"))
  (free mem_alloc))
```
```c
{    
  char * mem_alloc = malloc(15 * sizeof(char)); /* memory allocated dynamically */
  if (mem_alloc == NULL) {
    printf("Couldn't able to allocate requested memory\n");
  }
  free(mem_alloc);
}
```
Allocation with `alloc` and equivalent code in C:
```lisp
(let ((char * safe_alloc . #'(alloc (* 15 (sizeof char)))))
  (printf "Memory allocated safely\n"))
```
```c
{
    char * safe_alloc = ((char *)malloc((15 * sizeof(char))));
    if (safe_alloc == NULL) printf("dynamic memory allocation failed! safe_alloc\n");
    printf("Memory allocated safely\n");
    free(safe_alloc);
}
```
```lisp
(let ((int n_rows . 4)
      (int n_columns . 5)
      (int ** matrix . #'(alloc (* (* n_rows n_columns) (sizeof int)))))
  (printf "Matrix allocated\n"))
```
```c
{
    int n_rows = 4;
    int n_columns = 5;
    int ** matrix = ((int **)malloc(((n_rows * n_columns) * sizeof(int))));
    if (matrix == NULL) printf("dynamic memory allocation failed! matrix\n");
    printf("Matrix allocated\n");
    free(matrix);
}
```
Allocation by `alloc` and equivalent `calloc`:
```lisp
(let ((char * safe_alloc . #'(alloc 15 (sizeof char))))
  (printf "Memory allocated safely\n"))
```
```c
{    
  char * safe_alloc = calloc(15, sizeof(char)); 
  if (safe_alloc == NULL) printf("dynamic memory allocation failed! safe_alloc\n");
  printf("Memory allocated safely\n");
  free(safe_alloc);
}
```
## Structure
`declares` form is for declaring one or more variable(s) at the end of nested struct declaration just for anonymous structures.
Use `$` form for struct's member access and `->` form for member access of pointer of struct.
```lisp
(struct Course
  (member char WebSite [50])
  (member char Subject [50])
  (member int  Price))
  
(variable Course c1 . '{"domain.com" "Compilers" 100})
(variable Course * pc1 . #'(addressof c1))
  
(function print_course ()
  (printf "Course: %s in %s for %d$" 
    ($ c1 Subject) 
    ($ c1 WebSite)
    ($ c1 Price))
  (printf "Course: %s in %s for %d$" 
    (-> pc1 Subject) 
    (-> pc1 WebSite)
    (-> pc1 Price)))
```
```c
typedef struct Course {
  char WebSite [50];
  char Subject [50];
  int Price;
} Course;

Course c1 = {"domain.com", "Compilers", 100};
Course * pc1 = &c1;

void print_course () {
  printf("Course: %s in %s for %d$", c1.Subject, c1.WebSite, c1.Price);
  printf("Course: %s in %s for %d$", pc1->Subject, pc1->WebSite, pc1->Price);
}
```
## Union
`declares` form is for declaring one or more variable(s) at the end of nested union declaration just for anonymous unions.
Use `$` form for union's member access and `->` form for member access of pointer of union.
```lisp
(struct USHAContext
  (member int whichSha)                 ; which SHA is being used
  (union
    (member SHA1Context   sha1Context)
    (member SHA224Context sha224Context) 
    (member SHA256Context sha256Context)
    (member SHA384Context sha384Context) 
    (member SHA512Context sha512Context)
    (declares ctx)))
```
```c
typedef struct USHAContext {
  int whichSha;
  union {
    SHA1Context sha1Context;
    SHA224Context sha224Context;
    SHA256Context sha256Context;
    SHA384Context sha384Context;
    SHA512Context sha512Context;
  } ctx;
} USHAContext;
```
## Enum
```lisp
(enum
  (shaSuccess . 0)
  (shaNull)            ; Null pointer parameter
  (shaInputTooLong)    ; input data too long
  (shaStateError)      ; called Input after FinalBits or Result
  (shaBadParam))       ; passed a bad parameter
```
```c
enum {
  shaSuccess = 0,
  shaNull,
  shaInputTooLong,
  shaStateError,
  shaBadParam
};
```
## Guard
```lisp
(guard __STUDENT_H__
  (struct Student
    (member char name [50])
    (member char family [50])
    (member int  class_no)))
```
```c
#ifndef __STUDENT_H__
#define __STUDENT_H__
typedef struct Student {
  char name [50];
  char family [50];
  int class_no;
} Student;
#endif /* __STUDENT_H__ */ 
```
## Typedef
```lisp
(typedef int * intptr_t)
```
```c
typedef int * intptr_t;
```
## C++ Compiler
C++ compiler could be used instead of C compiler then some features availables:
* `&` modifier in function argument for pass by reference.
* Default value for members of structs.
* `method` form for defining a member function inside of structs.

# Good Luck!
