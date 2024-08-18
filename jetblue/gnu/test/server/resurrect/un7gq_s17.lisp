;; 3. Interactive Lisp mode

;; The best way to experience Lisp and experiment with it, 
;; is using interactive mode in a terminal window or operating 
;; system command shell. Since version 10.3, newLISP's read-eval-print-loop 
;; (REPL) accepts multi-line statements.

;; To enter a multi-line statement hit the [enter] key on an empty line after 
;; the system prompt. To exit multi-line mode, hit the [enter] key again on an 
;; empty line. In the following example computer output is shown in bold 
;; letters:
(net-ipv)
(define ip-address "192.168.1.1")
(print ip-address "192.168.1..1")


;; 4. Common Lisp's built-in functions

;; Common Lisp provides many built-in functions for working with strings,
;; numbers, and lists. Here are some examples:

;; Concatenating strings:
(concatenate 'string "Hello, " "World!")

;; Converting numbers to strings:
(number-to-string 123)

;; Converting strings to numbers:
(abs 123)


;; 5. Common Lisp's built-in data structures

;; Common Lisp provides several built-in data structures for working with
;; data. Here are some examples:

;; Lists:
(list 1 2 3 4 5)

;; acos
;; syntax: (acos num-radians)

;; The arc-cosine function is calculated from the number in num-radians. 
;; For example, acos(0.5) returns 0.52359
(acos 0.5)

;; 6. Common Lisp's built-in mathematical functions

;; Common Lisp provides several built-in mathematical functions for working with
;; numbers. Here are some examples:

;; sin
;; syntax: (sin num-radians)

;; The sine function is calculated from the number in num-radians.
;; For example, sin(0.5) returns 0.47943
(sin 0.5)

;; log
;; syntax: (log num)

;; The natural logarithm function is calculated from the number.
;; For example, log(10) returns 2.30259
(log 10)

;; 7. Common Lisp's built-in list manipulation functions

;; Common Lisp provides several built-in list manipulation functions for working
;; with lists. Here are some examples:

;; append
;; syntax: (append list1 list2 ...)

;; The append function takes multiple lists as arguments and returns a new list
;; that contains all the elements from the input lists.
;; For example, (append '(1 2 3) '(4 5 6))
;; returns '(1 2 3 4 5 6)
(append '(1 2 3) '(4 5 6))

;; mapcar
;; syntax: (mapcar function list1 list2 ...)

;; The mapcar function applies a function to each element of multiple lists
;; and returns a new list with the results.
;; For example, (mapcar #'* '(1 2 3) '(4 5
;; returns '(4 10 18)
(mapcar #'* '(1 2 3) '(4 5))

;; 8. Common Lisp's built-in conditional functions

;; Common Lisp provides several built-in conditional functions for working
;; with conditions. Here are some examples:

;; if
;; syntax: (if condition then-part else-part)

;; The if function takes a condition, a then-part, and an else-part. If
;; the condition evaluates to true, the then-part is evaluated and its value
;; is returned. Otherwise, the else-part is evaluated and its value is returned.
;; For example, (if (> 3 2) 'greater 'less) returns 'greater
(if (> 3 2) 'greater 'less)

;; 9. Common Lisp's built-in looping functions
;; Common Lisp provides several built-in looping functions for working
;; with data. Here are some examples:

;; loop
;; syntax: (loop while condition do body...)

;; The loop function takes a condition, a body, and an optional
;; finally clause. The condition is evaluated repeatedly until it evaluates to
;; false. Inside the body, the variables defined in the loop are
;; accessible. The finally clause is executed after the loop ends, even if the
;; loop was exited by a break.
;; For example, (loop with (i 0) until (<= i 10)
;; do (print i)
;;   (incf i))
;; prints:
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8

;; 10. Common Lisp's built-in file handling functions

;; Common Lisp provides several built-in file handling functions for working
;; with files. Here are some examples:

;; open
;; syntax: (open file-spec mode &key :if-exists if-does-not
;; :if-exists :if-does-not-exist :element-type :external-
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)
                :element-type :char :external-format :binary :text)
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)
                :element-type :char :external-format :binary :text)
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)
                :element-type :char :external-format :binary :text)
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)
                :element-type :char :external-format :binary :text)
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)
                :element-type :char :external-format :binary :text)
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)
                :element-type :char :external-format :binary :text)
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)
                :element-type :char :external-format :binary :text)
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)
                :element-type :char :external-format :binary :text)
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)
                :element-type :char :external-format :binary :text)
                :sequential :random :read-only :write-only :append :create :delete
                :exclusive :truncate :shared)
                :direction :input :output :iobase :ioencoding)



;; 11. Common Lisp's built-in string manipulation functions

;; Common Lisp provides several built-in string manipulation functions for working
;; with strings. Here are some examples:

;; substring
;; syntax: (substring string start &optional end)

;; The substring function returns a substring of the input string.
;; For example, (substring "Hello, World!" 7 12) returns "World
(substring "Hello, World!" 7 12)


;; 12. Common Lisp's built-in number manipulation functions
;; Common Lisp provides several built-in number manipulation functions for working
;; with numbers. Here are some examples:

;; round
;; syntax: (round number &key :ties-to even)
;; The round function rounds the input number to the nearest integer.
;; For example, (round 3.7) returns 4
;; (round 3.7 :ties-to-even) returns 4
;; 13. Common Lisp's built-in error handling functions
;; Common Lisp provides several built-in error handling functions for working
;; with errors. Here are some examples:

;; error
;; syntax: (error &rest message)
;; The error function raises an error with the given message.
;; For example, (error "Error: Invalid input") raises an error with the message "Error
;; Invalid input"
(error "Error: Invalid input")


;; 14. Common Lisp's built-in symbol manipulation functions
;; Common Lisp provides several built-in symbol manipulation functions for working
;; with symbols. Here are some examples:

;; intern
;; syntax: (intern symbol-name &optional package-name)
;; The intern function interns a symbol with the given name in the given package.
;; For example, (intern "foo" 'common-lisp) interns the symbol "foo


;; 15. Common Lisp's built-in type checking functions
;; Common Lisp provides several built-in type checking functions for working
;; with types. Here are some examples:

;; typep
;; syntax: (typep object type-spec)
;; The typep function checks if the object is of the specified type.
;; For example, (typep 10 'integer) returns true


;; 16. Common Lisp's built-in function definition and calling functions
;; Common Lisp provides several built-in function definition and calling functions
;; for working with functions. Here are some examples:

;; defun
;; syntax: (defun function-name (arg1 arg2...) body...)
;; The defun function defines a new function with the given name and arguments.
;; For example, (defun add (x y) (+ x y)) defines the function add that
;; returns the sum of its arguments
(defun add (x y) (+ x y))


;; 17. Common Lisp's built-in macro definition and calling macros
;; Common Lisp provides several built-in macro definition and calling macros
;; for working with macros. Here are some examples:

;; defmacro
;; syntax: (defmacro macro-name (arg1 arg2...) body...)
;; The defmacro function defines a new macro with the given name and arguments.
;; For example, (defmacro square (x) `(*, x x)) defines the macro square
;; that returns the square of its argument
(defmacro square (x) `(*, x x))


;; 18. Common Lisp's built-in function composition and application
;; Common Lisp provides several built-in function composition and application
;; functions for working with functions. Here are some examples:

;; funcall
;; syntax: (funcall function arg1 arg2 ...)
;; The funcall function calls the given function with the given arguments.
;; For example, (funcall add 3 4) returns 7


;; 19. Common Lisp's built-in higher-order function composition
;; Common Lisp provides several built-in higher-order function composition
;; functions for working with functions. Here are some examples:

;; compose
;; syntax: (compose function1 function2 ...)
;; The compose function composes the given functions from right to left.
;; For example, (compose add 10 square) returns a function that adds 10
;; and squares its result
(compose add 10 square)

;; 20. Common Lisp's built-in higher-order function application
;; Common Lisp provides several built-in higher-order function application
;; functions for working with functions. Here are some examples:

;; apply
;; syntax: (apply function argument-list)
;; The apply function calls the given function with the given arguments.
;; For example, (apply add '(3 4)) returns 7
(apply add '(3 4))


;; 21. Common Lisp's built-in function currying
;; Common Lisp provides several built-in function currying functions for working
;; with functions. Here are some examples:

;; curry
;; syntax: (curry function arg1)
;; The curry function curries the given function with the given argument.
;; For example, (curry add 3) returns a function that takes a second argument
;; and returns the sum of the given arguments
(curry add 3)


;; 22. Common Lisp's built-in function partial application
;; Common Lisp provides several built-in function partial application
;; functions for working with functions. Here are some examples:

;; apply-partially


;; 23. Common Lisp's built-in function memoization
;; Common Lisp provides several built-in function memoization functions for working
;; with functions. Here are some examples:

;; memoize


;; 24. Common Lisp's built-in function tail call optimization
;; Common Lisp provides several built-in function tail call optimization
;; functions for working with functions. Here are some examples:

;; tailcall-optimization

;; 25. Common Lisp's built-in function garbage collection
;; Common Lisp provides several built-in function garbage collection


;; 26. Common Lisp's built-in function load and eval
;; Common Lisp provides several built-in function load and eval functions for working
;; with files and strings. Here are some examples:

;; load
;; syntax: (load filename)
;; The load function loads the contents of the given file into the current
;; environment.
;; For example, (load "example.lisp") loads the contents of the file
;; "example.lisp" into the current environment
(load "example.lisp")

;; eval
;; syntax: (eval expression)
;; The eval function evaluates the given expression in the current environment.


;; 27. Common Lisp's built-in function thread-safety
;; Common Lisp provides several built-in function thread-safety functions for working
;; with threads. Here are some examples:

;; thread-local-variable

;; 28. Common Lisp's built-in function debugging
;; Common Lisp provides several built-in function debugging functions for working
;; with debugging. Here are some examples:

;; debug
;; syntax: (debug function)
;; The debug function enables debugging for the given function.
;; For example, (debug add) enables debugging for the function add
(debug add)


;; 29. Common Lisp's built-in function internationalization
;; Common Lisp provides several built-in function internationalization
;; functions for working with internationalization. Here are some examples:

;; format
;; syntax: (format string &rest arguments)
;; The format function formats the given string with the given arguments.


;; 30. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; gettext
;; syntax: (gettext message-domain &optional message-string &rest arguments)
;; The gettext function retrieves the localized string for the given message
;; domain and message string.
;; For example, (gettext "messages" "Hello, World!") retrieves the localized
;; string "Hello, World!" for the "messages" message domain
(gettext "messages" "Hello, World!")

;; 31. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; ngettext
;; syntax: (ngettext message-singular message-plural count &optional message-string &rest


;; 32. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; bind
;; syntax: (bind symbol value &body body)
;; The bind function binds the given symbol to the given value in the scope
;; of the body.
;; For example, (bind (symbol x) 10 (print x)) binds the
;; 33. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; unbind
;; syntax: (unbind symbol)
;; The unbind function unbinds the given symbol from its current value in the
;; scope.
;; For example, (bind (symbol x) 10) (print x) ; prints
;; 34. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; multiple-value-bind
;; syntax: (multiple-value-bind (symbol1 &rest symbol-n) expression &
;; The multiple-value-bind function binds the given symbols to the values
;; returned by the expression.
;; For example, (multiple-value-bind (symbol1 symbol2) (values 1
;; 35. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; multiple-value-call
;; syntax: (multiple-value-call function &rest arguments)
;; The multiple-value-call function calls the given function with the given
;; arguments and returns the values returned by the function.
;; For example, (multiple-value-call add 3 4) returns (7
;; 36. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; multiple-value-prog1


;; 37. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; multiple-value-prog2


;; 38. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; multiple-value-prog3

;; 39. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; multiple-value-prog4

;; 40. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization
;; functions for working with internationalization and localization. Here are some examples:

;; multiple-value-prog5

;; 41. Common Lisp's built-in function internationalization and localization


;; 42. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization


;; 43. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization

;; 44. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization


;; 45. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization


;; 46. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization

;; 47. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization

;; 48. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization

;; 49. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization

;; 50. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization


;; acosh
;; syntax: (acosh num-radians)

;; Calculates the inverse hyperbolic cosine of num-radians, the value whose hyperbolic cosine is num-radians. If num-radians is less than 1, acosh returns NaN.


;; acosh
;; syntax: (acosh num-radians)
;; Calculates the inverse hyperbolic cosine of num-radians, the value whose hyperbolic
;; 51. Common Lisp's built-in function internationalization and localization
;; functions for working with internationalization and localization
(acosh 2)

;; 52. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization

;; 53. Common Lisp's built-in function internationalization and localization
;; Common Lisp provides several built-in function internationalization and localization

;; 54. Common Lisp's built-in function internationalization and localization
;; 55. Common Lisp's built-in function internationalization and localization

;; 56. Common Lisp's built-in function internationalization and localization
;; 57. Common Lisp's built-in function internationalization and localization
;; 58. Common Lisp's built-in function internationalization and localization
;; 59. Common Lisp's built-in function internationalization and localization
;; 60. Common Lisp's built-in function internationalization and localization
;; 61. Common Lisp's built-in function internationalization and localization
;; 62. Common Lisp's built-in function internationalization and localization
;; 63. Common Lisp's built-in function internationalization and localization
;; 64. Common Lisp's built-in function internationalization and localization
;; 65. Common Lisp's built-in function internationalization and localization
;; 66. Common Lisp's built-in function internationalization and localization
;; 67. Common Lisp's built-in function internationalization and localization
;; 68. Common Lisp's built-in function internationalization and localization
;; 69. Common Lisp's built-in function internationalization and localization
;; 70. Common Lisp's built-in function internationalization and localization
;; 71. Common Lisp's built-in function internationalization and localization
;; 72. Common Lisp's built-in function internationalization and localization
;; 73. Common Lisp's built-in function internationalization and localization
;; 74. Common Lisp's built-in function internationalization and localization
;; 75. Common Lisp's built-in function internationalization and localization
;; 76. Common Lisp's built-in function internationalization and localization
;; 77. Common Lisp's built-in function internationalization and localization
;; 78. Common Lisp's built-in function internationalization and localization
;; 79. Common Lisp's built-in function internationalization and localization
;; 80. Common Lisp's built-in function internationalization and localization


;; add
;; syntax: (add num-1 [num-2 ... ])

;; All of the numbers in num-1, num-2, and on are summed. add accepts float or integer operands, but it always returns a floating point number. Any floating point calculation with NaN also returns NaN.
(add 1 2 3 4 5 6 7 8 9 0)



;; address
;; syntax: (address int)
;; syntax: (address float)
;; syntax: (address str)

;; Returns the memory address of the integer in int, the double floating point number in float, or the string in str. This function is used for passing parameters to library functions that have been imported using the import function. 
(set 's "\001\002\003\004")

;; Returns the memory address of the integer in int, the double floating point number in double
;; 81. Common Lisp's built-in function internationalization and localization
(get-char (+ (address s) 3))

(set 'x 12345) ; x is a 64-bit long int


;; address
;; syntax: (address int)
;; syntax: (address float)
;; syntax: (address str)

;; address string (address string) (address string)


;; address
;; syntax: (address int)
;; syntax: (address float)
;; syntax: (address str)
; on a big-endian CPU, i.e. PPC or SPARC 
(get-long (address x))     ;;    → 12345
; the 32-bit int is in high 32-bit part of the long int
(get-int (+ (address x) 4))  ;;  → 12345

; on a little-endian CPU, i.e. Intel i386
; the 32-bit int is in the low 32-bit part of the long int
(get-int (address x))        ;;  → 12345

; on both architectures (integers are 64 bit in newLISP)
(set 'x 1234567890)
(get-long (address x))        ;; →  1234567890

;; amb
;; syntax: (amb exp-1 [exp-2 ... ])

;; One of the expressions exp-1 ... n is selected at random, and the evaluation 
;; result is returned.

(amb 'a 'b 'c 'd 'e)  ;; → one of: a, b, c, d, or e at random

(dotimes (x 10) (print (amb 3 5 7))) ;; → 35777535755

;; Internally, newLISP uses the same function as rand to pick a random number. 
;; To generate random floating point numbers, use random, randomize, or normal. 
;; To initialize the pseudo random number generating process at a specific starting 
;; point, use the seed function. 

;; and
;; syntax: (and exp-1 [exp-2 ... ])

;; The expressions exp-1, exp-2, etc. are evaluated in order, returning the result 
;; of the last expression. If any of the expressions yield nil or the empty list (), 
;; evaluation is terminated and nil or the empty list () is returned.

(set 'x 10)                       ;; → 10
(and (< x 100) (> x 2))           ;; → true
(and (< x 100) (> x 2) "passed")  ;; → "passed"
(and '())                         ;; → ()
(and true)                        ;; → true
(and)                             ;; → true


;; append
;; syntax: (append list1 list2 ...)


;; Returns a new list that is the concatenation of list1 and list2.
;; The original lists list1 and list2 are not modified.


;; append
;; syntax: (append list1 list2 ...)
;; append
;; syntax: (append list-1 [list-2 ... ])
;; syntax: (append array-1 [array-2 ... ])
;; syntax: (append str-1 [str-2 ... ])

;; In the first form, append works with lists, appending list-1 through 
;; list-n to form a new list. The original lists are left unchanged.

(append '(1 2 3) '(4 5 6) '(a b)) ;; → (1 2 3 4 5 6 a b)

(set 'aList '("hello" "world"))   ;; → ("hello" "world")

(append aList '("here" "I am"))   ;; → ("hello" "world" "here" "I am")


;; In the second form append works on arrays:

(set 'A (array 3 2 (sequence 1 6)))
;; → ((1 2) (3 4) (5 6))
(set 'B (array 2 2 (sequence 7 10)))
;; → ((7 8) (9 10))

(append A B)
;; → ((1 2) (3 4) (5 6) (7 8) (9 10))

(append B B B)
;; → ((7 8) (9 10) (7 8) (9 10) (7 8) (9 10))


;; In the third form, append works on strings. The strings in str-n are concatenated 
;; into a new string and returned.

(set 'more " how are you")       ;; → " how are you"

(append "Hello " "world," more)  ;; → "Hello world, how are you"

;; append is also suitable for processing binary strings containing zeroes. 
;; The string function would cut off strings at zero bytes.

;; Linkage characters or strings can be specified using the join function. Use the string 
;; function to convert arguments to strings and append in one step.

;; Use the functions extend and push to append to an existing list or string modifying the 
;; target.


;; apply
;; syntax: (apply function args)
;; syntax: (apply function [arg ...])
;; syntax: (apply function [arg ... ] [key-value ... ])

;; Calls the function function with the given arguments, applying any key-value
;; pairs as keyword arguments. If no key-value pairs are given, the function
;; is called with the arguments as positional arguments.

(defun add (a b) (+ a b))


;; The following examples show how to use apply with different argument lists.
(apply add '(1 2 3 4 5))  ;; → 15
(apply add 1 2 3 4 5)  ;; → 15
(apply #'add 1 2 3 4 5)  ;; → 1



;; The following examples show how to use apply with keyword arguments.
(apply #'+ :x 1 :y 2 :z 3)  ;; →
(apply #'+ :x 1 :y 2 :z 3 :w 4) ;; → (apply #'+ :x 1 :y 2 :z 3 :w 4) ;;




;; args
;; syntax: (args)
;; syntax: (args int-idx-1 [int-idx-2 ... ])

;; Accesses a list of all unbound arguments passed to the currently evaluating define, 
;; define-macro lambda, or lambda-macro expression. Only the arguments of the current 
;; function or macro that remain after local variable binding has occurred are available. 
;; The args function is useful for defining functions or macros with a variable number 
;; of parameters.

;; args can be used to define hygienic macros that avoid the danger of variable capture. 
;; See define-macro.

(define-macro (print-line)
    (dolist (x (args))
        (print x "\n")))
                        
(print-line "hello" "World")

;; This example prints a line-feed after each argument. The macro mimics the effect of the 
;; built-in function println.

;; In the second syntax, args can take one or more indices (int-idx-n). 
(define-macro (foo)
    (print (args 2) (args 1) (args 0)))

(foo x y z) 
zyx 

(define (bar)
	(args 0 2 -1))

(bar '(1 2 (3 4)))  ;; → 4


;; apply
;; syntax: (apply function args)
;; syntax: (apply function [arg ...])
;; syntax: (apply function [arg ... ] [key-value ... ])

;; Calls the function function with the given arguments, applying any key-value
;; pairs as keyword arguments. If no key-value pairs are given, the function
;; is called with the arguments as positional arguments.

(defun add (a b) (+ a b))

;; The following examples show how to use apply with different argument lists.
(apply add '(1 2 3 4 5))  ;; → 15

;; The function foo prints out the arguments in reverse order. The bar function 
;; shows args being used with multiple indices to access nested lists.

;; Remember that (args) only contains the arguments not already bound to local 
;; variables of the current function or macro:

(define (foo a b) (args))
  
(foo 1 2)      ;;  → ()
                 
(foo 1 2 3 4 5) ;; → (3 4 5)

;; In the first example, an empty list is returned because the arguments are bound 
;; to the two local symbols, a and b. The second example demonstrates that, after 
;; the first two arguments are bound (as in the first example), three arguments 
;; remain and are then returned by args.

;; (args) can be used as an argument to a built-in or user-defined function call, 
;; but it should not be used as an argument to another macro, in which case (args) 
;; would not be evaluated and would therefore have the wrong contents in the new macro 
;; environment. 

;; array
;; syntax: (array int-n1 [int-n2 ... ] [list-init])

;; Creates an array with int-n1 elements, optionally initializing it with the contents 
;; of list-init. Up to sixteen dimensions may be specified for multidimensional arrays.

;; Internally, newLISP builds multidimensional arrays by using arrays as the elements 
;; of an array. newLISP arrays should be used whenever random indexing into a large list 
;; becomes too slow. Not all list functions may be used on arrays. For a more detailed 
;; discussion, see the chapter on arrays.

(array 5)    ;;              → (nil nil nil nil nil)

(array 5 (sequence 1  5))  ;; → (1 2 3 4 5)

(array 10 '(1 2))    ;;;      → (1 2 1 2 1 2 1 2 1 2)

;; Arrays can be initialized with objects of any type. If fewer initializers than elements 
;; are provided, the list is repeated until all elements of the array are initialized.

(set 'myarray (array 3 4 (sequence 1 12)))
;; → ((1 2 3 4) (5 6 7 8) (9 10 11 12))


;; Arrays are modified and accessed using most of the same functions 
;; used for modifying lists:

(setf (myarray 2 3) 99) → 99)
myarray ;; ((1 2 3 4) (5 6 7 8) (9 10 11 99))

(setf (myarray 1 1) "hello")  → "hello"

myarray ;; ((1 2 3 4) (5 "hello" 7 8) (9 10 11 99))

(setf (myarray 1) '(a b c d)) → (a b c d)
myarray ;; ((1 2 3 4) (a b c d) (9 10 11 99))

(nth 1 myarray)     → (a b c d)  ; access a whole row
                    
;; use implicit indexing and slicing on arrays
                    
(myarray 1)    ;;  → (a b c d)
                    
(myarray 0 -1) ;;  → 4

(2 myarray)    ;;  → ((9 10 11 99)) 

(-3 2 myarray) ;;  → ((1 2 3 4) (a b c d)) 

;; Care must be taken to use an array when replacing a whole row.

;; array-list can be used to convert arrays back into lists:

(array-list myarray) ;; → ((1 2 3 4) (a b c d) (1 2 3 99))


;; To convert a list back into an array, apply flat to the list:

(set 'aList '((1 2) (3 4)))     ;;        → ((1 2) (3 4))

(set 'aArray (array 2 2 (flat aList))) ;;  → ((1 2) (3 4))


;; The array? function can be used to check if an expression is an array:

(array? myarray)  ;; → true
                               
(array? (array-list myarray)) ;; → nil

;; The array-dimensions function can be used to get the dimensions of an array:

(array-dimensions myarray)  ;; → (2 2)


;; When serializing arrays using the function source or save, the generated 
;; code includes the array statement necessary to create them. This way, variables 
;; containing arrays are correctly serialized when saving with save or creating source 
;; strings using source.

(set 'myarray (array 3 4 (sequence 1 12)))

(save "array.lsp" 'myarray)

;; contents of file arraylsp ;;

(set 'myarray (array 3 4 (flat '(
  (1 2 3 4) 
  (5 6 7 8) 
  (9 10 11 12)))))


;; array-list
;; syntax: (array-list array)

;; Returns a list conversion from array, leaving the original array unchanged:

(set 'myarray (array 3 4 (sequence 1 12)))
;; → ((1 2 3 4) (5 6 7 8) (9 10 11 12))

(set 'mylist (array-list myarray))
;; → ((1 2 3 4) (5 6 7 8) (9 10 11 12))

(list (array? myarray) (list? mylist))
;; → (true true)


;; The array-dimensions function can be used to get the dimensions of an array:

(array-dimensions myarray)  ;; → (3 4)


;; array?
;; syntax: (array? exp)

;; Checks if exp is an array:

(set 'M (array 3 4 (sequence 1 4)))   
;; → ((1 2 3 4) (1 2 3 4) (1 2 3 4)))


(array? M) ;;              → true

(array? (array-list M)) ;; → nil

;; asin
;; syntax: (asin num-radians)

;; Calculates the arcsine function from the number in num-radians 
;; and returns the result.

(asin 1) ;; → 1.570796327
(sin (asin 1)) ;; → 1


;; asinh
;; syntax: (asinh num)
;; Calculates the hyperbolic arcsine function from the number num
;; and returns the result.


;; atan
;; syntax: (atan num)


;; Calculates the arctangent function from the number num and returns the result.

;;asinh
;; syntax: (asinh num-radians)

;; Calculates the inverse hyperbolic sine of num-radians, the value whose hyperbolic 
;; sine is num-radians.

(asinh 2)       ;;  → 1.443635475
(sinh (asinh 2)) ;;  → 2


;; atan2
;; syntax: (atan2 num-y num-x)
;; Calculates the arctangent of the quotient of num-y divided by num-x
;; and returns the result.

;; atan2
;; syntax: (atan2 num-y num-x)
;; Calculates the arctangent of the quotient of num-y divided by num-x
;; and returns the result.

;; assoc
;; syntax: (assoc exp-key list-alist)
;; syntax: (assoc list-exp-key list-alist)

;; In the first syntax the value of exp-key is used to search list-alist for 
;; a member-list whose first element matches the key value. If found, the 
;; member-list is returned; otherwise, the result will be nil.

(assoc 1 '((3 4) (1 2))) ;; → (1 2)

(set 'data '((apples 123) (bananas 123 45) (pears 7)))

(assoc 'bananas data) ;; → (bananas 123 45)
(assoc 'oranges data)  ;; → nil

;; Together with setf assoc can be used to change an association.

(setf (assoc 'pears data) '(pears 8))

data  ;; ((apples 123) (bananas 123 45) (pears 8))

;; In the second syntax more then one key expressions can be specified to search 
;; in nested, multilevel association lists:

(set 'persons '(
    (id001 (name "Anne") (address (country "USA") (city "New York")))
    (id002 (name "Jean") (address (country "France") (city "Paris")))
))

(assoc '(id001 address) persons) ;; (address (country "USA") (city "New York"))
(assoc '(id001 address city) persons) ;; (city "New York")


;; The list in list-aList can be a context which will be interpreted as its default 
;; functor. This way very big lists can be passed by reference for speedier access 
;; and less memory usage:

(set 'persons:persons '(
    (id001 (name "Anne") (address (country "USA") (city "New York")))
    (id002 (name "Jean") (address (country "France") (city "Paris")))
))

(define (get-city db id)
    (last (assoc (list id 'address 'city) db ))
)

(get-city persons 'id001) ;; "New York"


;; atan
;; syntax: (atan num-radians)

;; The arctangent of num-radians is calculated and returned.

(atan 1)       ;;  → 0.7853981634
(tan (atan 1)) ;;  → 1


;; atan2
;; syntax: (atan2 num-Y-radians num-X-radians)

;; The atan2 function computes the principal value of the arctangent 
;; of Y / X in radians. It uses the signs of both arguments to determine 
;; the quadrant of the return value. atan2 is useful for converting Cartesian 
;; coordinates into polar coordinates.

(atan2 1 1)                      ;;  → 0.7853981634
(div (acos 0) (atan2 1 1))       ;;  → 2
(atan2 0 -1)                     ;;  → 3.141592654
(= (atan2 1 2) (atan (div 1 2))) ;;  → true


