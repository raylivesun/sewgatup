;; atanh
;; syntax: (atanh num-radians)

;; Calculates the inverse hyperbolic tangent of num-radians, the value whose 
;; hyperbolic tangent is num-radians. If the absolute value of num-radians 
;; is greater than 1, atanh returns NaN; if it is equal to 1, atanh returns infinity.

(atanh 0.5) ;; 0.5493061443
(tanh (atanh 0.5)) ;; 0.5
(atanh 1.1) ;; NaN
(atanh 1) ;; inf


;; acos
;; syntax: (acos num)

;; atom?
;; syntax: (atom? exp)

;; Returns true if the value of exp is an atom, otherwise nil. An expression is an atom 
;; if it evaluates to nil, true, an integer, a float, a string, a symbol or a primitive. 
;; Lists, lambda or lambda-macro expressions, and quoted expressions are not atoms. 

(atom? '(1 2 3))     ;; → nil
(and (atom? 123)
     (atom? "hello")
     (atom? 'foo))   ;; → true
(atom? ''foo)        ;; → nil


;; base64-dec
;; syntax: (base64-dec str)

;; The BASE64 string in str is decoded. Note that str is not verified to be a 
;; valid BASE64 string. The decoded string is returned.

(base64-dec "SGVsbG8gV29ybGQ=") ;;  → "Hello World"

;; For encoding, use the base64-enc function.

;; newLISP's BASE64 handling is derived from routines found in the Unix curl utility 
;; and conforms to the RFC 4648 standard. 


;; bitwise-and
;; syntax: (bitwise-and num1 num2)

;; base64-enc
;; syntax: (base64-enc str [bool-flag])

;; The string in str is encoded into BASE64 format. This format encodes groups 
;; of 3 * 8 = 24 input bits into 4 * 8 = 32 output bits, where each 8-bit output 
;; group represents 6 bits from the input string. The 6 bits are encoded into 64 
;; possibilities from the letters A–Z and a–z; the numbers 0–9; and the characters 
;; + (plus sign) and / (slash). The = (equals sign) is used as a filler in 
;; unused 3- to 4-byte translations. This function is helpful for converting 
;; binary content into printable characters.

;; Without the optional bool-flag parameter the empty string "" is encoded into 
;; "====". If bool-flag evaluates to true, the empty string "" is translated into 
;; "". Both translations result in "" when using base64-dec.

;; The encoded string is returned.

;; BASE64 encoding is used with many Internet protocols to encode binary data for 
;; inclusion in text-based messages (e.g., XML-RPC).

(base64-enc "Hello World") ;;  → "SGVsbG8gV29ybGQ="

(base64-enc "")      ;;       → "===="
(base64-enc "" true) ;;       → ""


;; get-char  ⚠
;; syntax: (get-char int-address)

;; Gets an 8-bit character from an address specified in int-address. 
;; This function is useful when using imported shared library functions with import. 
;; char * foo(void)
;; {
;; char * result;
;; result = "ABCDEFG";
;; return(result);
;; }
;; ...
(let ((result (get-char (foo))))
  (format t "Result: ~A~%" result)) ;; Result: ABCDEFG~%


;; get-current-time
;; syntax: (get-current-time)

;; Returns a string representing the current date and time in the format
;; "YYYY-MM-DD HH:MM:SS".


;; get-environment-variable
;; syntax: (get-environment-variable var-name)

;; Returns the value of the environment variable specified by var-name.

;; Consider the above C function from a shared library, which returns a character 
;; pointer (address to a string).

(import "mylib.so" "foo")
(print (get-char (foo) ))      ;;     →  65 ; ASCII "A"
(print (get-char (+ (foo) 1))) ;;  →  66 ; ASCII "B"

;; get-file-size
;; syntax: (get-file-size file-name)

;; Returns the size of the file specified by file-name in bytes.

;; get-line
;; syntax: (get-line str-address)

;; Gets a line from an address specified in str-address. This function is useful
;; when using imported shared library functions with import. char * foo(void)
;; {
;; char * result;
;; result = "Hello\nWorld";
;; return(result);
;; }
;; ...


;; get-random-integer
;; get-float  ⚠
;; syntax: (get-float int-address)

;; Gets a 64-bit double float from an address specified in int-address. 
;; This function is helpful when using imported shared library functions 
;; (with import) that return an address pointer to a double float or a pointer 
;; to a structure containing double floats.

;; double float * foo(void)
;; {
;; double float * result;
;; …
;; *result = 123.456;
;; return(result);
;; }

;; The previous C function is compiled into a shared library.

(import "mylib.so" "foo")
(get-float (foo))  ;; 123.456

;; get-int  ⚠
;; syntax: (get-int int-address)

;; Gets a 32-bit integer from the address specified in int-address. This function is handy when using imported shared library functions with import, a function returning an address pointer to an integer, or a pointer to a structure containing integers.

;; int * foo(void)
;; {
;; int * result;
;; …
;; *result = 123;
;; return(result);
;; }

;; int foo-b(void)
;; {
;; int result;
;; …
;; result = 456;
;; return(result);
;; }


;; get-time-zone
;; syntax: (get-time-zone)

;; Returns the current time zone offset in seconds, as an integer.

;; get-unix-time
;; syntax: (get-unix-time)
;; Consider the C function foo (from a shared library), which returns 
;; an integer pointer (address of an integer).

(import "mylib.so" "foo")
(get-int (foo)) ;; → 123
(foo-b)         ;; → 456


;; is-atom
;; syntax: (is-atom exp)


;; is-boolean
;; syntax: (is-boolean exp)

;; is-character
;; syntax: (is-character exp)

;; is-cons
;; syntax: (is-cons exp)
;; get-long  ⚠
;; syntax: (get-long int-address)

;; Gets a 64-bit integer from the address specified in int-address. 
;; This function is handy when using import to import shared library 
;; functions, a function returning an address pointer to a long integer, 
;; or a pointer to a structure containing long integers.

;; long long int * foo(void)
;; {
;; int * result;
;; …
;; *result = 123;
;; return(result);
;; }

;; long long int foo-b(void)
;; {
;; int result;
;; …
;; result = 456;
;; return(result);
;; }

;; Consider the C function foo (from a shared library), which returns 
;; an integer pointer (address of an integer).

(import "mylib.so" "foo")
(get-int (foo))  ;; → 123
(foo-b)          ;; → 456


;; get-string  ⚠
;; syntax: (get-string int-address [int-bytes [str-limit])

;; Copies a character string from the address specified in int-address. 
;; This function is helpful when using imported shared library functions 
;; with import and a C-function returns the address to a memory buffer.

;; char * foo(void)
;; {
;; char * result;
;; result = "ABCDEFG";
;; return(result);
;; }

;; Consider the above C function from a shared library, which returns a character 
;; pointer (address to a string).

(import "mylib.so" "foo")
(print (get-string (foo)))  ;; → "ABCDEFG"

;; When a string is passed as an argument, get-string will take its address as 
;; the argument. Without the optional int-bytes argument get-string breaks off 
;; at the first first \000 (null character) it encounters. This works for 
;; retrieving ASCII strings from raw memory addresses:

(set 'buff "ABC\000\000\000DEF")  ;; → "ABC\000\000\000DEF"

(length buff) ;; → 9

(get-string buff)  ;; → "ABC"

(length (get-string buff))  ;; → 3

; get a string from offset into a buffer
(get-string (+ (address buff) 6)) ;; → "DEF"


;; get-symbol
;; syntax: (get-symbol str)

;; Returns a symbol object representing the symbol specified by str.

;; get-vector
;; syntax: (get-vector int-address [int-length])

;; Copies a vector from the address specified in int-address.
;; This function is useful when using imported shared library functions
;; with import and a C-function returns the address to a memory buffer.

;; When specifyung the number of bytes in the optional int-bytes parameter, 
;; reading does not stpop at the first zero byte found, but copies exactly 
;; int-bytes number of bytes from the address or string buffer:

(set 'buff "ABC\000\000\000DEF")  ;; → "ABC\000\000\000DEF"

; without specifying the number of bytes
; buff is equivalent to (address buff)
(get-string buff)  ;; → "ABC"

; specifying the number of bytes to get
(get-string buff 9) ;; → "ABC\000\000\000DEF"

;; is-equal
;; syntax: (is-equal exp1 exp2)

;; is-fixnum
;; syntax: (is-fixnum exp)

;; is-float
;; syntax: (is-float exp)

;; is-function
;; syntax: (is-function exp)

;; is-hash-table
;; syntax: (is-hash-table exp)
;; is-integer
;; syntax: (is-integer exp)

;; is-keyword
;; syntax: (is-keyword exp)
;; is-null
;; syntax: (is-null exp)

;; is-number
;; syntax: (is-number exp)
;; is-pair
;; syntax: (is-pair exp)
;; is-procedure
;; syntax: (is-procedure exp)
;; is-string
;; syntax: (is-string exp)

;; is-symbol
;; syntax: (is-symbol exp)
;; is-vector
;; syntax: (is-vector exp)

;; length
;; syntax: (length exp)

;; list
;; syntax: (list...)

;; map
;; syntax: (map function list)

;; member
;; syntax: (member item list)

;; not
;; syntax: (not exp)

;; null
;; syntax: (null exp)

;; pair
;; syntax: (pair car cdr)

;; procedure
;; syntax: (procedure name)

;; quotient
;; syntax: (quotient exp1 exp2)

(set 'buff "ABC\000\000EFG\000DQW") → "ABC\000\000EFG\000DQW"

; buff is eqivalent to (address buff)
(get-string buff 4 "FG") → "ABC\000"

(get-string buff 10) → "ABC\000\000EFG\000D"

(get-string buff 10 "FG") → "ABC\000\000E"


;; remainder
;; Although UTF-16 and UTF-32 encoding does not specify string termination characters, 
;; the sequences "\000\000" and "\000\000\000\000" are used often to terminate UTF-16 
;; and UTF-32 encodings. The additional optional str-limit can be used to limit the 
;; string when reading from the buffer address:

(set 'utf32 (unicode "我能吞下玻璃而不伤身体。"))

(set 'addr (address utf32)) → 140592856255712

; get-string automatically takes the address when a buffer is passed
; utf32 is equivalent to (address utf32) for get-string

(get-string utf32 80 "\000\000\000\000")
→ "\017b\000\000??\000\000\030T\000\000\011N\ 000\000?s\
000\000?t\000\000\f?\000\000\rN\000\000$O\000\000??\000\000SO\000\000\0020\000\000"


;; get-url
;; syntax: (get-url str-url [str-option] [int-timeout [str-header]])

;; Reads a web page or file specified by the URL in str-url using the 
;; HTTP GET protocol. Both http:// and file:// URLs are handled. "header" 
;; can be specified in the optional argument str-option to retrieve only 
;; the header. The option "list" causes header and page information to be 
;; returned as separate strings in a list and also includes the server 
;; status code as the third list member (since 10.6.4). The "raw" option 
;; (since 10.6.4), which can be used alone or combined with other options, 
;; suppresses header location redirection.

;; A "debug" option can be specified either alone or after the "header" 
;; or "list" option separated by one character, i.e. "header debug" or 
;; "list debug". Including "debug" outputs all outgoing information to 
;; the console window.

;; The optional argument int-timeout can specify a value in milliseconds. 
;; If no data is available from the host after the specified timeout, 
;; get-url returns the string ERR: timeout. When other error conditions 
;; occur, get-url returns a string starting with ERR: and the description 
;; of the error.

;; get-url handles redirection if it detects a Location: spec in the received 
;; header and automatically does a second request. get-url also understands 
;; the Transfer-Encoding: chunked format and will unpack data into an unchunked 
;; format.

;; get-url requests are also understood by newLISP server nodes.

(get-url "http://www.nuevatec.com")
(get-url "http://www.nuevatec.com" 3000)
(get-url "http://www.nuevatec.com" "header")
(get-url "http://www.nuevatec.com" "header" 5000)
(get-url "http://www.nuevatec.com" "list")

(get-url "file:///home/db/data.txt") ; access local file system

(env "HTTP_PROXY" "http://ourproxy:8080")
(get-url "http://www.nuevatec.com/newlisp/")

;; The index page from the site specified in str-url is returned as a string. 
;; In the third line, only the HTTP header is returned in a string. Lines 2 and 4 show 
;; a timeout value being used.

;; The second example shows usage of a file:// URL to access /home/db/data.txt on the 
;; local file system.

;; The third example illustrates the use of a proxy server. The proxy server's URL must 
;; be in the operating system's environment. As shown in the example, this can be added 
;; using the env function.

;; The int-timeout can be followed by an optional custom header in str-header: 

;; Custom header

;; The custom header may contain options for browser cookies or other directives 
;; to the server. When no str-header is specified, newLISP sends certain header 
;; information by default. After the following request: 

(get-url "http://somehost.com" 5000)

;; newLISP sends the following header:

;; GET / HTTP/1.1
;; Host: somehost.com
;; Connection: close
;; User-Agent: NewLISP/1.1.0
;; Accept: */*
;;
;; The following example demonstrates how to include a custom header: 

;; Custom header

;; The custom header may contain options for browser cookies or other directives
;; to the server. When no str-header is specified, newLISP sends certain header
;; information by default. After the following request:

;; newLISP will configure and send the request and header below:

;; GET / HTTP/1.1        
;; Host: somehost.com
;; User-Agent: newLISP v10603
;; Connection: close

;; As an alternative, the str-header option could be used: 
(get-url "http://somehost.com" 5000 
"User-Agent: Mozilla/4.0\r\nCookie: name=fred\r\n")


;; The following example demonstrates how to include a custom header with the "list" option:

;; Custom header

;; The custom header may contain options for browser cookies or other directives
;; to the server. When no str-header is specified, newLISP sends certain header
;; GET / HTTP/1.1        
;; Host: somehost.com
;; User-Agent: Mozilla/4.o
;; Cookie: name=fred
;; Connection: close

;; Note that when using a custom header, newLISP will only supply the GET request line, 
;; as well as the Host: and Connection: header entries. newLISP inserts all other entries 
;; supplied in the custom header between the Host: and Connection: entries. Each entry must 
;; end with a carriage return line-feed pair: \r\n.

;; See an HTTP transactions reference for valid header entries.

;; Custom headers can also be used in the put-url and post-url functions. 

;; global
;; syntax: (global sym-1 [sym-2 ... ])

;; One or more symbols in sym-1 [sym-2 ... ] can be made globally accessible from contexts 
;; other than MAIN. The statement has to be executed in the MAIN context, and only symbols 
;; belonging to MAIN can be made global. global returns the last symbol made global. 


;; The second example shows how constant and global can be combined into one statement, 
;; protecting and making a previous function definition global.


;; global?
;; syntax: (global? sym)

;; Checks if symbol in sym is global. Built-in functions, context symbols, and all 
;; symbols made global using the function global are global:

global? 'print)   ;; → true
(global 'var)     ;; → var
(global? 'var)    ;; → true

(constant (global 'foo))

(global? 'foo)   ;; → true

;; history
;; syntax: (history [bool-params])

;; history returns a list of the call history of the enclosing function. 
;; Without the optional bool-params, a list of function symbols is returned. 
;; The first symbol is the name of the enclosing function. When the optional 
;; bool-params evaluates to true, the call arguments are included with the symbol.

(define (foo x y) 
    (bar (+ x 1) (* y 2)))

(define (bar a b) 
    (history))

; history returns names of calling functions
(foo 1 2) → (bar foo)

; the addtional 'true' forces inclusion of callpatterns
(define (bar a b) 
    (history true))

(foo 1 2) → ((bar (+ x 1) (* y 2)) (foo 1 2))

;; inspect
;; syntax: (inspect obj)

;; inspect returns a string representation of obj. The representation may
;; include the type and the contents of obj. inspect is useful for debugging
;; and inspecting objects.

;; The second example shows how inspect can be used to inspect a complex object.


;; inspect?
;; syntax: (inspect? obj)

;; inspect? returns true if obj is a string representation of another object,
;; false otherwise.

;; The second example shows how inspect? can be used to check if a string
;; is a string representation of another object.

;; load
;; syntax: (load str-filename)

;; load loads the contents of the file specified by str-filename into the current
;; context. load returns the last symbol loaded. The file must contain valid
;; NewLISP code.

;; The second example shows how load can be used to include a NewLISP file into


;; load?
;; syntax: (load? str-filename)

;; load? returns true if the file specified by str-filename contains valid
;; NewLISP code, false otherwise.

;; The second example shows how load? can be used to check if a file contains
;; valid NewLISP code.

;; log
;; syntax: (log str-filename)

;; log opens a file specified by str-filename for writing and returns the file
;; object. The file is created if it does not exist, and the file is truncated
;; if it already exists.

;; The second example shows how log can be used to create a new log file.

;; log-append
;; syntax: (log-append file-obj str)

;; log-append appends str to the file specified by file-obj.


;; log-close
;; syntax: (log-close file-obj)

;; log-close closes the file specified by file-obj.


;; log-flush
;; syntax: (log-flush file-obj)

;; log-flush forces the contents of the file specified by file-obj to be written
;; to the disk.

;; log-read
;; syntax: (log-read file-obj)

;; log-read reads the entire contents of the file specified by file-obj and

;; if
;; syntax: (if exp-condition exp-1 [exp-2])
;; syntax: (if exp-cond-1 exp-1 exp-cond-2 exp-2 [ ... ])

;; If the value of exp-condition is neither nil nor an empty list, the result 
;; of evaluating exp-1 is returned; otherwise, the value of exp-2 is returned. 
;; If exp-2 is absent, the value of exp-condition is returned.

;; if also sets the anaphoric system variable $it to the value of the conditional 
;; expression in if.


;; The second example shows how if can be used to perform conditional
;; execution.

;; if?
;; syntax: (if? exp-condition)
(set 'x 50)                   → 50
(if (< x 100) "small" "big")  → "small"
(set 'x 1000)                 → 1000
(if (< x 100) "small" "big")  → "big"
(if (> x 2000) "big")         → nil

; more than one statement in the true or false
; part must be blocked with (begin ...)
(if (= x y)
  (begin
    (some-func x)
    (some-func y))
  (begin
    (do-this x y)
    (do-that x y))
)

; if also sets the anaphoric system variable $it
(set 'lst '(A B C))
(if lst (println (last $it)))  → C


;; The second form of if works similarly to cond, except it does not 
;; take parentheses around the condition-body pair of expressions. 
;; In this form, if can have an unlimited number of arguments.

(define (classify x)
(if
(< x 0) "negative"
(< x 10) "small"
(< x 20) "medium"
(>= x 30) "big"
"n/a"))

(classify 15)   → "medium"
(classify 100)  → "big"
(classify 22)   → "n/a"
(classify -10)  → "negative"


;; ifft
;; syntax: (ifft list-num)

;; Calculates the inverse discrete Fourier transform on a list of complex numbers 
;; in list-num using the FFT method (Fast Fourier Transform). Each complex number 
;; is specified by its real part, followed by its imaginary part. In case only real 
;; numbers are used, the imaginary part is set to 0.0 (zero). When the number 
;; of elements in list-num is not an integer power of 2, ifft increases the number 
;; of elements by padding the list with zeroes. When complex numbers are 0 in the 
;; imaginary part, simple numbers can be used. 

;; The second example shows how ifft can be used to calculate the inverse discrete
;; Fourier transform of a list of complex numbers.

;; The third example shows how ifft can be used to calculate the inverse discrete
;; Fourier transform of a list of simple numbers.

;; The fourth example shows how ifft can be used to calculate the inverse discrete
;; Fourier transform of a list of complex numbers with zeros in the imaginary
;; part.

;; The fifth example shows how ifft can be used to calculate the inverse discrete
;; Fourier transform of a list of simple numbers with zeros in the imaginary
;; part.

(ifft (ifft '((1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0))))

;; when imaginary part is 0, plain numbers work too
(ifft (ifft '(1 2 3 4 5 6 7 8 9)))

;; import  ⚠
;; syntax: (import str-lib-name str-function-name ["cdecl"])
;; syntax: (import str-lib-name str-function-name str-return-type [str-param-type . . .])
;; syntax: (import str-lib-name)

;; Imports the function specified in str-function-name from a shared library named 
;; in str-lib-name. Depending on the syntax used, string labels for return and parameter 
;; types can be specified

;; If the library in str-lib-name is not in the system's library path, the full path name 
;; should be specified.

;; A function can be imported only once. A repeated import of the same function will simply 
;; return the same - already allocated - function address.

;; Note, that the first simple syntax is available on all versions of newLISP, even those 
;; compiled without libffi support. On libffi enabled versions - capable of the second 
;; extended syntax - imported symbols are protected against change and can only be modified 
;; using constant.

;; The third syntax - on OSX, Linux and other Unix only - allows pre-loading libraries 
;; without importing functions. This is necessary when other library imports need access 
;; internally to other functions from pre-loaded libraries.

;; Incorrectly using import can cause a system bus error or a segfault can occur and crash 
;; newLISP or leave it in an unstable state.
;; The simple import syntax

;; Most library functions can be imported using the simpler first syntax. This form is 
;; present on all compile flavors of newLISP. The API expects all function arguments to 
;; be passed on the stack in either cdecl or stdcall conventions. On 32-bit platforms, 
;; integers, pointers to strings and buffers sometimes floating point values can be passed 
;; as parameters. On 64-bit platforms only integers can be passed but no floating point 
;; values. As return values only 32-bit or 64-bit values and pointers are allowed. No 
;; floating point numbers can be returned. Strings must be retrieved with the get-string 
;; helper function. Regardless of these limitations, most modules included in the 
;; distribution use this simple import API.

;; If pointers are returned to strings or structures the following helper functions can 
;; be used extract data: get-char, get-int, get-float, get-string, unpack

;; To pass pointers for data structures the following functions help to pack data and 
;; calculate addresses: address, pack.

;; To transform newLISP data types into the data types needed by the imported function, 
;; use the functions float for 64-bit double floats, flt for 32-bit floats, and int for 
;; 32-bit integers. By default, newLISP passes floating point numbers as 64-bit double 
;; floats, integers as 32-bit integers, and strings as 32-bit integers for string 
;; addresses (pointers in C). Floats can only be used with 32-bit versions of newLISP 
;; and libraries. To use floating point numbers in a 64-bit environment use the extended 
;; import syntax. 

;; define LIBC platform independent

(define LIBC (lookup ostype '()))
("Windows" "msvcrt.dll")
("OSX" "libc.dylib")

(import LIBC "printf")
(printf "%g %s %d %c\n" 1.23 "hello" 999 65)
;; 1.23 hello 999 A
;; → 17 ; return value

;; import MS Windows DLLs in 32-bit versions 

(import "kernel32.dll" "GetTickCount") ;;  → GetTickCount
(import "user32.dll" "MessageBoxA")    ;;  → MessageBoxA
(GetTickCount)                         ;;  → 3328896


;; import MS Windows DLLs in 64-bit versions

;; In the first example, the string "1.23 hello 999 A" is printed as a side effect, 
;; and the value 17 (number of characters printed) is returned. Any C function can 
;; be imported from any shared library in this way.

;; The message box example pops up a Windows dialog box, which may be hidden behind 
;; the console window. The console prompt does not return until the 'OK' button is 
;; pressed in the message box.


;; import MS Windows DLLs in 32-bit versions
;;this pops up a message box

(MessageBoxA 0 "This is the body" "Caption" 1) 

;; The other examples show several imports of MS Windows DLL functions and the details 
;; of passing values by value or by reference. Whenever strings or numbers are passed 
;; by reference, space must be reserved beforehand.

(import "kernel32.dll" "GetWindowsDirectoryA")

;; allocating space for a string return value
(set 'str (dup "\000" 64))  ; reserve space and initialize

(GetWindowsDirectoryA str (length str))

str  ;; → "C:\\WINDOWS\000\000\000 ... "

;; use trim or get-string to cut of trailing binary zeros
(get-string str) ;;  → "C:\\WINDOWS"
(trim str)       ;;  → "C:\\WINDOWS"

(import "kernel32.dll" "GetComputerNameA")

;; allocate memory and initialize to zeros
(set 'str (dup "\000" 64))
(set 'len (length str))

;; call the function
;; the length of the string is passed as address reference 
;; string str is automatically past by address (C pointer)
(GetComputerNameA str (address len)) 

str  ;; → "LUTZ-PC\000\000 ... "

(trim str)  ;; → "LUTZ-PC"


;; import returns the address of the function, which can be used to assign a different 
;; name to the imported function.


;; define LIBC platform independent

(set 'imprime (import "libc.so.6" "printf")) 
;; → printf@400862A0

(imprime "%s %d" "hola" 123)                 
;; → "hola 123"

;; The MS Windows and Cygwin versions of newLISP uses standard call stdcall conventions 
;; to call DLL library routines by default. This is necessary for calling DLLs that belong 
;; to the MS Windows operating system. Most third-party DLLs are compiled for C declaration 
;; cdecl calling conventions and may need to specify the string "cdecl" as an additional 
;; last argument when importing functions. newLISP compiled for macOS, Linux and other 
;; Unix systems uses the cdecl calling conventions by default and ignores any additional 
;; string.

;; force cdecl calling conventions on MS Windows
(import "sqlite.dll" "sqlite_open" "cdecl")  ;; → sqlite_open <673D4888>


;; Imported functions may take up to fourteen arguments. Note that floating point 
;; arguments take up two spaces each (e.g., passing five floats takes up ten of the 
;; fourteen parameters).
;; The extended import syntax

;; The extended import API works with the second syntax. It is based on the popular 
;; libffi library which is pre-installed on most OS platforms. The startup banner 
;; of newLISP should show the word libffi indicating the running version of newLISP 
;; is compiled to use the extended import API. The function sys-info can also be used 
;; to check for libffi-support.

;; The API works with all atomic C data types for passed parameters and return values. 
;; The extended API requires that parameter types are specified in the import statement 
;; as string type labels. Programs written with extended import API will run without 
;; change on 32-bit and 64-bit newLISP and libraries. Integers, floating point values 
;; and strings can be returned without using helper functions.

;; The following types can be specified for the return value in str-return-type and 
;; for function parameters in str-param-type:
;; label	C type for return value and arguments	newLISP return and argument type
;; The types "char*" and "void* can be interchanged and are treated identical inside 
;; libffi. Depending on the type of arguments passed and the type of return values, 
;; one or the other is used.

;; Aggregate types can be composed using the struct function and can be used for 
;; arguments and return values.

;; The following examples show how the extended import syntax can handle return values 
;; of floating point values and strings:


;; return a float value, LIBC was defined earlier
;             name   return   arg
(import LIBC "atof" "double" "char*")
(atof "3.141") ;; → 3.141

;; return a copied string
;             name     return  arg-1   arg-2
(import LIBC "strcpy" "char*" "char*" "char*")
(set 'from "Hello World")

(set 'to (dup "\000" (length from))) ; reserve memory
(strcpy to from) ;; → "Hello World"


;; The char* type takes a string buffer only. The "void* type can take either a string 
;; buffer or a memory address number as input. When using "void*" as a return type the 
;; address number of the result buffer will be returned. This is useful when returning 
;; pointers to data structures. These pointers can then be used with unpack and struct 
;; for destructuring. In the following example the return type is changed to void*:

(import LIBC "strcpy" "void*" "char*" "char*")
(set 'from "Hello World")
(set 'to (dup "\000" (length from)))

(strcpy to from)       ;; → 2449424
(address to)           ;; → 2449424
(unpack "s11" 2449424) ;; → "Hello World"
(get-string 2449424)   ;; → "Hello World"
to                     ;; → "Hello World"


;; A newLISP string is always passed by it's address reference.

;; For a more complex example see this OpenGL demo.
;; Memory management

;; Any allocation performed by imported foreign functions has to be de-allocated 
;; manually if there's no call in the imported API to do so. See the Code Patterns 
;; in newLISP document for an example.

;; In case of calling foreign functions with passing by reference, memory for variables 
;; needs to be allocated beforehand by newLISP — see import of GetWindowsDirectoryA above 
;; — and hence, memory needs not be deallocated manually, because it is managed automatically 
;; by newLISP.

;; inc !
;; syntax: (inc place [num])

;; Increments the number in place by 1.0 or by the optional number num and returns 
;; the result. inc performs float arithmetic and converts integer numbers passed 
;; into floating point type.

;; place is either a symbol or a place in a list structure holding a number, or a 
;; number returned by an expression.

(set 'x 0)    ;; → 0
(inc x)       ;; → 1
x             ;; → 1
(inc x 0.25)  ;; → 1.25
x             ;; → 1.25
(inc x)       ;; → 2.25

;; If a symbol for place contains nil, it is treated as if containing 0.0:

z             ;; → nil
(inc z)       ;; → 1

(set 'z nil)
(inc z 0.01)  ;; → 0.01


;; Places in a list structure or a number returned by another expression 
;; can be updated too:

(set 'l '(1 2 3 4))

(inc (l 3) 0.1) ;; → 4.1

(inc (first l)) ;; → 2

l ;; → (2 2 3 4.1)

(inc (+ 3 4)) ;; → 8


;; dec!
;; syntax: (dec place [num])

;; index
;; syntax: (index exp-predicate exp-list)

;; Applies the predicate exp-predicate to each element of the list exp-list 
;; and returns a list containing the indices of the elements for which exp-predicate 
;; is true.

(index symbol? '(1 2 d 4 f g 5 h))  ;; → (2 4 5 7)

(define (big? x) (> x 5)) ;; → (lambda (x) (> x 5))

(index big? '(1 10 3 6 4 5 11))  ;; → (1 3 6)

(select '(1 10 3 6 4 5 11) '(1 3 6)) ;;→ (10 6 11) 


;; inf?
;; syntax: (inf? float)

;; If the value in float is infinite the function returns true else nil.

(inf? (div 1 0)) ;; → true

(div 0 0) ;; → NaN


;; NaN?
;; syntax: (nan? float)

;; Note that an integer division by zero e.g. (/ 1 0) will throw an "division by zero" 
;; error and not yield infinity. See also NaN? to check if a floating point number 
;; is valid.


;; int
;; syntax: (int exp [exp-default [int-base]])

;; If the expression in exp evaluates to a number or a string, the result is converted 
;; to an integer and returned. If exp cannot be converted to an integer, then nil or the 
;; evaluation of exp-default will be returned. This function is mostly used when 
;; translating strings from user input or from parsing text. If exp evaluates to 
;; a string, the string must start with a digit; one or more spaces; 
;; or the + or - sign. The string must begin with '0x' for hexadecimal 
;; strings or '0' (zero) for octal strings. If exp is invalid, int returns 
;; nil as a default value if not otherwise specified.

;; A second optional parameter can be used to force the number base of conversion 
;; to a specific value.

;; Integers larger than 9,223,372,036,854,775,807 are truncated 
;; to 9,223,372,036,854,775,807. Integers smaller than -9,223,372,036,854,775,808 
;; are truncated to -9,223,372,036,854,775,808.

;; When converting from a float (as in the second form of int), floating point values 
;; larger or smaller than the integer maximum or minimum are also truncated. A floating 
;; point expression evaluating to NaN is converted to 0 (zero).

(int "123")          ;; → 123
(int " 123")         ;; → 123
(int "a123" 0)       ;; → 0
(int (trim " 123"))  ;; → 123
(int "0xFF")         ;; → 255
(int "0b11111")      ;; → 31
(int "055")          ;; → 45
(int "1.567")        ;; → 1
(int 1.567)          ;; → 1

(integer? 1.00)        ;; → nil
(integer? (int 1.00))  ;; → true

(int "1111" 0 2)  ;; → 15   ; base 2 conversion
(int "0FF" 0 16)  ;; → 255  ; base 16 conversion

(int 'xyz)     ;; → nil
(int 'xyz 0)   ;; → 0
(int nil 123)  ;; → 123

(int "abc" (throw-error "not a number"))  
;;→ ERR: user error : not a number

(print "Enter a num:")
(set 'num (int (read-line)))

(int (bits 12345) 0 2) ;; → 12345
