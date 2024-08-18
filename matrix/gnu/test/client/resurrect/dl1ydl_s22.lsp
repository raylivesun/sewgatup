;; make-dir
;; syntax: (make-dir str-dir-name [int-mode])

;; Creates a directory as specified in str-dir-name, with the optional access mode 
;; int-mode. Returns true or nil depending on the outcome. If no access mode is 
;; specified, most Unix systems default to drwxr-xr-x.

;; On Unix systems, the access mode specified will also be masked by the OS's user-mask 
;; set by the system administrator. The user-mask can be retrieved on Unix systems using 
;; the command umask and is usually 0022 (octal), which masks write (and creation) 
;; permission for non-owners of the file.

;; 0 (zero) in front of 750 makes it an octal number

(make-dir "adir" 0750)  


;; map
;; syntax: (map exp-functor list-args-1 [list-args-2 ... ])

;; Successively applies the primitive function, defined function, or lambda 
;; expression exp-functor to the arguments specified in list-args-1 list-args-2—, 
;; returning all results in a list. Since version 10.5.5 list-args can also be array 
;; vectors, but the returned result will always be a list.

(map + '(1 2 3) '(50 60 70))  ;; → (51 62 73)

(map if '(true nil true nil true) '(1 2 3 4 5) '(6 7 8 9 10))
;; → '(1 7 3 9 5)

(map (fn (x y) (* x y)) '(3 4) '(20 10))
;; → (60 40)

;; The second example shows how to dynamically create a function for map:

(define (foo op p) 
    (append (lambda (x)) (list (list op p 'x))))


;; filter
;; syntax: (filter pred list)

;; Returns a new list containing only those elements from list for which the
;; predicate pred returns true. The predicate pred can be a function, a lambda
;; expression, or a symbol referring to a predefined predicate function.


;; The following example demonstrates how to use filter with a lambda expression:
;; We can also use the shorter fn:

(define (foo op p) 
    (append (fn (x)) (list (list op p 'x))))


;; foo now works like a function-maker:

(foo 'add 2)  ;; → (lambda (x) (add 2 x))

(map (foo add 2) '(1 2 3 4 5))  ;; → (3 4 5 6 7)

(map (foo mul 3) '(1 2 3 4 5))  ;; → (3 6 9 12 15)



;; The following example demonstrates how to use filter with a symbol referring to a
;; predefined predicate function:

;; Note that the quote before the operand can be omitted because primitives evaluate 
;; to themselves in newLISP.

;; By incorporating map into the function definition, we can do the following:

(define (list-map op p lst) 
    (map (lambda (x) (op p x)) lst))

(list-map + 2 '(1 2 3 4))  ;; → (3 4 5 6)

(list-map mul 1.5 '(1 2 3 4)) ;;  → (1.5 3 4.5 6)

;; map also sets the internal list index $idx.

(map (fn (x) (list $idx x)) '(a b c)) ;;→ ((0 a) (1 b) (2 c))

;; The number of arguments used is determined by the length of the first argument list. 
;; Arguments missing in other argument lists cause map to stop collecting parameters for 
;; that level of arguments. This ensures that the nth parameter list gets converted to 
;; the nth column during the transposition occurring. If an argument list contains too 
;; many elements, the extra ones will be ignored.

;; Special forms which use parentheses as syntax cannot be mapped (i.e. case).


;; mat
;; syntax: (mat + | - | * | / matrix-A matrix-B)
;; syntax: (mat + | - | * | / matrix-A number)

;; Using the first syntax, this function performs fast floating point scalar 
;; operations on two-dimensional matrices in matrix-A or matrix-B. The type of 
;; operation is specified by one of the four arithmetic operators +, -, *, or /. 
;; This type of arithmetic operator is typically used for integer operations in 
;; newLISP. In the case of mat, however, all operations will be performed as 
;; floating point operations (add, sub, mul, div).

;; Matrices in newLISP are two-dimensional lists or arrays. Internally, newLISP 
;; translates lists and arrays into fast, accessible C-language data objects. 
;; This makes matrix operations in newLISP as fast as those coded directly in C. 
;; The same is true for the matrix operations multiply and invert.

(set 'A '((1 2 3) (4 5 6)))
(set 'B A)

(mat + A B)    ;; → ((2 4 6) (8 10 12))
(mat - A B)    ;; → ((0 0 0) (0 0 0))
(mat * A B)    ;; → ((1 4 9) (16 25 36))
(mat / A B)    ;; → ((1 1 1) (1 1 1))

; specify the operator in a variable

(set 'op +)
(mat op A B)    ;; → ((2 4 6) (8 10 12)) 

;; Using the second syntax, all cells in matrix-A are operated 
;; on with a scalar in number:

(mat + A 5)    ;; → ((6 7 8) (9 10 11))
(mat - A 2)    ;; → ((-1 0 1) (2 3 4))
(mat * A 3)    ;; → ((3 6 9) (12 15 18))
(mat / A 10)   ;; → ((.1 .2 .3) (.4 .5 .6))


;; See also the other matrix operations det, invert, multiply, and transpose.


;; match
;; syntax: (match list-pattern list-match [bool])

;; The pattern in list-pattern is matched against the list in list-match, 
;; and the matching expressions are returned in a list. The three wildcard 
;; characters ?, +, and * can be used in list-pattern.

;; Wildcard characters may be nested. match returns a list of matched expressions. 
;; For each ? (question mark), a matching expression element is returned. For each 
;; + (plus sign) or * (asterisk), a list containing the matched elements is returned. 
;; If the pattern cannot be matched against the list in list-match, match returns nil. 
;; If no wildcard characters are present in the pattern an empty list is returned.

;; Optionally, the Boolean value true (or any other expression not evaluating to nil) 
;; can be supplied as a third argument. This causes match to show all elements in the 
;; returned result.

;; match is frequently employed as a functor parameter in find, ref, ref-all and replace 
;; and is internally used by find-all for lists.

(match '(a ? c) '(a b c))  ;; → (b)

(match '(a ? ?) '(a b c))  ;; → (b c)

(match '(a ? c) '(a (x y z) c))  ;; → ((x y z))

(match '(a ? c) '(a (x y z) c) true)  ;; → (a (x y z) c)

(match '(a ? c) '(a x y z c))  ;; → nil


(match '(a * c) '(a x y z c))  ;; → ((x y z))

(match '(a (b c ?) x y z) '(a (b c d) x y z))  ;; → (d)

(match '(a (*) x ? z) '(a (b c d) x y z))  ;; → ((b c d) y)


(match '(+) '())  ;; → nil

(match '(+) '(a))  ;; → ((a))

(match '(+) '(a b))  ;; → ((a b))

(match '(a (*) x ? z) '(a () x y z))  ;; → (() y)

(match '(a (+) x ? z) '(a () x y z))  ;; → nil 

;; Note that the * operator tries to grab the fewest number of elements possible, 
;; but match backtracks and grabs more elements if a match cannot be found.

;; The + operator works similarly to the * operator, but it requires at least one 
;; list element.

;; The following example shows how the matched expressions can be bound to variables.

(map set '(x y) (match '(a (? c) d *) '(a (b c) d e f)))

x  ;; → b
y  ;; → (e f)

;; Note that match for strings has been eliminated. For more powerful 
;; string matching, use regex, find, find-all or parse.

;; unify is another function for matching expressions in a 
;; PROLOG like manner.


;; max
;; syntax: (max num-1 [num-2 ... ])

;; Evaluates the expressions num-1— and returns the largest number.

(max 4 6 2 3.54 7.1)  ;; → 7.1


;; member
;; syntax: (member exp list)
;; syntax: (member str-key str [num-option])

;; In the first syntax, member searches for the element exp in the list list. 
;; If the element is a member of the list, a new list starting with the element 
;; found and the rest of the original list is constructed and returned. If nothing 
;; is found, nil is returned. When specifying num-option, member performs a regular 
;; expression search.

(set 'aList '(a b c d e f g h))  ;; → (a b c d e f g h)
(member 'd aList)                ;; → (d e f g h)
(member 55 aList)                ;; → nil


;; In the second syntax, member searches for str-key in str. If str-key is found, 
;; all of str (starting with str-key) is returned. nil is returned if nothing 
;; is found.

(member "LISP" "newLISP")  ;; → "LISP"
(member "LI" "newLISP")    ;; → "LISP"
(member "" "newLISP")      ;; → "newLISP"
(member "xyz" "newLISP")   ;; → nil
(member "li" "newLISP" 1)  ;; → "LISP"


;; The third syntax of member allows for a regular expression search. If the
;; regular expression matches str-key, all of str (starting with str-key) is
;; returned. If no match is found, nil is returned.


;; min
;; syntax: (min num-1 [num-2 ... ])

;; Evaluates the expressions num-1— and returns the smallest number.


;; not
;; syntax: (not exp)

;; Evaluates the expression exp and returns true if the result is nil, and false
;; otherwise.

;; min
;; syntax: (min num-1 [num-2 ... ])

;; Evaluates the expressions num-1— and returns the smallest number.

(min 4 6 2 3.54 7.1)  ;; → 2

;; See also the max function. 

;; mod
;; syntax: (mod num-1 num-2 [num-3 ... ])
;; syntax: (mod num-1)

;; Calculates the modular value of the numbers in num-1 and num-2. mod computes 
;; the remainder from the division of the numerator num-i by the denominator 
;; num-i + 1. Specifically, the return value is numerator - n * denominator, 
;; where n is the quotient of the numerator divided by the denominator, 
;; rounded towards zero to an integer. The result has the same sign as 
;; the numerator and its magnitude is less than the magnitude of the 
;; denominator.

;; In the second syntax 1 is assumed for num-2 and the result is the fractional 
;; part of num-1.

(mod 10.5 3.3)   ;; →  0.6
(mod -10.5 3.3)  ;; → -0.6
(mod -10.5)      ;; → -0.5


;; Use the % (percent sign) function when working with integers only.


;; mul
;; syntax: (mul num-1 num-2 [num-3 ... ])

;; Evaluates all expressions num-1—, calculating and returning the product. 
;; mul can perform mixed-type arithmetic, but it always returns floating point 
;; numbers. Any floating point calculation with NaN also returns NaN.

(mul 1 2 3 4 5 1.1)  ;; → 132
(mul 0.5 0.5)        ;; → 0.25

;; When multiplying a matrix with a vector of n elements, the vector must 
;; be transformed into n rows by 1 column matrix using transpose.

;; All operations shown here on lists can be performed on arrays, as well.

;; See also the matrix operations det, invert, mat and transpose.


;; name

;; This function is deprecated, use term instead.

;; NaN?
;; syntax: (NaN? float)

;; Tests if the result of a floating point math operation is a NaN. 
;; Certain floating point operations return a special IEEE 754 number 
;; format called a NaN for 'Not a Number'.

; floating point operation on NaN yield NaN
(set 'x (sqrt -1))  ;; → NaN
(NaN? x)            ;; → true
(add x 123)         ;; → NaN
(mul x 123)         ;; → NaN

; integer operations treat NaN as zero
(+ x 123)  ;; → 123
(* x 123)  ;; → 0

; comparisons with NaN values yield nil
(> x 0)   ;; → nil
(<= x 0)  ;; → nil
(= x x)   ;; → nil

(set 'infinity (mul 1.0e200 1.0e200)) ;; → inf
(NaN? (sub infinity infinity)) ;; → true

;; Note that all floating point arithmetic operations with a NaN yield a NaN. 
;; All comparisons with NaN return nil, but true when comparing to itself. Comparison 
;; with itself, however, would result in not true when using ANSI C. Integer operations 
;; treat NaN as 0 (zero) values.

;; See also inf? for testing a floating point value for infinity.


;; net-accept
;; syntax: (net-accept int-socket)

;; Accepts a connection on a socket previously put into listening mode. 
;; Returns a newly created socket handle for receiving and sending data 
;; on this connection.

(set 'socket (net-listen 1234))

;; Note that for ports less than 1024, newLISP must be started in superuser 
;; mode on Unix-like operating systems.

;; See also the files server and client examples in the examples/ directory 
;; of the source distribution.

;; net-close
;; syntax: (net-close int-socket [true])

;; Closes a network socket in int-socket that was previously created by a net-connect 
;; or net-accept function. Returns true on success and nil on failure.

(net-close aSock)

;; The optional true flag suppresses immediate shutdown of sockets by waiting for pending 
;; data transmissions to finish.

;; net-connect
;; syntax: (net-connect str-remote-host int-port [int-timeout-ms])
;; syntax: (net-connect str-remote-host int-port [str-mode [int-ttl]])
;; syntax: (net-connect str-file-path)

;; In the first syntax, connects to a remote host computer specified in 
;; str-remote-host and a port specified in int-port. Returns a socket handle 
;; after having connected successfully; otherwise, returns nil.

(set 'socket (net-connect "example.com" 80))
(net-send socket "GET /\r\n\r\n")
(net-receive socket buffer 10000)
(println buffer)
(exit)

;; If successful, the net-connect function returns a socket number which can 
;; be used to send and receive information from the host. In the example a HTTP 
;; GET request is sent and subsequently a web page received. Note that newLISP 
;; has already a built-in function get-url offering the same functionality.

;; Optionally a timeout value int-timeout in milliseconds can be specified. 
;; Without a timeout value the function will wait up to 10 seconds for an open 
;; port. With a timeout value the function can be made to return on an unavailable 
;; port much earlier or later. The following example shows a port scanner looking 
;; for open ports:

(set 'host (main-args 2))
(println "Scanning: " host)
(for (port 1 1024)
    (if (set 'socket (net-connect host port 500))
        (println "open port: " port " " (or (net-service port "tcp") ""))
        (print port "\r"))
)

;; The programs takes the host string from the shell command line as either 
;; a domain name or an IP number in dot notation then tries to open each port 
;; from 1 to 1024. For each open port the port number and the service description 
;; string is printed. If no description is available, an empty string "" is output. 
;; For closed ports the function outputs numbers in the shell window staying on the 
;; same line.

;; On Unix net-connect may return with nil before the timeout expires, when the port 
;; is not available. On MS Windows net-connect will always wait for the timeout to 
;; expire before failing with nil.
;; UDP communications

;; In the second syntax, a third parameter, the string "udp" or "u" can be specified 
;; in the optional str-mode to create a socket suited for UDP (User Datagram Protocol) 
;; communications. In UDP mode, net-connect does not try to connect to the remote host, 
;; but creates the socket and binds it to the remote address, if an address is specified. 
;; A subsequent net-send will send a UDP packet containing that target address. When using 
;; net-send-to, only one of the two functions net-connect or net-send-to should provide a 
;; target address. The other function should specify and empty string "" as the target 
;; address.

;; example server
(net-listen 4096 "226.0.0.1" "udp")  ;; → 5
(net-receive-from 5 20)

;; example client I
(net-connect "226.0.0.1" 4096 "udp") ;; → 3
(net-send 3 "hello")

;; example client II
(net-connect "" 4096 "udp") ;; → 3
(net-send-to "226.0.0.1" 4096 "hello" 3)


;; The functions net-receive and net-receive-from can both be used and will 
;; perform UDP communications when the "udp" option as been used in net-listen 
;; or net-connect. net-select and net-peek can be used to check for received data 
;; in a non-blocking fashion.

;; net-listen binds a specific local address and port to the socket. When net-connect 
;; is used, the local address and port will be picked by the socket-stack functions 
;; of the host OS.
;; UDP multicast communications

;; When specifying "multi" or "m" as a third parameter for str-mode, a socket for UDP 
;; multicast communications will be created. Optionally, the fourth parameter int-ttl 
;; can be specified as a TTL (time to live) value. If no int-ttl value is specified, 
;; a value of 3 is assumed.

;; Note that specifying UDP multicast mode in net-connect does not actually establish 
;; a connection to the target multicast address but only puts the socket into UDP 
;; multicasting mode. On the receiving side, use net-listen together with the UDP 
;; multicast option.

;; example client I
(net-connect "" 4096 "multi")  ;; → 3
(net-send-to "226.0.0.1" 4096 "hello" 3)

;; example client II
(net-connect "226.0.0.1" 4096 "multi")  ;; → 3
(net-send 3 "hello")

;; example server
(net-listen 4096 "226.0.0.1" "multi")  ;; → 5
(net-receive-from 5 20)               
;; → ("hello" "192.168.1.94" 32769)

;;  On the server side, net-peek or net-select can be used for non-blocking 
;; communications. In the above example, the server would block until a datagram 
;; is received.

;; The address 226.0.0.1 is just one multicast address in the Class D range of 
;; multicast addresses from 224.0.0.0 to 239.255.255.255.

;; The net-send and net-receive functions can also be used instead of net-send-to 
;; and net-receive-from.
;; UDP broadcast communications

;; Specifying the string "broadcast" or "b" in the third parameter, str-mode, causes 
;; UDP broadcast communications to be set up. In this case, the broadcast address 
;; ending in 255 is used. 

;; example client
(net-connect "192.168.2.255" 3000 "broadcast")  ;; → 3
(net-send 3 "hello")

;; example server
(net-listen 3000 "" "udp")  ;; → 5

(net-receive 5 buff 10)
buff ;;  → "hello"
;; or
(net-receive-from 5 10)
;; → ("hello" "192.168.2.1" 46620)

;; Note that on the receiving side, net-listen should be used with the 
;; default address specified with an "" (empty string). Broadcasts will 
;; not be received when specifying an address. As with all UDP communications, 
;; net-listen does not actually put the receiving side in listen mode, but rather 
;; sets up the sockets for the specific UDP mode.

;; The net-select or net-peek functions can be used to check for incoming communications 
;; in a non-blocking fashion.
;; Local domain Unix sockets

;; In the third syntax, net-connect connects to a server on the local file system via a 
;; local domain Unix socket named using str-file-path. Returns a socket handle after having 
;; connected successfully; otherwise, returns nil.

(net-connect "/tmp/mysocket")  ;; → 3

; on OS/2 use "\\socket\\" prefix

(net-connect "\\socket\\mysocket")


(net-error) ;; → nil

(net-connect "jhghjgkjhg" 80)  ;;→  nil

(net-error)  ;; →  (2 "ERR: "DNS resolution failed") 

;; When int-error is specified the number and error text for that error 
;; number is returned.

(net-error 10) ;; → (10 "Cannot bind socket")

;; See also last-error and sys-error.

;; net-eval
;; syntax: (net-eval str-host int-port exp [int-timeout [func-handler]])
;; syntax: (net-eval '((str-host int-port exp) ... ) [int-timeout [func-handler]])

;; Can be used to evaluate source remotely on one or more newLISP servers. This function 
;; handles all communications necessary to connect to the remote servers, send source for 
;; evaluation, and wait and collect responses.

;; The expression in exp will be evaluated remotely in the environment of the target node. 
;; The exp is either a quoted expression, or it is enclosed in string delimiters. For bigger 
;; expressions [text] ... [/text] delimiters can be used instead of double quotes " ... ". 
;; Only one expression should be enclosed in the string. When more than one are specified, 
;; all will get evaluated in the target node, but only the result of the first will be 
;; returned.

;; The remote TCP/IP servers are started in the following way:

newlisp -c -d 4711 &

; preloading function definitions

newlisp preload.lsp -c -d 12345 &

; logging connections

newlisp -l -c -d 4711 &

; communicating via Unix local domain sockets

newlisp -c /tmp/mysocket

;; The -c option is necessary to suppress newLISP emitting prompts.

;; The -d daemon mode allows newLISP to maintain state between connections. 
;; When keeping state between connections is not desired, the inetd daemon 
;; mode offers more advantages. The Internet inetd or xinetd services daemon 
;; will start a new newLISP process for each client connection. This makes 
;; for much faster servicing of multiple connections. In -d daemon mode, 
;; each new client request would have to wait for the previous request to 
;; be finished. See the chapter inetd daemon mode on how to configure 
;; this mode correctly.

;; Instead of 4711, any other port number can be used. Multiple nodes can 
;; be started on different hosts and with the same or different port numbers. 
;; The -l or -L logging options can be specified to log connections and remote 
;; commands.

;; In the first syntax, net-eval talks to only one remote newLISP server node, 
;; sending the host in str-host on port int-port a request to evaluate the 
;; expression exp. If int-timeout is not given, net-eval will wait up to 60 seconds 
;; for a response after a connection is made. Otherwise, if the timeout in milliseconds 
;; has expired, nil is returned; else, the evaluation result of exp is returned.

; the code to be evaluated is given in a quoted expression
(net-eval "192.168.1.94" 4711 '(+ 3 4))       ;; → 7

; expression as a string (only one expression should be in the string)
(net-eval "192.168.1.94" 4711 "(+ 3 4)")      ;; → 7

; with timeout
(net-eval "192.168.1.94" 4711 '(+ 3 4) 1)     ;; → nil  ; 1ms timeout too short
(net-error)                                   ;; → (17 "ERR: Operation timed out")

(net-eval "192.168.1.94" 4711 '(+ 3 4) 1000)  ;; → 7

; program contained in a variable
(set 'prog '(+ 3 4))
(net-eval "192.168.1.94" 4711 prog)           ;; → 7

; specify a local-domain Unix socket (not available on MS Windows)
(net-eval "/tmp/mysocket" 0 '(+ 3 4))         ;; → 7


;; The second syntax of net-eval returns a list of the results after all of the 
;; responses are collected or timeout occurs. Responses that time out return nil. 
;; The last example line shows how to specify a local-domain Unix socket specifying 
;; the socket path and a port number of 0. Connection errors or errors that occur 
;; when sending information to nodes are returned as a list of error numbers and 
;; descriptive error strings. See the function net-error for a list of potential 
;; error messages.

; two different remote nodes different IPs
(net-eval '(
    ("192.168.1.94" 4711 '(+ 3 4)) 
    ("192.168.1.95" 4711 '(+ 5 6))
    ) 5000)
;; → (7 11)

; two persistent nodes on the same CPU different ports
(net-eval '(
    ("localhost" 8081 '(foo "abc")) 
    ("localhost" 8082 '(myfunc 123)') 
    ) 3000)

; inetd or xinetd nodes on the same server and port
; nodes are loaded on demand
(net-eval '(
    ("localhost" 2000 '(foo "abc")) 
    ("localhost" 2000 '(myfunc 123))
    ) 3000)

;; The first example shows two expressions evaluated on two different remote nodes. 
;; In the second example, both nodes run on the local computer. This may be useful 
;; when debugging or taking advantage of multiple CPUs on the same computer. When 
;; specifying 0 for the port number , net-eval takes the host name as the file path 
;; to the local-domain Unix socket.

;; Note that definitions of foo and myfunc must both exist in the target environment. 
;; This can be done using a net-eval sending define statements before. It also can be 
;; done by preloading code when starting remote nodes.

;; When nodes are inetd or xinetd-controlled, several nodes may have the same IP address 
;; and port number. In this case, the Unix daemon inetd or xinetd will start multiple 
;; newLISP servers on demand. This is useful when testing distributed programs on just 
;; one machine. The last example illustrates this case. It is also useful on multi core 
;; CPUs, where the platform OS can distribute different processes on to different CPU 
;; cores.

;; The source sent for evaluation can consist of entire multiline programs. This way, 
;; remote nodes can be loaded with programs first, then specific functions can be called. 
;; For large program files, the functions put-url or save (with a URL file name) can be 
;; used to transfer programs. The a net-eval statement could load these programs.

;; Optionally, a handler function can be specified. This function will be repeatedly 
;; called while waiting and once for every remote evaluation completion.

(define (myhandler param)
    (if param
        (println param))
)

(set 'Nodes '(
    ("192.168.1.94" 4711)
    ("192.168.1.95" 4711)
))

(set 'Progs '(
    (+ 3 4)
    (+ 5 6)
))

(net-eval (map (fn (n p) (list (n 0) (n 1) p)) Nodes Progs) 5000 myhandler)
;; → ("192.168.1.94" 4711 7)
;; ("192.168.1.95" 4711 11)

;; The example shows how the list of node specs can be assembled from a list 
;; of nodes and sources to evaluate. This may be useful when connecting to a 
;; larger number of remote nodes.

(net-eval (list 
  (list (Nodes 0 0) (Nodes 0 1) (Progs 0)) 
  (list (Nodes 1 0) (Nodes 1 1) (Progs 1)) 
 ) 3000 myhandler)


;; While waiting for input from remote hosts, myhandler will be called with nil 
;; as the argument to param. When a remote node result is completely received, 
;; myhandler will be called with param set to a list containing the remote host 
;; name or IP number, the port, and the resulting expression. net-eval will 
;; return true before a timeout or nil if the timeout was reached or exceeded. 
;; All remote hosts that exceeded the timeout limit will contain a nil in their 
;; results list.

;; For a longer example see this program: mapreduce. The example shows how a word 
;; counting task gets distributed to three remote nodes. The three nodes count words 
;; in different texts and the master node receives and consolidates the results.


;; net-interface
;; syntax: (net-interface str-ip-addr)
;; syntax: (net-interface)

;; Sets the default local interface address to be used for network connections. 
;; If not set then network functions will default to an internal default address, 
;; except when overwritten by an optional interface address given in net-listen.

;; When no str-ip-addr is specified, the current default is returned. If the 
;; net-interface has not been used yet to specify an IP address, the address 0.0.0.0 
;; is returned. This means that all network routines will use the default address 
;; preconfigured by the underlying operating system.

;; This function has only usage on multihomed servers with either multiple network 
;; interface hardware or otherwise supplied multiple IP numbers. On all other machines 
;; network functions will automatically select the single network interface installed.

;; On error the function returns nil and net-error can be used to report the error.

(net-interface "192.168.1.95")  ;; → "192.168.1.95"
(net-interface "localhost")     ;; → "127.0.0.1"

;; An interface address can be defined as either an IP address or a name. 
;; The return value is the address given in str-ip-addr


;; net-ipv
;; syntax: (net-ipv int-version)
;; syntax: (net-ipv)

;; Switches between IPv4 and IPv6 internet protocol versions. int-version 
;; contains either a 4 for IPv4 or a 6 for IPv6. When no parameter is given, 
;; net-ipv returns the current setting.

(net-ipv)      ;; → 4
(net-ipv 6)    ;; → 6


;; By default newLISP starts up in IPv4 mode. The IPv6 protocol mode can also 
;; be specified from the commandline when starting newlisp:

newlisp -6

;; Once a socket is connected with either net-connect or listened on with net-listen, 
;; the net-accept, net-select, net-send, net-receive and net-receive-from functions 
;; automatically adjust to the address protocol used when creating the sockets. Different 
;; connections with different IPv4/6 settings can be open at the same time.

;; Note, that currently net-packet does not support IPv6 and will work in IPv4 mode 
;; regardless of settings.


;; net-listen
;; syntax: (net-listen int-port [str-ip-addr [str-mode]])
;; syntax: (net-listen str-file-path)

;; Listens on a port specified in int-port. A call to net-listen returns immediately 
;; with a socket number, which is then used by the blocking net-accept function to wait 
;; for a connection. As soon as a connection is accepted, net-accept returns a socket 
;; number that can be used to communicate with the connecting client.

(set 'port 1234)
(set 'listen (net-listen port))
(unless listen (begin
    (print "listening failed\n")
    (exit)))

(print "Waiting for connection on: " port "\n")

(set 'connection (net-accept listen))
(if connection
    (while (net-receive connection buff 1024 "\n")
        (print buff)
        (if (= buff "\r\n") (exit)))
    (print "Could not connect\n"))

;; The example waits for a connection on port 1234, then reads incoming lines 
;; until an empty line is received. Note that listening on ports lower than 1024 
;; may require superuser access on Unix systems.

;; On computers with more than one interface card, specifying an optional interface 
;; IP address or name in str-ip-addr directs net-listen to listen on the specified 
;; address.

;; listen on a specific address
(net-listen port "192.168.1.54") 


;; Local domain Unix sockets

;; In the second syntax, net-listen listens for a client on the local 
;; file system via a local domain Unix socket named using str-file-path. 
;; If successful, returns a socket handle that can be used with net-accept 
;; to accept a client connection; otherwise, returns nil.

(net-listen "/tmp/mysocket")  ;; → 5

; on OS/2 use "\\socket\\" prefix

(net-listen "\\socket\\mysocket")

(net-accept 5)


;; A local domain file system socket is created and listened on. A client will 
;; try to connect using the same str-file-path. After a connection has been accepted 
;; the functions net-select, net-send and net-receive can be used as usual for TCP/IP 
;; stream communications. This type of connection can be used as a fast bi-directional 
;; communications channel between processes on the same file system. This type of 
;; connection is not available on MS Windows platforms.
;; UDP communications

;; As a third parameter, the optional string "udp" or "u" can be specified in str-mode 
;; to create a socket suited for UDP (User Datagram Protocol) communications. A socket 
;; created in this way can be used directly with net-receive-from to await incoming UDP 
;; data without using net-accept, which is only used in TCP communications. 
;; The net-receive-from call will block until a UDP data packet is received. 
;; Alternatively, net-select or net-peek can be used to check for ready data 
;; in a non-blocking fashion. To send data back to the address and port received 
;; with net-receive-from, use net-send-to.

;; Note that net-peer will not work, as UDP communications do not maintain a connected 
;; socket with address information.

(net-listen 10002 "192.168.1.120" "udp") 

(net-listen 10002 "" "udp") 


;; The first example listens on a specific network adapter, while the second 
;; example listens on the default adapter. Both calls return a socket number 
;; that can be used in subsequent net-receive, net-receive-from, net-send-to, 
;; net-select, or net-peek function calls.

;; Both a UDP server and UDP client can be set up using net-listen with the "udp" 
;; option. In this mode, net-listen does not really listen as in TCP/IP communications; 
;; it just binds the socket to the local interface address and port.

;; For a working example, see the files examples/client and examples/server in the 
;; newLISP source distribution.

;; Instead of net-listen and the "udp" option, the functions net-receive-udp 
;; and net-send-udp can be used for short transactions consisting only of one 
;; data packet.

;; net-listen, net-select, and net-peek can be used to facilitate non-blocking 
;; reading. The listening/reading socket is not closed but is used again for 
;; subsequent reads. In contrast, when the net-receive-udp and net-send-udp 
;; pair is used, both sides close the sockets after sending and receiving.
;; UDP multicast communications

;; If the optional string str-mode is specified as "multi" or "m", net-listen 
;; returns a socket suitable for multicasting. In this case, str-ip-addr contains 
;; one of the multicast addresses in the range 224.0.0.0 to 239.255.255.255. 
;; net-listen will register str-ip-addr as an address on which to receive 
;; multicast transmissions. This address should not be confused with the IP address 
;; of the server host.

;; example client

(net-connect "226.0.0.1" 4096 "multi")  ;; → 3

(net-send-to "226.0.0.1" 4096 "hello" 3)


;; On the server side, net-peek or net-select can be used for non-blocking 
;; communications. In the example above, the server would block until a datagram 
;; is received.

;; The net-send and net-receive functions can be used instead of net-send-to 
;; and net-receive-from.
;; Packet divert sockets and ports

;; If str-mode is specified as "divert" or "d", a divert socket can be created 
;; for a divert port in int-port on BSD like platforms. The content of IP address 
;; in str-ip-addr is ignored and can be specified as an empty string. Only the 
;; int-port is relevant and will be bound to the raw socket returned.

;; To use the divert option in net-listen, newLISP must run in super-user mode. 
;; This option is only available on Unix like platforms.

;; The divert socket will receive all raw packets diverted to the divert port. 
;; Packets may also be written back to a divert socket, in which case they re-enter 
;; OS kernel IP packet processing.

;; Rules for packet diversion to the divert port must be defined using either the 
;; ipfw BSD or ipchains Linux configuration utilities.

;; The net-receive-from and net-send-to functions are used to read and write raw 
;; packets on the divert socket created and returned by the net-listen statement. 
;; The same address received by net-receive-from is used in the net-send-to call 
;; when re-injecting the packet:

; rules have been previously configured for a divert port
(set 'divertSocket (net-listen divertPort "" "divert"))

(until (net-error)
    (set 'rlist (net-receive-from divertSocket maxBytes))
    (set 'buffer (rlist 1))
    ; buffer can be processed here before reinjecting
    (net-send-to (rlist 0) divertPort buffer divertSocket)
)


For more information see the Unix man pages for divert and the ipfw (BSDs) or ipchains (Linux) configuration utilities.


;; net-local
;; syntax: (net-local int-socket)

;; Returns the IP number and port of the local computer for a connection on a specific 
;; int-socket.

(net-local 16)  ;; → ("204.179.131.73" 1689)

;; Use the net-peer function to access the remote computer's IP number and port. 

;; example server

(net-listen 4096 "226.0.0.1" "multi")  ;; → 5

(net-receive-from 5 20)               
;; → ("hello" "192.168.1.94" 32769)

;; net-lookup
;; syntax: (net-lookup str-ip-number)
;; syntax: (net-lookup str-hostname [bool])

;; Returns either a hostname string from str-ip-number in IP dot format or the IP 
;; number in dot format from str-hostname:

(net-lookup "209.24.120.224")    ;; → "www.nuevatec.com"
(net-lookup "www.nuevatec.com")  ;; → "209.24.120.224"

(net-lookup "216.16.84.66.sbl-xbl.spamhaus.org" true)
;; → "216.16.84.66"

;; Optionally, a bool flag can be specified in the second syntax. 
;; If the expression in bool evaluates to anything other than nil, 
;; host-by-name lookup will be forced, even if the name string starts 
;; with an IP number.


;; net-packet
;; syntax: (net-packet str-packet)

;; The function allows custom configured network packets to be sent via a raw 
;; sockets interface. The packet in str-packet must start with an IP (Internet Protocol) 
;; header followed by either a TCP, UDP or ICMP header and optional data. newLISP must 
;; be run with super user privileges, and this function is only available on macOS, 
;; Linux and other Unix operating systems and only for IPv4. Currently net-packet 
;; is IPv4 only and has been tested on macOS, Linux and OpenBSD.

;; On success the function returns the number of bytes sent. On failure the function 
;; returns nil and both, net-error and sys-error, should be inspected.

;; When custom configured packets contain zeros in the checksum fields, net-packet 
;; will calculate and insert the correct checksums. Already existing checksums 
;; stay untouched.

;; The following example injects a UDP packet for IP number 192.168.1.92. 
;; The IP header consists of 20 bytes ending with the target IP number. 
;; The following UDP header has a length of 8 bytes and is followed by the 
;; data string Hello World. The checksum bytes in both headers are left 
;; as 0x00 0x00 and will be recalculated internally.

; packet as generated by: (net-send-udp "192.168.1.92" 12345 "Hello World")

(set 'udp-packet (pack (dup "b" 39) '(
    0x45 0x00 0x00 0x27 0x4b 0x8f 0x00 0x00 0x40 0x11 0x00 0x00 192  168  1    95
    192  168  1    92   0xf2 0xc8 0x30 0x39 0x00 0x13 0x00 0x00 0x48 0x65 0x6c 0x6c
    0x6f 0x20 0x57 0x6f 0x72 0x6c 0x64)))

(unless (net-packet udp-packet)
    (println "net-error: " (net-error))
    (println "sys-error: " (sys-error)))


;; The net-packet function is used when testing net security. Its wrong application 
;; can upset the correct functioning of network routers and other devices connected 
;; to a network. For this reason the function should only be used on well isolated, 
;; private intra-nets and only by network professionals.

;; For other examples of packet configuration, see the file qa-specific-tests/qa-packet 
;; in the newLISP source distribution.


;; net-peek
;; syntax: (net-peek int-socket)

;; Returns the number of bytes ready for reading on the network socket int-socket. 
;; If an error occurs or the connection is closed, nil is returned.

(set 'aSock (net-connect "aserver.com" 123))

(while ( = (net-peek aSock) 0) 
    (do-something-else))

(net-receive aSock buff 1024)

;; After connecting, the program waits in a while loop until 
;; aSock can be read.

;; Use the peek function to check file descriptors and stdin.


;; net-peer
;; syntax: (net-peer int-socket)

;; Returns the IP number and port number of the remote computer for a connection 
;; on int-socket.

(net-peer 16)  ;; → ("192.100.81.100" 13)
