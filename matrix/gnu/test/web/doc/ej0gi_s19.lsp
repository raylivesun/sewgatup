;; The inverse function to int with base 2 is bits.

;; Use the float function to convert arguments to floating point numbers.


;; integer?
;; syntax: (integer? exp)

;; Returns true only if the value of exp is an integer; otherwise, it returns nil. 

(set 'num 123)  ;; → 123
(integer? num)  ;; → true

;; intersect
;; syntax: (intersect list-A list-B)
;; syntax: (intersect list-A list-B bool)

;; In the first syntax, intersect returns a list containing one copy of each 
;; element found both in list-A and list-B.

(intersect '(3 0 1 3 2 3 4 2 1) '(1 4 2 5))  
;; → (2 4 1)

;; In the second syntax, intersect returns a list containing one copy of each
;; element found both in list-A and list-B, as well as all elements that appear
;; only in list-B.
(intersect '(3 0 1 3 2 3 4 2 1) '(3 0 1 1))


;; ;; In the second syntax, intersect returns a list containing one copy of each
;; ;; element found both in list-A and list-B, as well as all elements that
;; ;; appear only in list-B.
;; In the second syntax, intersect returns a list of all elements in list-A that 
;; are also in list-B, without eliminating duplicates in list-A. bool is an expression 
;; evaluating to true or any other value not nil.

(intersect '(3 0 1 3 2 3 4 2 1) '(1 4 2 5) true)
;; → (1 2 4 2 1)


;; invert
;; syntax: (invert matrix [float-pivot])

;; Returns the inversion of a two-dimensional matrix in matrix. The matrix must 
;; be square, with the same number of rows and columns, and non-singular (invertible). 
;; Matrix inversion can be used to solve systems of linear equations (e.g., multiple 
;; regression in statistics). newLISP uses LU-decomposition of the matrix to find 
;; the inverse.

;; Optionally 0.0 or a very small value can be specified in float-pivot. This value 
;; substitutes pivot elements in the LU-decomposition algorithm, which result in zero 
;; when the algorithm deals with a singular matrix.

;; The dimensions of a matrix are defined by the number of rows times the number 
;; of elements in the first row. For missing elements in non-rectangular matrices, 
;; 0.0 (zero) is assumed. A matrix can either be a nested list or an array.

(set 'A '((-1 1 1) (1 4 -5) (1 -2 0)))
(invert A)  ;; → ((10 2 9) (5 1 4) (6 1 5))
(invert (invert A)) ;; → ((-1 1 1) (1 4 -5) (1 -2 0))

; solve Ax = b for x
(multiply (invert A) '((1) (2) (3))) ;; → ((41) (19) (23))

; treatment of singular matrices
(invert '((2 -1) (4 -2)))        ;; → nil
(invert '((2 -1) (4 -2)) 0.0)    ;; → ((inf -inf) (inf -inf))
(invert '((2 -1) (4 -2)) 1e-20)  ;; → ((5e+19 -2.5e+19) (1e+20 -5e+19)) 

;; invert will return nil if the matrix is singular and cannot be inverted, 
;; and float-pivot is not specified.

;; All operations shown here on lists can be performed on arrays, as well.

;; See also the matrix functions det, mat, multiply and transpose.


;; irr
;; syntax: (irr list-amounts [list-times [num-guess]])

;; Calculates the internal rate of return of a cash flow per time period. 
;; The internal rate of return is the interest rate that makes the present 
;; value of a cash flow equal to 0.0 (zero). In-flowing (negative values) 
;; and out-flowing (positive values) amounts are specified in list-amounts. 
;; If no time periods are specified in list-times, amounts in list-amounts 
;; correspond to consecutive time periods increasing by 1 (1, 2, 3—). The 
;; algorithm used is iterative, with an initial guess of 0.5 (50 percent). 
;; Optionally, a different initial guess can be specified. The algorithm 
;; returns when a precision of 0.000001 (0.0001 percent) is reached. nil 
;; is returned if the algorithm cannot converge after 50 iterations.

;; irr is often used to decide between different types of investments.

(irr '(-1000 500 400 300 200 100))  
;; → 0.2027

(npv 0.2027 '(500 400 300 200 100)) 
;; → 1000.033848 ; ~ 1000

(irr '(-1000 500 400 300 200 100) '(0 3 4 5 6 7)) 
;; → 0.0998

(irr '(-5000 -2000 5000 6000) '(0 3 12 18)) 
;; → 0.0321

;; If an initial investment of 1,000 yields 500 after the first year, 400 after two years, 
;; and so on, finally reaching 0.0 (zero) after five years, then that corresponds to a 
;; yearly return of about 20.2 percent. The next line demonstrates the relation between 
;; irr and npv. Only 9.9 percent returns are necessary when making the first withdrawal 
;; after three years.

;; In the last example, securities were initially purchased for 5,000, then for another 
;; 2,000 three months later. After a year, securities for 5,000 are sold. Selling the 
;; remaining securities after 18 months renders 6,000. The internal rate of return 
;; is 3.2 percent per month, or about 57 percent in 18 months.

;; See also the fv, nper, npv, pmt, and pv functions.


;; json-error
;; syntax: (json-error)

;; When json-parse returns nil due to a failed JSON data translation, 
;; this function retrieves an error description and the last scan position 
;; of the parser.

; failed parse returns nil
(json-parse [text]{"address" "http://example.com"}[/text]) ;;→ nil

; inspect the error information
(json-error) ;; → ("missing : colon" 11)


;; The file is read, parsed and the resulting S-expression stored in jsp:

(set 'jsp (json-parse (read-file ".vscode/author.json")))
;;→
( ("name" "John Smith") 
("age" 32) 
("employed" true) 
("address" ( ("street" "701 First Ave.") 
       ("city" "Sunnyvale, CA 95125") 
       ("country" "United States")) ) 
("children" (
(("name" "Richard") ("age" 7)) 
(("name" "Susan") ("age" 4)) 
(("name" "James") ("age" 3))) )
)


;; Data can be extracted using assoc, lookup or ref:

; the address
(lookup "address" jsp)
;; → (("street" "701 First Ave.") ("city" "Sunnyvale, CA 95125") ("country" "United States"))

; the city of the address
(lookup "city" (lookup "address" jsp)) 
;; → "Sunnyvale, CA 95125"

; a child named Susan
(ref '(( * "Susan") *) jsp match true) 
;; → (("name" "Susan") ("age" 4))

; all names
(map last (ref-all '("name" *) jsp match true)) 
;; → ("John Smith" "Richard" "Susan" "James")

; only names of children
(map last (ref-all '("name" *) (lookup "children" jsp) match true))
;; → ("Richard" "Susan" "James")

; names of children other method
(map last (map first (lookup "children" jsp)))
;; → ("Richard" "Susan" "James")


;; Although most of the time JSON object types are parsed, all JSON data types can 
;; be parsed directly, without occurring as part of a JSON object. The following 
;; examples show parsing of a JSON array:

; parse a JSON array data type

(json-parse "[1, 2, 3, 4, 5]") ;; → (1 2 3 4 5)


;; When the UTF-8 capable version of newLISP is used, JSON formatted Unicode gets 
;; translated into UTF-8:

; parse a JSON object data type ands Unicode
; the outer {,} are newLISP string delimiters [text],[/text] tags could also be used
; the inner {,} are JSON object delimiters

(json-parse { {"greek letters" : "\u03b1\u03b2\u03b3\u03b4"} }) → (("greek letters" "αβγδ"))

; strings longer than 2047 bytes should be delimted with [text], [/text] tags

(json-parse [text]{"greek letters" : "\u03b1\u03b2\u03b3\u03b4"}[/text]) → (("greek letters" "αβγδ"))


;; The hex-code representation of Unicoder characters in JSON is the same as can be used 
;; in UTF-8 enabled newLISP.

;; Because JSON objects contain {,}," characters, quotes should not be used to limit 
;; JSON data, or all quotes inside the JSON data would need a preceding backslash \. {,} 
;; braces can be used as long as braces inside the JSON data are balanced. The safest 
;; delimiter are [text], [/text] tags — they suppress all special processing of the 
;; string when read by newLISP and are suitable to delimit large data sizes greater 
;; 2047 bytes.


;; join
;; syntax: (join list-of-strings [str-joint [bool-trail-joint]])

;; Concatenates the given list of strings in list-of-strings. If str-joint is present, 
;; it is inserted between each string in the join. If bool-trail-joint is true then a 
;; joint string is also appended to the last string. 

(set 'lst '("this" "is" "a" "sentence"))

(join lst " ")  ;; → "this is a sentence"

(join (map string (slice (now) 0 3)) "-")  ;; → "2003-11-26"

(join (explode "keep it together"))  ;; → "keep it together"

(join '("A" "B" "C") "-")         ;; → "A-B-C"
(join '("A" "B" "C") "-" true)    ;; → "A-B-C-"


;; last
;; syntax: (last list)
;; See also the append, string, and explode functions, which are the inverse 
;; of the join operation.


;; kmeans-query
;; syntax: (kmeans-query list-data matrix-centroids)
;; syntax: (kmeans-query list-data matrix-data)

;; In the first usage, kmeans-query calculates the Euclidian distances from the data 
;; vector given in list-data to the centroids given in matrix-centroids. The data vector 
;; in list-data has m elements. The 2-dimensional list in matrix-centroids, result from 
;; a previous kmeans-train clustering, has k rows and m columns for k centroids measuring 
;; m features.

; centroids from previous kmeans-train
K:centroids ;; →
;; ( (6.39 7.188333333 5.935) 
;; (7.925714286 3.845714286 9.198571429) 
;; (2.207142857 2.881428571 0.8885714286) )

(kmeans-query '(1 2 3) K:centroids) ;; →
;; (8.036487279 9.475994267 2.58693657) ; distances to cluster 1, 2 and 3


;; The data record (1 2 3) shows the smallest distance to the 3rd cluster 
;; centroid and would be classified as belonging to that cluster.

;; In the second application kmeans-query calculates Euclidian distances 
;; to a list of other data points which are not centroids. The following 
;; example calculates distances of the (1 2 3) data vector to all original 
;; points from the original kmeans-train data analysis.

;; The data in matrix-data can be either a nested list or a 2-dimensional array.

;; This vector could be sorted for a subsequent kNN (k Nearest Neighbor) analysis:

(kmeans-query '(1 2 3) data) ;; →
;; (10.91671196 3.190626898 9.19723328 3.014415366 9.079763213 
;; 6.83130295 8.533111976 9.624816881 6.444261013 2.013107051 
;; 3.186549858 9.475199206 9.32936761 2.874786949 7.084638311 
;; 10.96221237 10.50080473 3.162419959 2.423674896 9.526436899)

; show distances to members in each cluster

; for cluster labeled 1
(select (kmeans-query '(1 2 3) data) (K:clusters 0)) ;; → 
;; (9.079763213 6.83130295 9.624816881 6.444261013 7.084638311 10.50080473)

; for cluster labeled 2
(select (kmeans-query '(1 2 3) data) (K:clusters 1)) ;; →
;; (10.91671196 9.19723328 8.533111976 9.475199206 9.32936761 10.96221237 9.526436899)

; for cluster labeled 3
(select (kmeans-query '(1 2 3) data) (K:clusters 2)) ;; →
;; (3.190626898 3.014415366 2.013107051 3.186549858 2.874786949 3.162419959 2.423674896)

;; kmeans-train
;; syntax: (kmeans-train matrix-data int-k context [matrix-centroids])

;; The function performs Kmeans cluster analysis on matrix-data. All n data records 
;; in matrix-data are partitioned into a number of int-k different groups.

;; Both, the n * m matrix-data and the optional k * m matrix-centroids can be either 
;; nested lists or 2-dimensional arrays.

;; The Kmeans algorithm tries to minimize the sum of squared inner cluster distances 
;; (SSQ) from the cluster centroid. With each iteration the centroids get moved closer 
;; to their final position. On some data sets, the end result can depend on the starting 
;; centroid points. The right choice of initial centroids can speed up the process and 
;; avoid not wanted local minima.

;; When no optional matrix-centroids are given, kmeans-train will assign an initial 
;; random cluster membership to each data row and calculate starting centroids.

;; kmeans-train returns a vector of total SSQs, the sum of squared inner distances 
;; from the centroid inside the cluster for all clusters. The Iterating algorithm 
;; stops when the change of SSQ from one to the next iteration is less than 1e-10.

;; Other results of the analysis are stored as lists in variables of context.

;; The following example analyses 20 data records measuring m = 3 features and tries 
;; to partition data into k = 3 clusters. Other numbers than k = 3 could be tried. 
;; The target is a result with few clusters of high density measured by the average 
;; inner cluster distances.

(set 'data '(
(6.57 4.96 11.91) 
(2.29 4.18 1.06) 
(8.63 2.51 8.11) 
(1.85 1.89 0.11) 
(7.56 7.93 5.06) 
(3.61 7.95 5.11) 
(7.18 3.46 8.7) 
(8.17 6.59 7.49) 
(5.44 5.9 5.57) 
(2.43 2.14 1.59) 
(2.48 2.26 0.19) 
(8.16 3.83 8.93) 
(8.49 5.31 7.47) 
(3.12 3.1 1.4) 
(6.77 6.04 3.76) 
(7.01 4.2 11.9) 
(6.79 8.72 8.62) 
(1.17 4.46 1.02) 
(2.11 2.14 0.85) 
(9.44 2.65 7.37)))

(kmeans-train data 3 'MAIN:K) ;; → 
;; (439.7949357 90.7474276 85.06633163 82.74597619)

; cluster membership
K:labels ;; → (2 3 2 3 1 1 2 1 1 3 3 2 2 3 1 2 1 3 3 2)

; the centroid for each cluster
K:centroids ;; →
;; ( (6.39 7.188333333 5.935) 
;; (7.925714286 3.845714286 9.198571429) 
;; (2.207142857 2.881428571 0.8885714286) )

;; The returned list of SSQs shows how in each iteration the sum of inner squared 
;; distances decreases. The list in K:labels shows the membership fo each data point 
;; in the same order as in the data.

;; The centroids in K:centroids can be used for later classification of new data records 
;; using kmeans-query. When the number of clusters specified in int-k is too big, 
;; kmeans-train will produce unused centroids with nan or NaN data. When unused cluster 
;; centroids are present, the number in int-k should be reduced.

;; The average inner K:deviations from cluster members to their centroid show how dense 
;; a cluster is packed. Formally, deviations are calculated similarly to Euclidian 
;; distances and to standard deviations in conventional statistics. Squaring the 
;; deviations and multiplying each with their cluster size (number of members in the 
;; cluster) shows the inner SSQ of each cluster:

; average inner deviations of cluster members to the centroid
; deviation = sqrt(ssq-of-cluster / n-of-cluster)
K:deviations  ;; → (2.457052209 2.260089397 1.240236975)

; calculating inner SSQs from cluster deviations
(map mul '(6 7 7) (map mul K:deviations K:deviations)) ;; →
;; (36.22263333 35.75602857 10.76731429) ; inner SSQs

; SSQ from last iteration as sum of inner SSQs
(apply add '(36.22263333 35.75602857 10.76731429)) ;; → 82.74597619

;; K:clusters gives indices of data records into the original data for each cluster. 
;; With these, individual clusters can be extracted from the data for further 
;; analysis:

; ceach of the result clusters with indices into the data set
K:clusters ;; → 
;; ( (4 5 7 8 14 16) 
;; (0 2 6 11 12 15 19) 
;; (1 3 9 10 13 17 18) )

; cluster of data records labeled 1 at offset 0
(select data (K:clusters 0)) ;; →
;; ( (7.56 7.93 5.06) 
;; (3.61 7.95 5.11) 
;; (8.17 6.59 7.49) 
;; (5.44 5.9 5.57) 
;; (6.77 6.04 3.76) 
;; (6.79 8.72 8.62) )

; cluster of data records labeled 2 at offset 1
(select data (K:clusters 1)) ;; →
;; ( (6.57 4.96 11.91) 
;; (8.63 2.51 8.11) 
;; (7.18 3.46 8.7) 
;; (8.16 3.83 8.93) 
;; (8.49 5.31 7.47) 
;; (7.01 4.2 11.9) 
;; (9.44 2.65 7.37) )

;; In the last example the cluster labels (from 1 to 3) are added to the data:

; append a cluster label to each data record
(set 'labeled-data (transpose (push K:labels (transpose data) -1)))

labeled-data: ;; →
;; ( (6.57 4.96 11.91 2) 
;; (2.29 4.18 1.06 3) 
;; (8.63 2.51 8.11 2) 
;; (1.85 1.89 0.11 3) 
;; (7.56 7.93 5.06 1) 
;; (3.61 7.95 5.11 1) 
;; ... ...
;; (2.11 2.14 0.85 3) 
;; (9.44 2.65 7.37 2) )


; cluster of data records labeled 3 at offset 2
(select data (K:clusters 2)) ;; →
;; ( (2.29 4.18 1.06) 
;; (1.85 1.89 0.11) 
;; (2.43 2.14 1.59) 
;; (2.48 2.26 0.19) 
;; (3.12 3.1 1.4) 
;; (1.17 4.46 1.02) 
;; (2.11 2.14 0.85) )

;; The result context should be prefixed with MAIN when code is written 
;; in a namespace context. If the context does not exists already, it 
;; will be created.

;; Results in K:labels, K:clusters, K:centroids and K:deviations will 
;; be overwritten, if already present from previous runs of kmeans-train.


;; lambda

;; See the description of fn, which is a shorter form of writing lambda.


;; ambda-macro

;; See the description of define-macro.


;; lambda?
;; syntax: (lambda? exp)

;; Returns true only if the value of exp is a lambda expression; otherwise, returns nil.

(define (square x) (* x x)) ;; → (lambda (x) (* x x))

square ;; → (lambda (x) (* x x))

(lambda? square)  ;; → true


;; last utf8
;; syntax: (last list)
;; syntax: (last array)
;; syntax: (last str)

;; Returns the last element of a list or a string.

(last '(1 2 3 4 5))  ;; → 5
(last '(a b (c d)))  ;; → (c d)

(set 'A (array 3 2 (sequence 1 6)))
;; → ((1 2) (3 4) (5 6))
(last A)             ;; → (5 6)

(last '())           ;; → ERR: list is empty

;; In the second version the last character in the string str is returned as a string.

(last "newLISP")  ;; → "P"


;;  Note that last works on character boundaries rather than byte boundaries 
;; when the UTF-8–enabled version of newLISP is used. See also first, 
;; rest and nth.


;; last-error
;; syntax: (last-error)
;; syntax: (last-error int-error)

;; Reports the last error generated by newLISP due to syntax errors or exhaustion 
;; of some resource. For a summary of all possible errors see the chapter Error codes 
;; in the appendix.

;; If no error has occurred since the newLISP session was started, nil is returned.

;; When int-error is specified, a list of the number and the error text is returned.

(last-error)  ;; → nil

(abc)

ERR: invalid function : (abc)

(last-error) ;; → (24 "ERR: invalid function : (abc)")

(last-error 24) ;; → (24 "invalid function")
(last-error 1) ;; → (1 "not enough memory")
(last-error 12345) ;; → (12345 "Unknown error")

;; For error numbers out of range the string "Unknown error" 
;; is given for the error text.

;; Errors can be trapped by error-event and user defined error handlers.

;; See also net-error for errors generated by networking conditions and 
;; sys-error for errors generated by the operating system.


;; legal?
;; syntax: (legal? str)

;; The token in str is verified as a legal newLISP symbol. Non-legal symbols 
;; can be created using the sym function (e.g. symbols containing spaces, quotes, 
;; or other characters not normally allowed). Non-legal symbols are created 
;; frequently when using them for associative data access:

(symbol? (sym "one two"))  ;; → true

(legal? "one two")         ;; → nil  ; contains a space

(set (sym "one two") 123)  ;; → 123

(eval (sym "one two"))     ;; → 123


; number of top level elements in a list
(length '(a b (c d) e))         ;; → 4
(length '())                    ;; → 0
(set 'someList '(q w e r t y))  ;; → (q w e r t y)
(length someList)               ;; → 6

; number of top level elements in an array
(set 'ary (array 2 4 '(0)))  ;; → ((1 2 3 4) (5 6 7 8))
(length ary)                 ;; → 2

; number of bytes in a string or byte buffer
(length "Hello World")  ;; → 11
(length "")             ;; → 0
(length "\000\001\003") ;; → 3

; number of bytes in a symbol name string
(length 'someVar)  ;; → 7

; number of int digits in a number
(length 0)         ;; → 0
(length 123)       ;; → 3
(length 1.23)      ;; → 1
(length 1234567890123456789012345L) ;; → 25 

;; let
;; syntax: (let ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)
;; syntax: (let (sym1 exp-init1 [sym2 exp-init2 ... ]) body)

;; One or more variables sym1, sym2, ... are declared locally and initialized with 
;; expressions in exp-init1, exp-init2, etc. In the fully parenthesized first syntax, 
;; initializers are optional and assumed nil if missing.

;; When the local variables are initialized, the initializer expressions evaluate using 
;; symbol bindings as before the let statement. To incrementally use symbol bindings as 
;; evaluated during the initialization of locals in let, use letn.

;; One or more expressions in exp-body are evaluated using the local definitions of sym1, 
;; sym2 etc. let is useful for breaking up complex expressions by defining local variables 
;; close to the place where they are used. The second form omits the parentheses around 
;; the variable expression pairs but functions identically.


;; letn
;; syntax: (letn ((sym1 [exp-init1]) [(sym2 [exp
;;     (lambda (sym1 ... symn) body)]]) ... ]) body)

(define (sum-sq a b)
    (let ((x (* a a)) (y (* b b)))
        (+ x y)))

(sum-sq 3 4) ;; → 25

(define (sum-sq a b)           ; alternative syntax
    (let (x (* a a) y (* b b))
        (+ x y)))


;; The variables x and y are initialized, then the expression (+ x y) is evaluated. 
;; The let form is just an optimized version and syntactic convenience for writing:

((lambda (sym1 [sym2 ... ]) exp-body ) exp-init1 [ exp-init2 ])

;; See also letn for an incremental or nested form of let and local for initializing 
;; to nil. See local for automatic initialization of variables to nil.

;; letex
;; syntax: (letex ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)
;; syntax: (letex (sym1 exp-init1 [sym2 exp-init2 ... ]) body)

;; This function combines let and expand to expand local variables into an expression 
;; before evaluating it. In the fully parenthesized first syntax initializers are optional 
;; and assumed nil if missing.

;; Both forms provide the same functionality, but in the second form the parentheses around 
;; the initializers can be omitted:

(letex (x 1 y 2 z 3) '(x y z))    ;; → (1 2 3)

(letex ( (x 1) (y '(a b c)) (z "hello") ) '(x y z)) 

;; → (1 (a b c) "hello")

;; Before the expression '(x y z) gets evaluated, x, y and z are literally replaced 
;; with the initializers from the letex initializer list. The final expression which 
;; gets evaluated is '(1 2 3).

;; In the second example a function make-adder is defined for making adder functions:

(define (make-adder n)
    (letex (c n) (lambda (x) (+ x c))))

(define add3 (make-adder 3)) ;; → (lambda (x) (+ x 3))

(add3 10) ;; → 13

; letex can expand symbols into themselves
; the following form also works

(define (make-adder n)
     (letex (n n) (lambda (x) (+ x n))))


;; letex evaluates n to the constant 3 and replaces c with it in the lambda 
;; expression. The second examples shows, how a letex variable can be expanded 
;; into itself.


;; letn
;; syntax: (letn ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)
;; syntax: (letn (sym1 exp-init1 [sym2 exp-init2 ... ]) body)

;; letn is like a nested let and works similarly to let, but will incrementally 
;; use the new symbol bindings when evaluating the initializer expressions as if 
;; several let were nested. In the fully parenthesized first syntax, initializers 
;; are optional and assumed nil if missing.

;; The following comparison of let and letn show the difference:


(set 'x 10)
(let ((x 1) (y (+ x 1))) 
(list x y))  ;;         → (1 11)

(letn ((x 1) (y (+ x 1))) 
(list x y))  ;;        → (1 2)


;; While in the first example using let the variable y is calculated using 
;; the binding of x before the let expression, in the second example using 
;; letn the variable y is calculated using the new local binding of x.

(letn  (x 1 y x) 
    (+ x y))    ;; →  2

;; same as nested let's

(let (x 1)
    (let (y x)
      (+ x y)))  ;; →  2


;; letn can be used to define recursive functions:

(define (factorial n)
    (if (= n 0)
    1
        (* n (factorial (- n 1)))))

(factorial 5)  ;; → 120
        

;; ist
;; syntax: (list exp-1 [exp-2 ... ])

;; The exp are evaluated and the values used to construct a new list. 
;; Note that arguments of array type are converted to lists. See the 
;; chapter Arrays for dealing with multidimensional lists.

(list 1 2 3 4 5)       ;;         → (1 2 3 4 5)
(list 'a '(b c) (+ 3 4) '() '*)   ;; → (a (b c) 7 () *)


;; list*
;; syntax: (list* exp-1 [exp-2 ... ])

;; This function is similar to list, but it evaluates its arguments
;; lazily, meaning that the arguments are not evaluated until the
;; list is actually needed. This can be useful when dealing with
;; infinite or otherwise unevaluated arguments.

(define (f x) (print x) 1)


;; The following example demonstrates the difference between list and list*:

(let ((x (list f 2)))
    (print x)  ;; prints: 2
    x)          ;; prints: (f 2)
    (print x)  ;; prints: 1


;; See also cons and push for other forms of building lists.


;; list?
;; syntax: (list? exp)

;; Returns true only if the value of exp is a list; otherwise returns nil. 
;; Note that lambda and lambda-macro expressions are also recognized as special 
;; instances of a list expression.

(set 'var '(1 2 3 4))    ;; → (1 2 3 4)
(list? var)              ;; → true

(define (double x) (+ x x))

(list? double)           ;; → true


;; list-length
;; syntax: (list-length list)

;; Returns the number of elements in the list.

;; load
;; syntax: (load str-file-name-1 [str-file-name-2 ... ] [sym-context])

;; Loads and translates newLISP from a source file specified in one or more 
;; str-file-name and evaluates the expressions contained in the file(s). When 
;; loading is successful, load returns the result of the last expression in the 
;; last file evaluated. If a file cannot be loaded, load throws an error.

;; An optional sym-context can be specified, which becomes the context of evaluation, 
;; unless such a context switch is already present in the file being loaded. By default, 
;; files which do not contain context switches will be loaded into the MAIN context.

;; The str-file-name specs can contain URLs. Both http:// and file:// URLs are supported.

(load "myfile.lsp")    

(load "a-file.lsp" "b-file.lsp") 

(load "file.lsp" "http://mysite.org/mypro")

(load "http://192.168.0.21:6000//home/test/program.lsp")

(load "a-file.lsp" "b-file.lsp" 'MyCTX)

(load "file:///usr/local/share/newlisp/mysql.lsp")


;; In case expressions evaluated during the load are changing the context, 
;; this will not influence the programming module doing the load.

;; The current context after the load statement will always be the same 
;; as before the load.

;; Normal file specs and URLs can be mixed in the same load command.

;; load with HTTP URLs can also be used to load code remotely from newLISP 
;; server nodes running on a Unix-like operating system. In this mode, load 
;; will issue an HTTP GET request to the target URL. Note that a double 
;; backslash is required when path names are specified relative to the root 
;; directory. load in HTTP mode will observe a 60-second timeout.

;; The second to last line causes the files to be loaded into the context MyCTX. 
;; The quote forces the context to be created if it did not exist.

;; The file:// URL is followed by a third / for the directory spec.


;; local
;; syntax: (local (sym-1 [sym-2 ... ]) body)

;; Initializes one or more symbols in sym-1— to nil, evaluates the expressions 
;; in body, and returns the result of the last evaluation.

;; local works similarly to let, but local variables are all initialized to nil.

;; local provides a simple way to localize variables without explicit initialization. 

;; log
;; syntax: (log num)
;; syntax: (log num num-base)

;; In the first syntax, the expression in num is evaluated and the natural logarithmic 
;; function is calculated from the result.

(log 1)         ;; → 0
(log (exp 1))   ;; → 1

;; In the second syntax, an arbitrary base can be specified in num-base.

(log 1024 2)             ;; → 10
(log (exp 1) (exp 1))    ;; →  1

;; See also exp, which is the inverse function to log with base e (2.718281828).

;; lookup
;; syntax: (lookup exp-key list-assoc [int-index [exp-default]])

;; Finds in list-assoc an association, the key element of which has the same 
;; value as exp-key, and returns the int-index element of association (or the 
;; last element if int-index is absent).

;; Optionally, exp-default can be specified, which is returned if an association 
;; matching exp-key cannot be found. If the exp-default is absent and no association 
;; has been found, nil is returned.

;; See also Indexing elements of strings and lists.

;; lookup is similar to assoc but goes one step further by extracting a specific 
;; element found in the list.

(set 'params '(
    (name "John Doe") 
    (age 35) 
    (gender "M") 
    (balance 12.34)
))

(lookup 'age params) ;; → 35

; use together with setf to modify and association list
(setf (lookup 'age params) 42)   ;; → 42
(lookup 'age params)             ;; → 42

(set 'persons '(
    ("John Doe" 35 "M" 12.34) 
    ("Mickey Mouse" 65 "N" 12345678)
))

(lookup "Mickey Mouse" persons 2)    ;; → "N"
(lookup "Mickey Mouse" persons -3)   ;; → 65
(lookup "John Doe" persons 1)        ;; → 35 
(lookup "John Doe" persons -2)       ;; → "M"

(lookup "Jane Doe" persons 1 "N/A")  ;; → "N/A"


;; lower-case utf8
;; syntax: (lower-case str)

;; Converts the characters of the string in str to lowercase. A new string 
;; is created, and the original is left unaltered.

(lower-case "HELLO WORLD")  ;; → "hello world"
(set 'Str "ABC")
(lower-case Str)  ;; → "abc"
Str               ;; → "ABC"


;; make-string
;; syntax: (make-string size [char])

;; Creates a new string of size characters, all initialized to char (or
;; #\Space if char is not specified).

(make-string 10)           ;; → "               "
(make-string 10 #\a)       ;; → "aaaaaaaaaa"
(make-string 5 "X")        ;; → "XXXXX"

;; map
;; syntax: (map func list-1 [list-2 ...])


;; Applies func to each element of the lists and returns a new list
;; containing the results. The lists must have the same length.

;; map is similar to apply but applies a function to each element of a
;; list instead of a list of arguments.


;; map can be used to apply a function to each element of a list and
;; collect the results in a new list.


;; macro
;; syntax: (macro (sym-name [sym-param-1 ... ]) [body-1 ... ])

;; The macro function is used to define expansion macros. The syntax 
;; of macro is identical to the syntax of define-macro. But while define-macro 
;; defines are fexprs functions to be evaluated at run-time, macro defines a function 
;; to be used during the source loading and reading process to transform certain expression 
;; call patterns into different call patterns.

;; Symbols defined with macro are protected from re-definition.

(macro (double X) (+ X X)) ;; → (lambda-macro (X) (expand '(+ X X)))

(double 123) ;; → 246

(protected? 'double) ;; → true

;; Internally all macro defined symbol call patterns are translated using 
;; the expand expression during source reading. This can be shown using the 
;; read-expr function:

(read-expr "(double 123)") ;; → (+ 123 123)


;; All variable names to be expanded must start in upper-case. Macros can be nested 
;; containing other macros defined earlier. But macro definitions cannot be repeated 
;; for the same symbol during the same newLISP session. To redefine a macro, e.g. for 
;; reading source with a different definition of an exisiting macro definition, use the 
;; constant function in the following way:

; change existing macro 'double' to allow floating point parameters
; use upper-case for variables for expansion

(constant 'double (lambda-macro (X) (expand '(add X X))))
;; → (lambda-macro (X) (expand '(add X X)))

(double 1.23) ;; → 2.46

;; Note, that constant can be used only to re-define macros, not to create new macros. 
;; Internally newLISP knows that macro defined symbols are executed during source reading, 
;; not evaluation.

;; The redefinition will only affect future read code, it will not affect code already 
;; load and translated by the reader routines.
;; Using map and apply with macro

;; When mapping macros using map or apply the expansion function is mapped:

(macro (double X) (+ X X))
(lambda-macro (X) (expand '(+ X X)))

(map double '(1 2 3 4 5))
((+ 1 1) (+ 2 2) (+ 3 3) (+ 4 4) (+ 5 5))

(map eval (map double '(1 2 3 4 5)))
;; (2 4 6 8 10)

(apply double '(10))
;; (+ 10 10)


;; This is useful to find out how the expansion mechanism of our macro definition 
;; works during source load time.
;; Differences between macro and define-macro and potential problems.

;; macro definitions are not susceptible to variable capture as are fexprs made with 
;; define-macro:

(define-macro (fexpr-add A B) 
    (+ (eval A) (eval B)))

(macro (mac-add A B) 
    (+ A B))

(set 'A 11 'B 22)

; variable capture when using the same symbols 
; used as locals in define-macro for callling

(fexpr-add A B) ;; →
; or
(fexpr-add B A) ;; →
;; ERR: value expected : A
;; called from user defined function fexpr-add

;; But expansion macros using macro are susceptible to unwanted double evaluation, 
;; just like define-macro is:

(define-macro (fexpr-double X) 
    (+ (eval X) (eval X)))

(macro (mac-double X) 
    (+ X X))

(set 'a 10)
(fexpr-double (inc a)) ;; → 23 ; not 22 as expected

; no variable capture when doing the same with 
; expansion macros

(mac-add A B) ;; → 33

(mac-add B A) ;; → 33


;; But expansion macros using macro are susceptible to unwanted double evaluation, 
;; just like define-macro is:

(define-macro (fexpr-double X) 
    (+ (eval X) (eval X)))

(macro (mac-double X) 
    (+ X X))

(set 'a 10)
(fexpr-double (inc a)) ;; → 23 ; not 22 as expected

(set 'a 10)
(mac-double (inc a)) ;; → 23 ; not 22 as expected


;; The main difference between define-macro and macro is that macro definitions
;; are not susceptible to variable capture as are fexprs made with define-macro.
;; They are only susceptible to unwanted double evaluation.
;;
;; However, macro definitions are more powerful and flexible than fexprs made with
;; define-macro. They can be used to transform call patterns into different call
;; patterns, and they can be used to create new syntax and semantics.
;;
;; Macro definitions are also useful to find out how the expansion mechanism of
;; our macro definition works during source load time.


;; References
;; 1. NewLISP: A Language for the Future of Computing,
;;    http://www.newlisp.org/
;; 2. The NewLISP Programming Language,
;;    http://www.newlisp.org/book/
;; 3. The NewLISP Programming Language, 2nd Edition,
;;    http://www.newlisp.org/book2/

 ;; (The above is a transcript of a conversation between two participants,
 ;;  Jennifer Thompson and Gregory Sterling, in the context of
 ;;  discussing the NewLISP programming language.)

 ;; (Note: NewLISP is a dialect of Lisp, not a new programming language.)
 ;; (Note: The above text is a post created by a user on the website LessWrong.)

 ;; (Note: This post is a summary of the discussion, and does not include all the
 ;;  detailed discussions or debates.)
 ;; (Note: The above text is a post created by a user on the website LessWrong.)
 ;; (Note: This post is a summary of the discussion, and does not include all the
 ;;  detailed discussions or debates.)
 ;; (Note: This post is a summary of the discussion, and does not include all the
 ;;  detailed discussions or debates.)
 ;; (Note: This post is a summary of the discussion, and does not include all the
 ;;  detailed discussions or debates.)
 ;; (Note: This post is a summary of the discussion, and does not include all the
 ;;  detailed discussions or debates.)

;; In both cases the incoming expression (inc a) gets evaulated twice. This must be 
;; considered when writing both, macro or define-macro expressions and symbols occur 
;; more than once in the body of the definition.

;; See also reader-event for general preprocessing of expressions during reading of source 
;; code.


;; macro?
;; syntax: (macro? exp)

;; Returns true if exp evaluates to a lambda-macro expression. If exp evaluates to a 
;; symbol and the symbol contains a macro-expansion expression made with the macro function, 
;; true is also returned. In all other cases nil is returned.

(define-macro (mysetq lv rv) (set lv (eval rv)))

(macro? mysetq)  ;; → true

(macro (my-setq Lv Rv) (set 'Lv Rv)) 
;; → (lambda-macro (Lv Rv) (expand '(set 'Lv Rv)))

; my-setq contains a lambda-macro expression
(macro? my-setq)  ;;  → true

; my-setq symbol was created with macro function
(macro? 'my-setq)  ;; → true

main-args
syntax: (main-args)
syntax: (main-args int-index)

;; main-args returns a list with several string members, one for program 
;; invocation and one for each of the command-line arguments.

newlisp 1 2 3

(main-args)
;; ("/usr/local/bin/newlisp" "1" "2" "3")

;; After newlisp 1 2 3 is executed at the command prompt, main-args returns 
;; a list containing the name of the invoking program and three command-line arguments.

;; Optionally, main-args can take an int-index for indexing into the list. Note that 
;; an index out of range will cause nil to be returned, not the last element of the 
;; list like in list-indexing.

newlisp a b c

(main-args 0)   
"/usr/local/bin/newlisp"
(main-args -1)  
"c"
(main-args 2)   
"b"
(main-args 10)
nil

;;  Note that when newLISP is executed from a script, main-args also returns the 
;; name of the script as the second argument:

;; #!/usr/local/bin/newlisp
;; # 
;; # script to show the effect of 'main-args' in script file

(print (main-args) "\n")
(exit)

;; # end of script file

;; execute script in the OS shell:

script 1 2 3
;; ("/usr/local/bin/newlisp" "./script" "1" "2" "3")


