#lang pl

#|
Written by Ortal Hanoch

To learn more and get help from I used this links
https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-suffix~3f%29%29
https://php.developreference.com/article/24135453/calculate+polynomial+function+%5Bscheme+racket%5D
websites.

Q1:Suffix
plSuffixContained

This function get a list of string and return "true" 
**ONLY IF**
there is the suffix "pl" in the list, otherwise return "false".
 
Example 1:
> (string-suffix? "Racket" "et")
#t

Example 2:
(string-suffix? s suffix) → boolean?

 s : string?
 suffix : string?

suffix checks whether s includes at any location,
here we cheks what ends with the second argument, respectively.
|#

(: plSuffixContained : (Listof String) -> (U String #f))

(define(plSuffixContained listsuffix)
  (cond
    [(null? listsuffix) #f] ;;if the list is empty - return false.
    [(<= (string-length (first listsuffix)) 1) (plSuffixContained(rest listsuffix))] ;if the word <= 1 go to next num.
    [(equal? (substring (first listsuffix) (- (string-length (first listsuffix)) 2)) "pl") (first listsuffix)] ;checks for good word.
    [else (plSuffixContained(rest listsuffix))]))

;;TESTS:
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => false)
(test (plSuffixContained '("yyyt" "TplT" "plTT" "PlPl" "plplpl")) => "plplpl")
(test (plSuffixContained '("plplplplpl")) => "plplplplpl")
(test (plSuffixContained '()) => false)
(test (plSuffixContained '("a" "b" "c" "101010101")) => #f)
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTTllllpl" "lol")) => "lpTTllllpl")
(test (plSuffixContained '("pllp" "plyy" "ppppppppl" "lpTT" "lol")) => "ppppppppl")
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "plopl")) => "plopl")
(test (plSuffixContained '("ypy" "lll" "ttttyyylp" "ppppppl")) => "ppppppl")
(test (plSuffixContained '("yyyt" "Tpl" "plTT" "PlPl" "pLpLpLpL")) => "Tpl")
(test (plSuffixContained '("pLpLpL" "PLPL" "pl" "PlPl" "llllppp")) => "pl")

#|
Q2: Polyomyals
The thought behind the code is to take care of all the logical signs
and write the polynomial so that for each variable there is an appropriate strong and an appropriate logical sign.
Reference to end-cases where there is no coefficient or negative sign.
First write the polynomial appropriately
and then make sure to place the variable in 'x' and then to do the  mathematical calculation appropriately.

2.1:write-poly
This function gets a list of numbers and return a valid polynomial expression.
function that consumes a list of coefficients (numbers) a1,a2,...,an
and returns the polynomial (in a reversed order of coefficients) "a1xn+a2xn-1+⋯+an".

Example 1: '(3 2 6) -> "3x^2+2x+6"
|#
(: write-poly : (Listof Number) -> String)
(define (write-poly list)
  ;Tail recursive:
(: help-function : (Listof Number) String -> String)
  (define ( help-function list templist)
  (cond
    [(null? list) templist] ;if the list is empty - stop and return ans.
    [(and (= (first list) 0) (string=? "" templist) (= (length list) 1))(help-function (rest list) (string-append templist "0"))]
    ;if the first num is 0 and lis len is 1 send the rest to help function and add 0 to ans.
    [(= (first list) 0) (help-function (rest list) (string-append templist ""))]
    ;else - first num is 0 send the rest to help function and add none.
    
[(string=? "" templist )
  (cond
     [(= (length list) 2)(help-function (rest list) (string-append templist (number->string (first list)) "x"))]
     [(= (length list) 1)(help-function (rest list) (string-append templist (number->string (first list))))]

     [else
     (help-function (rest list) (string-append templist (number->string (first list)) "x^" (number->string (- (length list) 1))))])]

;if the number is positive:
[(> (first list) 0)
 (cond
    [(= (length list) 2)(help-function (rest list) (string-append templist "+" (number->string (first list)) "x"))] ;if the number is second from end.
    [(= (length list) 1)(help-function (rest list) (string-append templist "+" (number->string (first list))))] ;if the number is the last one.
    [else (help-function (rest list) (string-append templist "+" (number->string (first list)) "x^" (number->string (- (length list) 1))))])]

;if the number is negative:
[else
  (cond
    [(= (length list) 2)(help-function (rest list) (string-append templist (number->string (first list)) "x"))] ;if the number is second from end.
    [(= (length list) 1)(help-function (rest list) (string-append templist (number->string (first list))))] ;if the number is the last one.
    [else (help-function (rest list) (string-append templist (number->string (first list)) "x^" (number->string (- (length list) 1))))])]))
    (help-function list ""))

;;TESTS:
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(3 2)) => "3x+2")
(test (write-poly '(3)) => "3")
(test (write-poly '(4 7 11 6)) => "4x^3+7x^2+11x+6")
(test (write-poly '(-8 40 0)) => "-8x^2+40x")
(test (write-poly '(1 1 1 1 1)) => "1x^4+1x^3+1x^2+1x+1")
(test (write-poly '(0 1 2)) => "1x+2")
(test (write-poly '(10 20 30)) => "10x^2+20x+30")
(test (write-poly '(-60 -50 -40)) => "-60x^2-50x-40")
(test (write-poly '(-1 2 -3 4)) =>"-1x^3+2x^2-3x+4")
(test (write-poly '(0 0 0 16)) => "16")
(test (write-poly '(0 0 0 0)) => "0")
(test (write-poly '(9 0 0 0 0 0 0 0 0 0)) =>"9x^9")

#|
2.2:compute-poly
This function consumes a number x and a list of coefficients (numbers)
a1, a2, ... , an
and returns the result of the polynomial
again, in a reversed order of coefficients)
a1xn + a2xn-1 +⋯ + an
applied to the first argument x.

The function calculate a polynomial by placing the value of x.
|#

( : compute-poly : Number (Listof Number) -> Number)
( define (compute-poly x mlist)

   ( : help-function : Number Number (Listof Number) -> Number)
   ( define (help-function res x mlist)
      (if (null? mlist)
          res
          (help-function (+ res (* (first mlist) (expt x (- (length mlist) 1)))) x (rest mlist))))                           
   (help-function 0 x mlist))

;;TESTS:
(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
;;MORE TESTS:
(test (compute-poly 3 '(1 3 8 4 0 0)) => 738)
(test (compute-poly 2 '(7 1 5 0 0)) => 140)
(test (compute-poly 2 '(-7 -1 -5 0 0)) => -140)
(test (compute-poly 2 '(-1 7 3 0)) => 26)
(test (compute-poly 3/2 '(10 -1 -9 8)) => 26)
(test (compute-poly 1/2 '(16 2 4 0)) => 9/2)

#|
Q3:
3.1 Implement the empty stack EmptyKS + 3.2 Implement the PUSH operation Push
|#
(define-type st
  [EmptyKS]
  [Push Symbol String st])

#|
3.3 The implementation of the SEARCH operation search-stack.

The search operation takes as input a symbol (key) and a keyed-stack
and return the first (LIFO, last in first out) value that is keyed accordingly
|#

(: search-stack : Symbol st -> (U String #f))
(define (search-stack symbol sts)
  (cases sts
    ;; starts with [EmptyKS #f], can be also in the end (Different form of writing)
    [(EmptyKS) #f]
    [(Push mysymbol msy msts)
      (cond
        [(eq? symbol mysymbol)msy]
        [else
         (search-stack symbol msts)])]))

#|
3.4 The implementation of the POP operation pop-stack.

The pop operation should take as input a keyed-stack
and return the keyed-stack without its first (keyed) value.
If the original stack was empty, it should return a #f value.
|#

(: pop-stack : st -> (U st #f))
(define (pop-stack sts)
  (cases sts
    ;; ends with [EmptyKS #f], can be also in the begining (Different form of writing)
    [(Push mysymbol msy msts) msts] [EmptyKS #f]))

;;TESTS:
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>
(Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))
=> (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a
"A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a
"A" (EmptyKS))))) => #f)
(test (search-stack 'a (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A"
(EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

;;MORE TESTS:
(test (EmptyKS) => (EmptyKS))
(test (pop-stack (EmptyKS)) => #f)
(test (Push 'ONE "1" (EmptyKS)) => (Push 'ONE "1" (EmptyKS)))
(test (Push 'TWO "2" (EmptyKS)) => (Push 'TWO "2" (EmptyKS)))
(test (Push 'THREE "3" (EmptyKS)) => (Push 'THREE "3" (EmptyKS)))
(test (Push 'I "I" (Push 'LOVE "LOVE" (Push 'THIS "THIS" (EmptyKS)))) => (Push 'I "I" (Push 'LOVE "LOVE" (Push 'THIS "THIS" (EmptyKS)))))
(test (search-stack 'Hello (Push 'World "World" (Push 'Hello "Hello" (Push 'Test "Test" (EmptyKS))))) => "Hello")
(test (pop-stack (Push 'ONE "1" (Push 'TWO "2" (EmptyKS)))) => (Push 'TWO "2" (EmptyKS)))
(test (pop-stack (Push 'I "I" (Push 'LOVE "LOVE" (EmptyKS)))) => (Push 'LOVE "LOVE" (EmptyKS)))


#|
Q4 The following is an explanation of the question:

"In this question you are given full code together with tests for the presented functions.
All you are required to do is to add the appropriate comments for each of the functions.
These comments should describe what the function takes as input, what it outputs,
what its purpose is, and how it operates.
Do not forget to also add your personal remarks on the process in which you personally
came to realize the above. You should copy the following code into your .rkt file, and add the comment therein."

For example:
In "is-odd", the 0 is false.
In "is-even", the 0 is true.
So, if we call the number '6' in "is-odd" function, we will end up at 0 (we will reduce 1 more and more ...) and eventually return false.
And if we call number '6' in "is-even" function, we will end up at 0 (we will reduce 1 more and more ...) and at the end we will return true.
It's tricky and cool at the same time
|#

(: is-odd? : Natural -> Boolean)
;;Function signature.
;;Defined with the name "is-odd?", gets a natural number and returns Boolean value.
;;returns true ONLY if the number is odd, otherwise returns false.
(define (is-odd? x)
(if (zero? x)
false
(is-even? (- x 1))))
(: is-even? : Natural -> Boolean)
;;Function signature.
;;Defined as "is-even?", gets a natural number and returns Boolean value.
;;returns true ONLY if the number is even, otherwise returns false.
;;The function behaves and is written recursively using 'is-odd' function as well.
(define (is-even? x)
(if (zero? x)
true
(is-odd? (- x 1))))
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; See explanation about the All syntax at the end of the file...
;;Function signature.
;;Defined as "every?", Gets a list and function. The function and list can contain the same kind of values
;;The value obtained in the function is determined by the function and the list.
(define (every? pred lst)
(or (null? lst)
(and (pred (first lst))
(every? pred (rest lst)))))
;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;Function signature.
;;Defined as "all-even?", Gets a list of natural numbers and returns Boolean value.
;;returns true ONLY if all the numbers in the list are even, otherwise returns false.
(define (all-even? lst)
(every? is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
;;Function signature.
;;Defined as "every2?", Gets a list of lists and function for each of them.
;;and returns true if all the numbers in list A are even and all the numbers in list B are odd.

(define (every2? pred1 pred2 lst1 lst2)
(or (null? lst1) ;; both lists assumed to be of same length
(and (pred1 (first lst1))
(pred2 (first lst2))
(every2? pred1 pred2 (rest lst1) (rest lst2)))))

(: all-even-odd? : (Listof Natural) (Listof Natural) -> Boolean)
(define (all-even-odd? list1 list2)
    (every2? is-even? is-odd? list1 list2))

;;TESTS:
(test (all-even-odd? null null))
(test (all-even-odd? (list 10 20 30 40 50 60 70) (list 11 21 31 41 51 61 71)))
(test (all-even-odd? (list 2 4 6 8 10) (list 1 3 5 7 9)))
(test (not (all-even-odd? (list 2 3 4 5) (list 6 7 8 9))))
(test (not (all-even-odd? (list 14 16 2 7) (list 1 3 5 7))))

;;The end HW-1