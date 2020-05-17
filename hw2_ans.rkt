#lang pl
;The ROL BNF and Parsing code:
;Written by Ortal Hanoch

;;Q1: --------BNF for ROL--------

;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

#| BNF for the ROL language:

<ROL> ::= {reg-len = <num> <RegE> }
<RegE> ::= <Bits>       
	   | {and <RegE> <RegE>}        
	   | {or <RegE> <RegE>}        
	   | {shl <RegE>}        
<Bits> ::= <bit> | <bit> <Bits>
<bit>::= 1 | 0

For example, some valid expressions in this language are:
"{ reg-len = 4 {1 0 0 0}}" "{ reg-len = 4 {shl {1 0 0 0}}}"
"{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}"
"{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}"
"{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}"
"{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}" ;; BNF allows wrong length

However, the following are invalid expressions:
"{1 0 0 0}"
"{ reg-len = 3 {and {2 2 1} {0 1 1}}}"
"{ reg-len = 3 {+ {1 1 1} {0 1 1}}}"
"{ reg-len = 4 {shl {1 1 1 1} {0 1 1 1}}}"
"{ reg-len = 0 {}}"

|#

;; RegE abstract syntax trees
(define-type RegE
  [Reg Bit-List]
  [And RegE RegE]
  [Or RegE RegE]
  [Shl RegE])

;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))]))

(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
  (match sexpr
    [(list 'reg-len = (number: n) args)
     (if (> n 0) ;; remember to make sure specified register length is at least 1
         (parse-sexpr-RegL args n)
         (error 'parse-sexpr "Register length must be at least 1 ~s" sexpr) )]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    [(list (and a (or 1 0)) ... ) (if (= reg-len (length a))
                                      (Reg (list->bit-list a))
                                      (error 'parse-sexpr-RegE "wrong number of bits in ~s" a))] ;; from the slides (changing the given code).
    [(list 'and list1 list2) (And (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'or list1 list2) (Or (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'shl list) (Shl (parse-sexpr-RegL list reg-len))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))
  
;;TESTS:
(test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
(test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
(test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
(test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
(test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
(test (parse "{ reg-len = 4 {or {1 1 a 1} {0 1 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 3 {and {2 2 1} {0 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 3 {+ {1 1 1} {0 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 3 {1 2 1}}") =error> "bad syntax in")
(test (parse "{ reg-len = 4 {shl {1 1 1 1} {0 1 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 0 }") =error> "bad syntax in")
(test (parse "{ reg-len = 1  { and {0} {1}}}") => (And (Reg '(0)) (Reg '(1))))
(test (parse "{ reg-len = 5 { or {and {shl {1 0 1 1 1}} {1 1 1 1 0}} {1 1 1 1 0}}}") => (Or (And (Shl(Reg '(1 0 1 1 1))) (Reg '(1 1 1 1 0))) (Reg '(1 1 1 1 0))))
(test (parse "{ reg-len = 9 {shl {1 0 0 1 1 1 1 1 0}}}") => (Shl (Reg '(1 0 0 1 1 1 1 1 0))))
(test (parse "{ reg-len = 1  {0}}") => (Reg '(0)))



#|
Q2: --------Accommodating Memory Operations--------

Given a MAE as :
<MAE> ::= <num> 
        | { + <MAE> <MAE> } 
        | { - <MAE> <MAE> } 
        | { * <MAE> <MAE> } 
        | { / <MAE> <MAE> } 
        | { set <MAE> } 
        | get
In this case, there is only one set, which limits the grammer a lot,
but sufficiently solves the problem.

One of the problem that this approach encouters is the ambiguity of evaluating these complicated expressions
(e.g. {* {+ {set 1} {set 2}} get} is evaluable to different values like 2 or 8,
depending on which "set"'s value is evaluated and memorized last)
To solve the problem of ambiguity is to force the grammar to have Setters, as a set&get relation:

<MAE> ::= {seq <AE>}            
         |{seq <SETERS><GAE>}  

<AE> ::= <num>          
       |{+ <AE> <AE>}    
       |{- <AE> <AE>}    
       |{* <AE> <AE>}    
       |{/ <AE> <AE>}    

<SETERS> ::= {set <GAE>}           
            |{set <GAE>}<SETERS>   

<GAE> ::= <num>               
       |get                    
       |{+ <GAE> <GAE>}        
       |{- <GAE> <GAE>}        
       |{* <GAE> <GAE>}        
       |{/ <GAE> <GAE>}        



In this case, there is only one set, which limits the grammer a lot,
but sufficiently solves the above problem.
Solving with sequence of sets.
The Objective of this approach is :preventing the first MAE from having get,
and the last MAE from having set.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ID number is 205672538,

{seq {set {+ 53 725}}
{set {* get get}}
{/ get 05}}

<MAE> -> {seq <SETERS><GAE>}{seq <SETERS><GAE>}->{set <GAE>}<SETERS> {seq {set <GAE>}<SETERS><GAE>}->{set <GAE>}{seq {set <GAE>}{set <GAE>}<GAE>}
->{+ <GAE> <GAE>}{* <GAE> <GAE>}{/ <GAE> <GAE>}{set {+ <GAE> <GAE>}}{set {* <GAE> <GAE>}}{/ <GAE> <GAE>}}
-><num>get{set {+ <num> <num>}}{set {* get get}}{/ get <num>}}
->{set {+ 53 725}}{set {* get get}}{/ get 05}}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{seq { set { + 205 672} } { set { + get 538} } get}

<MAE> -> {seq <SETERS><GAE>}{seq <SETERS><GAE>} ->(10) {seq {set <GAE>}<SETERS><GAE>}->{set <GAE>} {seq {set <GAE>}{set <GAE>}<GAE>}
->{+ <GAE> <GAE>} {seq {set {+ <GAE> <GAE>} }{set {+ <GAE> <GAE>} }<GAE>}
-><num>get {seq {set {+ <num> <num>} }{set {+ get <num>} }get}
->{seq {set {+ 205 672} }{set {+ get 538} } get}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{seq {- 53 538}}
<MAE> ->{seq <AE>}{seq <AE>}->{- <AE> <AE>}{seq {- <AE> <AE>}}-><num>{seq {- <num> <num>}}

|#



#|
Q3: --------Higher Order Functions--------

Use foldl together with (or without) map to define a sum-of-squares function
which takes a list of numbers as input, and produces a number which is the sum
of the squares of all of the numbers in the list.
Thia solution is a one-liner.
And I used the square calculation too.

Got some ideas and help from this website:
https://docs.racket-lang.org/reference/pairs.html?q=map#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29

|#

;;this function includes the squares calculation '(foldl (lambda....',
;;I worte that shorter then I worte in my first try.
(: sum-of-squares : (Listof Natural) -> Natural)
(define (sum-of-squares lst)
  (foldl (lambda ([a : Natural] [b : Natural]) (+ (* a a) b)) 0 lst))

;;TESTS:
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(2)) => 4)
(test (sum-of-squares '(1 2)) => 5)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 2 3 4)) => 30)
(test (sum-of-squares '(1 2 3 4 5)) => 55)
(test (sum-of-squares '(1 2 3 4 5 6)) => 91)
(test (sum-of-squares '(1 2 3 4 5 6 7)) => 140)
(test (sum-of-squares '(1 2 3 4 5 6 7 8)) => 204)
(test (sum-of-squares '(1 2 3 4 5 6 7 8 9)) => 285)
(test (sum-of-squares '(1 2 3 4 5 6 7 8 9 10)) => 385)


#|
Q4: --------Typed Racket (and more H.O. functions)--------

Define a binary tree as a something that is either a Leaf holding a number,
or a Node that contains a binary tree on the left and one on the right.

define-type definition for BINTREE, with two variants called Node and Leaf.
Using this BINTREE definition,
The function called tree-map that takes in a numeric function f and a binary tree,
and returns a tree with the same shape but using f(n) for values in its leaves.
For example (test):

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))

define the BINTREE type, and the following procedures tree-map, tree-fold, and tree-reverse and tests.

Got some help from webs:
https://docs.racket-lang.org/reference/pairs.html?q=map&q=exp&q=foldl#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldl%29%29
https://docs.racket-lang.org/reference/pairs.html?q=map&q=exp&q=foldl#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29
|#

(define-type BINTREE
  [Node BINTREE BINTREE]
  [Leaf Number]) ;define-type definition for BINTREE, with two variants called Node and Leaf.

 ;; tree-map : (num -> num) BINTREE -> BINTREE
 ;; Maps the given function recursively over the given tree, returning a
 ;; tree of the results with the same shape.
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
 (define (tree-map f bin_tree) ;tree-map that takes in a numeric function f and a binary tree
   (cases bin_tree
       [(Leaf num) (Leaf (f num))]
       [(Node t_left t_right) (Node (tree-map f t_left) (tree-map f t_right))]))

;;Example: (equal? (reverse (tree-flatten t)) (tree-flatten (tree-reverse t)))
(: tree-fold : (All(A) (A A -> A) (Number -> A) BINTREE -> A)) ;tree-fold is a polymorphic function, so its type should be parameterized over “some input type A”.
(define (tree-fold f_num f_bin_tree bin_tree)
  (cases bin_tree
     [(Leaf num) (f_bin_tree num) ]
     [(Node t_left t_right) (f_num (tree-fold f_num f_bin_tree t_left)(tree-fold f_num f_bin_tree t_right))]))


(: tree-flatten : BINTREE -> (Listof Number))
 (define (tree-flatten tree)
  (tree-fold (inst append Number) (inst list Number)tree))

;Using tree-fold to define a tree-reverse and switch-nodes functions that consumes a tree and returns a tree that is its mirror image.
(: tree-reverse : BINTREE -> BINTREE)
  (define (tree-reverse bin_tree)
   (: switch-nodes : BINTREE BINTREE -> BINTREE)
     (define (switch-nodes n_left n_right)
       (Node n_right n_left))(tree-fold switch-nodes Leaf bin_tree))

;;TESTS:
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (Node (Leaf 1) (Leaf 2)) => (Node (Leaf 1) (Leaf 2)))
(test (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))) => (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))=> (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 (Node (Leaf 5) (Node (Leaf 6)(Leaf 7))))=> (Node (Leaf 6) (Node (Leaf 7) (Leaf 8))))
(test (tree-map add1 (Node (Leaf 7) (Node (Leaf 8)(Leaf 9))))=> (Node (Leaf 8) (Node (Leaf 9) (Leaf 10))))
(test (tree-map sub1 (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))=> (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))
(test (tree-map sub1 (Node (Leaf 5) (Node (Leaf 6)(Leaf 7))))=> (Node (Leaf 4) (Node (Leaf 5) (Leaf 6))))
(test (tree-map sub1 (Node (Leaf 7) (Node (Leaf 8)(Leaf 9))))=> (Node (Leaf 6) (Node (Leaf 7) (Leaf 8))))
(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))=> '(1 2 3) )
(test (tree-flatten (Node (Node (Node (Leaf 3) (Leaf 4))(Node (Leaf 5) (Leaf 6))) (Node (Leaf 7) (Leaf 8))))=> '(3 4 5 6 7 8 ) )
(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))(tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))))=> #t )
(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))(tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2)(Leaf 333))))))=> #f )
(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))(tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 221)(Leaf 3))))))=> #f )
(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))(tree-flatten (tree-reverse (Node (Leaf 101) (Node (Leaf 2)(Leaf 333))))))=> #f )
(test (tree-reverse (Node (Leaf 2) (Leaf 3)))  =>  (Node (Leaf 3) (Leaf 2)))
(test (tree-reverse (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))  => (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)))
