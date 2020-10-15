#lang pl
;;205672538


#| Please complete the missing rules below ---------Q1  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL> } 2 groups
        |  { union <SOL> <SOL>  } 2 groups
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
    [Set  SET]
    [Smult Number SOL] ;;adition scalar mult, scalar-mult contain number and sol to multiply the number
    [Inter SOL SOL] ;; addition intersect, Inter contain 2 sol And in the end
    [Union SOL SOL] ;;addition unuin, Union contain 2 sol And in the end
    [Id    Symbol]
;;    [With  Symbol SOL SOL] -- not to be used, syntactic sugar for ...
    [Fun   Symbol Symbol SOL]
    [CallS SOL SOL SOL]
    [CallD SOL SOL SOL])

;; ----------------------------------------------------Q2
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

;; Takes a number and a list of numbers and
;; returns #t if The number is a value un the list and otherwise returns #f.
  (: ismember? : Number SET  -> Boolean)
  (define (ismember? n l)
    (cond [(null? l) #f]
          [(= n (first l)) #t]
          [else (ismember? n (rest l))]))

  (test (not (ismember? 1 '(3 4 5))))
  (test (not (ismember? 1 '( 3 2 3 5 6))))
  (test (ismember? 1 '(3 4 5 1 3 4)))
  (test (ismember? 1 '(1)))
;;MORE TESTS
(test (ismember? 0 '(0)))
(test (ismember? 2.5 '(2.5)))

#|
 Takes a list of numbers and returns The sub-list without repetition
 we arbitrarily take the rightmost instance of this number in the list using 'ismember?'
 for excample:

(test (remove-duplicates '(3 4 5)) => '(3 4 5))
(test (remove-duplicates '( 3 2 3 5 6)) => '(2 3 5 6))
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
|#
  (: remove-duplicates : SET  -> SET)
  (define (remove-duplicates l)
    (cond [(or (null? l) (null? (rest l))) l]
          [(ismember? (first l) (rest l)) (remove-duplicates (rest l))] ;; using the 'ismember?' function to check if the number exists in SET or does not
          [else (cons (first l) (remove-duplicates (rest l)))]))

(test (remove-duplicates '(3 4 5)) => '(3 4 5))
(test (remove-duplicates '( 3 2 3 5 6)) => '(2 3 5 6))
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))

;;MORE TESTS
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(0 0 0 0 0 0)) => '(0))
(test (remove-duplicates '(-1)) => '(-1))
(test (remove-duplicates '(2 3 4 5)) => '(2 3 4 5))

  ;;Takes a list of numbers and returns
  ;;the partial list without repetition - sorted by <
  (: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (remove-duplicates (sort l <))) ;; here using 'remove-duplicates' function to delete duplicate after they sorted

;;MORE TESTS
(test (create-sorted-set '(1)) => '(1))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '(-1)) => '(-1))
(test (create-sorted-set '(1 -8 2 3)) => '(-8 1 2 3))
(test (create-sorted-set '(0 0 0 0 0 0)) => '(0))

;;Takes two lists of numbers and returns
;;List all numbers belonging to at least one of the two - sorted and without repetition.
;;Accepts a 2 SET, Unite the SET ,sorted and delete duplicate
  (: set-union : SET SET -> SET)
  (define (set-union A B)
   (create-sorted-set (append A B))) ;; here using 'create-sorted-set' function to delete duplicates

;;MORE TESTS
(test (set-union '(1) '(1)) => '(1))
(test (set-union '() '()) => '())
(test (set-union '(-1) '(-1)) => '(-1))
(test (set-union '(2 3) '(-3 2)) => '(-3 2 3))

;;Takes two lists of numbers And returns the list of all belonging to each of the two -
;;sorted and without repetition.
;;Without assuming that the input is sorted series and no repetitions.
;;Accepts a 2 SET, intersection the SET, sorted and delete duplicate
  (: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n) ;;here using 'mem-filter' function that filters the members 
    (ismember? n A))
  (create-sorted-set (filter mem-filter B))) ;;here using 'create-sorted-set' to sort delete duplicates

;;MORE TESTS
(test (set-intersection '(1) '(1)) => '(1))
(test (set-intersection '() '()) => '())
(test (set-intersection '(-1) '(-1)) => '(-1))
(test (set-intersection '(2 -7 7) '(-7 2 7)) => '(-7 2 7))

;; ---------------------------------------------------------Q3
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
  (: parse-sexpr : Sexpr -> SOL)
  ;; to convert s-expressions into SOLs
  ;; here using 'create-sorted-set' function to sort and delete duplicate members
  (define (parse-sexpr sexpr)
    (match sexpr
      [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
         (CallS (Fun name name (parse-sexpr body)) (parse-sexpr named) (parse-sexpr named))] ;;; there is no With constructor replace with existing constructors
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name1) (symbol: name2)) body)
          (if (eq? name1 name2)
              ;;here I put error because I don't want it to have the same name more then one time
              (error'parse-sexpr "`fun' has a duplicate parameter name in ~s" sexpr) ;; cannot use the same param name twice
              (Fun name1 name2 (parse-sexpr body)))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
      [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
      ;;the order is: function ,SOL, Sol call-static and call-dynamic in the same constructor
      [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
      [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


    


  (: parse : String -> SOL)
  ;; parses a string containing a SOL expression to a SOL AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
              {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters

;;MORE TESTS
(test (parse "{with {x {}} {fun {x y} {1}}}") =>
      (CallS (Fun 'x 'x (Fun 'x 'y (Set '(1)))) (Set '()) (Set '())))
(test (parse "{with {x {-1 2 3}} {fun {x y} {union x y}}}") 
      =>
     (CallS (Fun 'x 'x (Fun 'x 'y (Union (Id 'x) (Id 'y)))) (Set '(-1 2 3)) (Set '(-1 2 3))))
(test (parse "{with {x {}} {fun {x y} {}}}") =>
      (CallS (Fun 'x 'x (Fun 'x 'y (Set '()))) (Set '()) (Set '())))
      

;;-----------------------------------------------------Q4
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl }, env)  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E},env) =   (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,env) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2},env) = (sort (create-set (set-intersection (eval(E1,env) , eval(E2,env))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2},env) = (sort (create-set (eval(E1,env) , eval(E2,env))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x1 x2} E},env)  = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env),extend(x1, eval(E1, env), envf))
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise
    eval({call-dynamic E-op E1 E2},env)
             = eval(Ef, extend(x2, eval(E2, env), extend(x1, eval(E1, env), env))
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise

|#

;; Types for environments, values, and a lookup function

  (define-type ENV
    [EmptyEnv]
    [Extend Symbol VAL ENV])

  (define-type VAL
    [SetV SET]
    [FunV Symbol Symbol SOL ENV])

  (: lookup : Symbol ENV -> VAL)
  (define (lookup name env)
    (cases env
      [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
      [(Extend id val rest-env)
       (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 
;; Accepts VAL and convert that to SET
  (: SetV->set : VAL -> SET)
    (define (SetV->set v)
      (cases v
        [(SetV S) S]
        [else (error 'SetV->set "expects a set, got: ~s" v)]))

;;MORE TESTS
(test (SetV->set (SetV '(7 8 9))) => '(7 8 9))
(test (parse "{7 8 9}") => (Set '(7 8 9)))
(test (SetV '(7 8 9)) => (SetV '(7 8 9)))
(test (SetV->set (SetV '())) => '())

;;smult-set accepts Number VAL and calculator scalar mult
  (: smult-set : Number VAL -> VAL)
  (define (smult-set n s)
    (: mult-op : Number -> Number)
    (define (mult-op k)
      (* k n))

;; here using 'map' function from racket to calculte scalar after that sorted and delete duplicted
;; at first I convert SetV to s to be able to use map and in the end I covcert back to SetV to return the VAL

    (SetV (map mult-op (SetV->set s))))

;;MORE TESTS
(test (smult-set 2 (SetV '(1 2 3))) => (SetV '(2 4 6)))
(test (smult-set 1 (SetV '(1 2 3))) => (SetV '(1 2 3)))
(test (smult-set 3 (SetV '(1 2 3))) => (SetV '(3 6 9)))

(: set-op : (SET SET -> SET) VAL VAL -> VAL);;The function get SOL VAL VAL and calculator the operator
  ;; gets a binary SET operator, and uses it within a SetV
  ;; wrapper
  (define (set-op op val1 val2)
     (SetV (op (SetV->set val1) (SetV->set val2))))

;;MORE TESTS
(test (set-op set-union (SetV '(4 5 6)) (SetV '(4 5 6))) => (SetV '(4 5 6)))
(test (set-op set-intersection (SetV '(4 5 6)) (SetV '(4 5 6))) => (SetV '(4 5 6)))
(test (set-op set-intersection (SetV '(1 1 1 1 )) (SetV '(1 1 1 0))) => (SetV '(1)))
(test (set-op set-union (SetV '()) (SetV '(4 5 6))) => (SetV '(4 5 6)))

;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL ENV -> VAL)
  ;; evaluates SOL expressions by reducing them to set values
  (define (eval expr env)
    (cases expr
      [(Set S) (SetV S)] ;;converts S to SetV and return S
       [(Smult n set) (smult-set n (eval set env))] ;;smult-set with number and VAL ,return multply scalar
      [(Inter l r) (set-op set-intersection (eval l env) (eval r env))] ;; set-op with function set-intersection or set-union with left and right
      [(Union l r)(set-op set-union (eval l env) (eval r env))] ;; set-op with function set-intersection or set-union with left and right
      [(Id name) (lookup name env)]
      [(Fun bound-id1 bound-id2 bound-body)
       (FunV bound-id1 bound-id2 bound-body env)]
      [(CallS fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env);;static model-always looking at the environment is associated with f-env
                     (eval bound-body (Extend bound-id1 (eval arg-expr1 env) 
            (Extend bound-id2 (eval arg-expr2 env) f-env))) ]
           [else (error 'eval "`call-static' expects a function, got: ~s"
                              fval)]))]
      [(CallD fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
            [(FunV bound-id1 bound-id2 bound-body env)
            (eval bound-body (Extend bound-id1 (eval arg-expr1 env);;dynamic model-general environment and not looking at the environment is associated
            (Extend bound-id2 (eval arg-expr2 env) env))) ]
           [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                              fval)]))]))
;;---Q5
 ;; (: createGlobalEnv : -> ENV) ;;global envirment?? I had a hard time to answer this question. :(
;;  (define (createGlobalEnv)
  ;;  (Extend 'second <-- fill in -->
        ;;    (Extend <-- fill in -->
      ;;              (Extend <-- fill in --> 
               ;;                     (EmptyEnv)))))

  (: run : String -> (U SET VAL))
  ;; evaluate a SOL program contained in a string
  (define (run str)
    (let ([result (eval (parse str) (EmptyEnv))])
       (cases result
         [(SetV S) S]
         [else (error 'run "evaluation returned a non-number: ~s" result)])))


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")

(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
;;MORE TESTS
(test (run "{with {x {union {1} {5 6 7 8 9 10}}}
                 {call-dynamic {fun {x y} {intersect x y}} {scalar-mult 3 x} {4 8 9}}}")=> '())
(test (run "{union {0 0 0} {-1 2 3}}") => '(-1 0 2 3))
(test (run "{intersect {1} {5 6 7 8 9 10}}") => '())

#|
Q6:
1- I used lecture summaries and lectures and exercises for question 5 I watched a video of lesson 11 and more 
2- The type SET of number and functions
3- I've duplicated parameters to get over it
4- ismember? - is tail recursive. The advantage of using tail recursion is not to get flooded (stack over flow)
5- even thow that i did not answer this question i think the smart way to solve this is to use static model
6- we will get error
|#


