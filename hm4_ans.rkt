#lang pl 04


  ;; The Flang interpreter

  #|
This assignment was a good repetition of the material we learned in the lessons, it took us two days to do it (not continuous).
In the assignment, we added the following as requested:
ture, false, if,then-do,else-do, <, >, =, not.


   The grammar:
 <FLANG> ::= <num> ;; Rule 1
 | { + <FLANG> <FLANG> } ;; Rule 2
 | { - <FLANG> <FLANG> } ;; Rule 3
 | { * <FLANG> <FLANG> } ;; Rule 4
 | { / <FLANG> <FLANG> } ;; Rule 5
 | { with { <id> <FLANG> } <FLANG> } ;; Rule 6
 | <id> ;; Rule 7
 | { fun { <id> } <FLANG> } ;; Rule 8
 | { call <FLANG> <FLANG> } ;; Rule 9

*we Add the rules True,False,=,>,<,not,if,then-do,else-do *


 | {True} ;; add rule for True ;; Rule 10
 | {False};; Rule 11
 | { =  <FLANG> <FLANG>}  ;; add rule for = ;; Rule 12
 | { >  <FLANG> <FLANG>} ;; Rule 13
 | { <  <FLANG> <FLANG>} ;; Rule 14
 | { not  <FLANG>} ;; Rule 15
 | {if <FLANG> then-do <FLANG> else-do <FLANG>};; add rule 16 for (the above) if
expressions

   Evaluation rules help us to function eval:

    subst:
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {call E1 E2}[v/x]     = {call E1[v/x] E2[v/x]}
      {fun {y} E}[v/x]      = {fun {y} E[v/x]}           ; if y =/= x
      {fun {x} E}[v/x]      = {fun {x} E}

    eval:
      eval(N)            = N
      eval({+ E1 E2})    = eval(E1) + eval(E2)  \ if both E1 and E2
      eval({- E1 E2})    = eval(E1) - eval(E2)   \ evaluate to numbers
      eval({* E1 E2})    = eval(E1) * eval(E2)   / otherwise error!
      eval({/ E1 E2})    = eval(E1) / eval(E2)  /
      eval(id)           = error!
      eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
      eval(FUN)          = FUN ; assuming FUN is a function expression
      eval({call E1 E2}) = eval(Ef[eval(E2)/x]) if eval(E1)={fun {x}Ef}
                         = error!    

  |#

  (define-type FLANG
    [Num  Number]
    [Add  FLANG FLANG]
    [Sub  FLANG FLANG]
    [Mul  FLANG FLANG]
    [Div  FLANG FLANG]
    [Id   Symbol]
    [With Symbol FLANG FLANG]
    [Fun  Symbol FLANG]
    [Call FLANG FLANG]
;*Add constructors to parser - for constructors that need 2 Arguments To perform their operation we have added 2 FLANG*
;And for those who only need one, we only added one FLANG.And for 3 arguments as well

    [Bool Boolean]
    [Bigger FLANG FLANG]
    [Smaller FLANG FLANG]
    [Equal FLANG FLANG]
    [Not FLANG]
    [If FLANG FLANG FLANG])

  (: parse-sexpr : Sexpr -> FLANG)
  ;; to convert s-expressions into FLANGs.We add the new Operators for them to be able to convert too
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)    (Num n)]
  ;;we Add true or false to paeser
      ['True (Bool true)]
      ['False (Bool false)]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name)) body)
          (Fun name (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
  ;;we Add = > < not and if to paeser
      [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'not exp) (Not (parse-sexpr exp))]
      [(cons 'if other) (match other
                         [(list condition (list 'then-do case1) (list 'else-do case2))
                         (If (parse-sexpr condition) (parse-sexpr case1) (parse-sexpr case2))]
                         [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]

      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 


  (: parse : String -> FLANG)
  ;; parses a string containing a FLANG expression to a FLANG AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

;; The following function is used in multiple places below,
 ;; hence, it is now a top-level definition
 (: Num->number : FLANG -> Number) 
 ;; gets a FLANG -- presumably a Num variant -- and returns the
 ;; unwrapped number
 (define (Num->number e)
 (cases e
 [(Num n) n]
 [else (error 'Num->number "expected a number, got: ~s" e)]))
 (: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
 ;; gets a Racket numeric binary operator, and uses it within a FLANG
 ;; `Num' wrapper
 (define (arith-op op expr1 expr2)
 (Num (op (Num->number expr1) (Num->number expr2))))
 (: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
 ;; gets a Racket Boolean binary operator (on numbers), and applies it
 ;; to two `Num' wrapped FLANGs
 (define (logic-op op expr1 expr2)
 ;;get operator flang and flang and return boolean if is = < >
 ;;
 (Bool (op (Num->number expr1) (Num->number expr2))))

 (: flang->bool : FLANG -> Boolean)
 ;; gets a Flang E (of any kind) and returns a its appropiate
 ;; Boolean value -- which is true if and only if E does not
 ;; represent false
 ;; Remark: the `flang->bool` function will also be top-level
 ;; since it's used in more than one place.
 (define (flang->bool e)
 (cases e
 ;;get flang boolean and return boolean
 [(Bool boolean)(if (eq? boolean #f) #f #t)]
 [else #t]))

  (: subst : FLANG Symbol FLANG -> FLANG)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to)
    (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Fun bound-id bound-body)
       (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
  ;we Add boolean = > < not and if to subst
    [(Bool bool) expr]
    [(Equal l r) (Equal (subst l from to) (subst r from to))]
    [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
    [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
    [(Not l) (Not (subst l from to))]
    [(If condition case1 case2) (If (subst condition from to) (subst case1 from to) (subst case2 from to))]))



  (: eval : FLANG -> FLANG)
  ;; evaluates FLANG expressions by reducing them to *expressions*
  (define (eval expr)
    (cases expr
      [(Num n) expr]
      [(Add l r) (arith-op + (eval l) (eval r))]
      [(Sub l r) (arith-op - (eval l) (eval r))]
      [(Mul l r) (arith-op * (eval l) (eval r))]
      [(Div l r) (arith-op / (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (eval named-expr)))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Fun bound-id bound-body) expr]
      [(Call fun-expr arg-expr)
       (let([fval (eval fun-expr)])
         (cases fval
           [(Fun bound-id bound-body)
            (eval (subst bound-body
                         bound-id
                         (eval arg-expr)))]
           [else (error 'eval "`call' expects a function, got: ~s" fval)]))]
 ;;we Add boolean = > < not and if to evaluates
      [(Bool bool) expr]
      [(Equal l r) (logic-op eq? (eval l) (eval r))]
      [(Smaller l r) (logic-op < (eval l) (eval r))]
      [(Bigger l r) (logic-op > (eval l) (eval r))]
      [(If condition case1 case2)
        (let ([switch (eval condition)])
          (if (flang->bool switch) (eval case1) (eval case2)))]
      [(Not exp) (Bool (not(flang->bool (eval exp))))]
      ))

  (: run : String -> (U Number Boolean FLANG))
  ;; evaluate a FLANG program contained in a string
  (define (run str)
    (let ([result (eval (parse str))])
      (cases result
        [(Num n) n]
 ;;we add to run boolean and result
        [(Bool bool) bool]
        [else result])))


  ;; tests
(test (run "{with {foo {fun {x} {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}") =>
      (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
  (test (run "{call {fun {x} {+ x 1}} 4}")
        => 5)
  (test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
        => 4)
    (test (run "{with {add1 {fun {x} {+ x 10}}}
                {with {add1 {fun {x} {+ x 10}}}
                  {with {x 10}
                    {call x {call add3 x}}}}}")=error>"eval: `call' expects a function, got: #(struct:Num 10)")
    (test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
        => 7)
(test (run "{with {x 4} {with {x 6} { + x 7}}}") => 13)
(test (run "{with {x y}}") =error> "parse-sexpr: bad `with' syntax in (with (x y))")
(test (run "{with {x 2} x}") => 2)
(test (run "{fun {x y}}") =error> "parse-sexpr: bad `fun' syntax in (fun (x y))")
(test (run "True") => true)
(test (run "{with {x 2}{* x 5}}") => 10)
(test (run "{with {x 2}{* x x}}") => 4)
(test (run "{not True}") => false) 
(test (run "{> 3 44}") => false) 
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 2} {- {* 2 2} x} }") => 2)
(test (run "{with {x 8}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{if {= 2 1} {then-do True} {else-do False}}") => false)
(test (run "{if {2 2 1} {then-do True} {else-do False}}") =error> "parse-sexpr: bad syntax in (2 2 1)")
(test (run "{with {c True}
 {if c {then-do {> 2 1}} {else-do 2}}}") 
 => true)
(test (run "{call {fun {x}
 {if {< x 2} {then-do x} {else-do {/ x 2}}}} 4}")
 => 2)
(test (run "{call {fun {x}
 {if {= x 2} {then-do x} {else-do {/ x 2}}}} 4}")
 => 2)
(test (run "{call {fun {x}
 {if {not x} {then-do True} {else-do False}}} True}")
 => false)
(test (run "{call foo {fun {x}
 {if {< x 2} {then-do x} {else-do {/ x 2}}}} 4}")
 =error> "bad syntax in (call foo (fun (x) (if (< x 2) (then-do x) (else-do (/ x 2)))) 4)")  
(test (run "{call {+ 2 4}}")
 =error> "bad syntax in (call (+ 2 4))")  
(test (run "{call {+ 2 4}}")
 =error> "bad syntax in (call (+ 2 4))")  
(test (run "{with {x 0}
 {if {> x 0} {/ 2 x} x}}") 
 =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
 (test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}")
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")
