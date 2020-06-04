#lang pl 03
#|

Part A:

BNF for the MUWAE language:

< MUWAE> ::=  <num>
            | {+ <MUWAE> <MUWAE>}
            | {- <MUWAE> <MUWAE>}
            | {* <MUWAE> <MUWAE>}
            | {/ <MUWAE>  MUWAE>}
            | {with {<id> <MUWAE>} <MUWAE>}
            | <id>
            | {sqrt <MUWAE>}
|#

;MUWAE abstract syntax trees
;A new defination for MUWAE:
(define-type  MUWAE
  [Num (Listof Number)]
  [Add MUWAE MUWAE]
  [Sub MUWAE MUWAE]
  [Mul MUWAE MUWAE]
  [Div MUWAE MUWAE]
  [With Symbol MUWAE MUWAE]
  [Id Symbol]
  [Sqrt MUWAE]) ;Added Sqrt.

;A parse-sexpr for MUWAE:
(: parse-sexpr : Sexpr -> MUWAE)
;To convert s-expressions into MUWAEs.
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num (list n))] ;List of numbers for sqrt.
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'sqrt rest) (Sqrt (parse-sexpr rest))] ;Added sqrt parse-sexpr.
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;A parser for MUWAE:
(: parse : String -> MUWAE)
;Parses a string containing a MUWAE expression to a MUWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#|
Formal specs for `subst:

(`N' is a <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>, `y' is a *different* <id>)

N[v/x] = N

{+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
{- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
{* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
{/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}

y[v/x] = y
x[v/x] = v

{with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
{with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
{sqrt E1}[v/x] = {sqrt E1[v/x]} ;Added rule for sqrt.
|#

(: subst : MUWAE Symbol MUWAE -> MUWAE)
;substitutes the second argument with the third argument in the first argument,
;as per the rules of substitution.
;the resulting expression contains no free instances of the second argument.
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Sqrt rest) (Sqrt (subst rest from to))] ;Added rule for Sqrt. 
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body) (With bound-id (subst named-expr from to)
                                                 (if (eq? bound-id from) bound-body
                                                     (subst bound-body from to)))]))

#|
Formal specs for `eval':

eval(N) = N 
eval({+ E1 E2}) = eval(E1) + eval(E2)
eval({- E1 E2}) = eval(E1) - eval(E2)
eval({* E1 E2}) = eval(E1) * eval(E2)
eval({/ E1 E2}) = eval(E1) / eval(E2)
eval(id) = problem
eval({with {x E1} E2}) = eval(E2 [eval(E1)/x])
eval({sqrt E1}) = sqrt eval(E1) ;Added rule for sqrt.
|#

(: eval : MUWAE -> (Listof Number))
;Evaluates MUWAE expressions by reducing them to numbers.
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add left right) (bin-op + (eval left) (eval right))]
    [(Sub left right) (bin-op - (eval left) (eval right))]
    [(Mul left right) (bin-op * (eval left) (eval right))]
    [(Div left right) (bin-op / (eval left) (eval right))]
    [(With bound-id named-expr bound-body) (eval (subst bound-body bound-id (Num (eval named-expr))))] 
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Sqrt e) (sqrt+ (eval e))])) ;Added Sqrt eval.

(: run : String -> (Listof Number))
;Evaluate a MUWAE program contained in a string.
(define (run str)
  (eval (parse str)))

;From internet.
;This function returns list of 2 numbers - results for sqrt operation.
(: sqrt+ : (Listof Number) -> (Listof Number)) 
(define (sqrt+ ns)
  (cond [(null? ns) ns]
        [(< (first ns) 0) (error sqrt "`sqrt' requires a nonnegative input")] ;if negative - error.
        [else (cons (sqrt (first ns)) ;else - sqrt.
                    (cons (* -1 (sqrt (first ns)))
                          (sqrt+ (rest ns))))]))

;This function helps to compute any operation for 2 results of sqrt operation.
(: bin-op : (Number Number -> Number) (Listof Number) (Listof Number) -> (Listof Number))
;applies a binary numeric function on all combinations of numbers from the two input lists,
;and return the list of all of the results
(define (bin-op op ls rs)
(: helper : Number (Listof Number) -> (Listof Number))
  (define (helper l rs)
    (: f : Number -> Number)
    (define (f num)
      (op l num)) ;Computes the operator we got on left number.
    (map f rs))
  (if (null? ls)
      null
      (append (helper (first ls) rs) (bin-op op (rest ls) rs))))

;tests:
(test (run "1") => '(1))

(test (run "{+ 2 3}") => '(5))
(test (run "{+ -1 1}") => '(0))
(test (run "{+ {* 2 3} {* 2 2}}") => '(10))

(test (run "{- 2 3}") => '(-1))
(test (run "{- 10 3}") => '(7))

(test (run "{* 2 3}") => '(6))
(test (run "{* 10 0}") => '(0))

(test (run "{/ 2 -1}") => '(-2))
(test (run "{/ 10 10}") => '(1))

(test (run "{* 3}") =error> "bad syntax in ")

(test (run "{with {x 3} {/ x x}}") => '(1))
(test (run "{with {z 10} {with {w -10} {/ w z}}}") => '(-1))
(test (run "{with {a 1} b}") =error> "free identifier")
(test (run "{with {/ x x}}") =error> "bad `with' syntax in ")
(test (run "{with {a 1} a}") => '(1))
(test (run "{with {x 3} {+ x x}}") => '(6))
(test (run "{with {y 3} {- y y}}") => '(0))
(test (run "{with {z 0} {* z z}}") => '(0))
(test (run "{with {w 10} {/ w w}}") => '(1))
(test (run "{with {x 100} {sqrt x}}") => '(10 -10))
(test (run "{with {x 100} 100}") => '(100))
(test (run "{with {x 100} {with {x 100} x}}") => '(100))

(test (run "{sqrt 9}") => '(3 -3))
(test (run "{sqrt 1}") => '(1 -1))
(test (run "{sqrt 0}") => '(0 0))
(test (run "{sqrt 36}") => '(6 -6))
(test (run "{sqrt -1}") =error> "`sqrt' requires a nonnegative input")
(test (run "{sqrt -0}") => '(0 0))

(test (run "{+ {sqrt 1} 3}") => '(4 2))
(test (run "{* {sqrt 1} 1}") => '(1 -1))
(test (run "{+ {sqrt -1} 0}") =error> "`sqrt' requires a nonnegative input")
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}") => '(12 -8 11 -9))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}") => '(5 -5 4 -4))
(test (run "{sqrt {+ 16 84}}") => '(10 -10))


#|
Part B:

BNF for the WAE language:

<WAE> ::=  <num>
         | {+ <WAE> <WAE>}
         | {- <WAE> <WAE>}
         | {* <WAE> <WAE>}
         | {/ <WAE> <WAE>}
         | {with { <id> <WAE>} <WAE>}
         | <id>
|#

;WAE abstract syntax trees
(define-type WAE
  [Num2  Number]
  [Add2  WAE WAE]
  [Sub2  WAE WAE]
  [Mul2  WAE WAE]
  [Div2  WAE WAE]
  [Id2   Symbol]
  [With2 Symbol WAE WAE])

  (: parse-sexpr2 : Sexpr -> WAE)
  ;; to convert s-expressions into WAEs
  (define (parse-sexpr2 sexpr)
    (match sexpr
      [(number: n)    (Num2 n)]
      [(symbol: name) (Id2 name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With2 name (parse-sexpr2 named) (parse-sexpr2 body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add2 (parse-sexpr2 lhs) (parse-sexpr2 rhs))]
      [(list '- lhs rhs) (Sub2 (parse-sexpr2 lhs) (parse-sexpr2 rhs))]
      [(list '* lhs rhs) (Mul2 (parse-sexpr2 lhs) (parse-sexpr2 rhs))]
      [(list '/ lhs rhs) (Div2 (parse-sexpr2 lhs) (parse-sexpr2 rhs))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse2 : String -> WAE)
  ;; parses a string containing a WAE expression to a WAE AST
  (define (parse2 str)
    (parse-sexpr2 (string->sexpr str)))

  #| Formal specs for `subst':
     (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>, `y' is a
     *different* <id>)
        N[v/x]                = N
        {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
        {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
        {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
        {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
        y[v/x]                = y
        x[v/x]                = v
        {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
        {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
  |#

  (: subst2 : WAE Symbol WAE -> WAE)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst2 expr from to)
    (cases expr
      [(Num2 n) expr]
      [(Add2 l r) (Add2 (subst2 l from to) (subst2 r from to))]
      [(Sub2 l r) (Sub2 (subst2 l from to) (subst2 r from to))]
      [(Mul2 l r) (Mul2 (subst2 l from to) (subst2 r from to))]
      [(Div2 l r) (Div2 (subst2 l from to) (subst2 r from to))]
      [(Id2 name) (if (eq? name from) to expr)]
      [(With2 bound-id named-expr bound-body)
       (With2 bound-id
             (subst2 named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst2 bound-body from to)))]))



(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)
  (cases expr
    [(Num2 n) '()]
    [(Add2 l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(Sub2 l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(Mul2 l r) (append (freeInstanceList l) (freeInstanceList r))] 
    [(Div2 l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(With2 bound-id named-expr bound-body)
     (append (freeInstanceList named-expr)
             (freeInstanceList (subst2 bound-body bound-id (Num2 0))))] 
    [(Id2 name) (list name)]))

;Tests:
(test (freeInstanceList (parse2 "w")) => '(w))
(test (freeInstanceList (parse2 "{+ x y}")) => '(x y))
(test (freeInstanceList (parse2 "{- z {+ x y}}")) => '(z x y))
(test (freeInstanceList (parse2 "{* {+ x x} y}")) => '(x x y))
(test (freeInstanceList (parse2 "{/ w {- z {+ x y}}}")) => '(w z x y))
(test (freeInstanceList (parse2 "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (parse2 "{with {xxx 10} {with {yyy 20} {+ {- xxx yyy} z}}}")) => '(z))
(test (freeInstanceList (parse2 "{with {x 4} {with {yy 5} {+ {- x y} z}}}")) => '(y z))
(test (freeInstanceList (parse2 "{with {w 1} {with {z 0} {+ {- w z} z}}}")) => '())
(test (freeInstanceList (parse2 "{with {a 8} {+ {- b c} d}}")) => '(b c d))
(test (freeInstanceList (parse2 "{with {a 8} {+ {- a b} d}}")) => '(b d))
(test (freeInstanceList (parse2 "{with {+ {- a b} d}}")) =error> "bad `with' syntax in ")
(test (freeInstanceList (parse2 "{+ d}")) =error> "bad syntax in ")

(test (freeInstanceList (Id2 'x)) => '(x))
(test (freeInstanceList (Num2 2)) => '())

(test (freeInstanceList (Add2 (Id2 'x) (Id2 'y))) => '(x y))
(test (freeInstanceList (Add2 (Num2 2) (Id2 'y))) => '(y))
(test (freeInstanceList (Add2 (Num2 1) (Num2 2))) => '())

(test (freeInstanceList (Sub2 (Id2 'x) (Id2 'y))) => '(x y))
(test (freeInstanceList (Sub2 (Id2 'x) (Num2 3))) => '(x))
(test (freeInstanceList (Sub2 (Num2 4) (Num2 3))) => '())

(test (freeInstanceList (Mul2 (Id2 'x) (Id2 'y))) => '(x y))
(test (freeInstanceList (Mul2 (Num2 2) (Id2 'y))) => '(y))
(test (freeInstanceList (Mul2 (Num2 1) (Num2 2))) => '())

(test (freeInstanceList (Div2 (Id2 'x) (Id2 'y))) => '(x y))
(test (freeInstanceList (Div2 (Id2 'x) (Num2 3))) => '(x))
(test (freeInstanceList (Div2 (Num2 4) (Num2 3))) => '())

(test (freeInstanceList (With2 'x (Num2 2) (Add2 (Id2 'x) (Num2 3)))) => '())
(test (freeInstanceList (With2 'y (Num2 5) (Mul2 (Id2 'z) (Id2 'z)))) => '(z z))
(test (freeInstanceList (With2 'z (Num2 4) (Sub2 (Id2 'z) (Id2 'x)))) => '(x))
(test (freeInstanceList (With2 'z (Num2 4) (Div2 (Id2 'a) (Id2 'b)))) => '(a b))
(test (freeInstanceList (With2 'z (Num2 6) (With2 'z (Num2 6) (Id2 'z)))) => '())

(test (freeInstanceList (With2 'z (Num2 6) (With2 'w (Num2 7) (Sub2 (Id2 'z) (Id2 'w))))) => '())
(test (freeInstanceList (With2 'a (Num2 10) (With2 'b (Num2 11) (Sub2 (Id2 'c) (Id2 'b))))) => '(c))
(test (freeInstanceList (With2 'a (Num2 10) (With2 'b (Num2 11) (Sub2 (Id2 'c) (Id2 'd))))) => '(c d))
