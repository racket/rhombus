#lang wraith plait

;; Based on shrubbery/interp.shrb,
;; translating Shrubbery to Wraith to see how they compare

#| An interpreter from Utah CS 3520, originally witten in Plait.
   The curly-brace S-expressions are replaced with parenthesized
   S-expressions, since curly-braces are for infix now |#

define-type Value
  numV (n : Number)
  closV (arg : Symbol)
        (body : Exp)
        (env : Env)

define-type Exp
  numE (n : Number)
  idE (s : Symbol)
  plusE (l : Exp)
        (r : Exp)
  multE (l : Exp)
        (r : Exp)
  letE (n : Symbol)
       (rhs : Exp)
       (body : Exp)
  lamE (n : Symbol)
       (body : Exp)
  appE (fun : Exp)
       (arg : Exp)

define-type Binding
  bind (name : Symbol)
       (val : Value)

define-type-alias Env (Listof Binding)

define mt-env empty

define extend-env cons

module+ test
  print-only-errors #true

;; parse ----------------------------------------
define parse : (S-Exp -> Exp)
  lambda (s)
    cond
      (s-exp-match? `NUMBER s) (numE (s-exp->number s))
      (s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))
      (s-exp-match? `(+ ANY ANY) s)
        plusE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s)))
      (s-exp-match? `(* ANY ANY) s)
        multE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s)))
      (s-exp-match? `(let ((SYMBOL ANY)) ANY) s)
        let [bs (s-exp->list first
                               (s-exp->list second
                                              s-exp->list s))]
          appE (lamE (s-exp->symbol (first bs))
                     (parse (third (s-exp->list s))))
               (parse (second bs))
      (s-exp-match? `(lambda (SYMBOL) ANY) s)
        lamE (s-exp->symbol (first s-exp->list 
                                     second (s-exp->list s)))
             (parse (third (s-exp->list s)))
      (s-exp-match? `(ANY ANY) s)
        appE (parse (first (s-exp->list s)))
             (parse (second (s-exp->list s)))
      else (error 'parse "invalid input")

module+ test
  test
    parse `2
    numE 2
  test (parse `x) ; note: backquote instead of normal quote
    idE 'x
  test
    parse `(+ 2 1)
    plusE (numE 2) (numE 1)
  test
    parse `(* 3 4)
    multE (numE 3) (numE 4)
  test
    parse `(+ (* 3 4) 8)
    plusE (multE (numE 3) (numE 4))
          (numE 8)
  test
    parse `(let ((x (+ 1 2)))
              y)
    appE (lamE 'x (idE 'y))
         (plusE (numE 1) (numE 2))
  test
    parse `(lambda (x) 9)
    lamE 'x (numE 9)
  test
    parse `(double 9)
    appE (idE 'double) (numE 9)
  test/exn
    parse `((+ 1 2))
    "invalid input"

;; interp ----------------------------------------
define (interp (a : Exp) (env : Env)) : Value
  type-case Exp a
    (numE n) (numV n)
    (idE s) (lookup s env)
    (plusE l r) (num+ (interp l env) (interp r env))
    (multE l r) (num* (interp l env) (interp r env))
    (letE n rhs body) interp body
                             extend-env (bind n (interp rhs env))
                                        env
    (lamE n body) (closV n body env)
    (appE fun arg) type-case Value (interp fun env)
                     (closV n body c-env)
                       interp body
                              extend-env (bind n (interp arg env))
                                         c-env
                     else (error 'interp "not a function")

module+ test
  test
    interp (parse `2) mt-env
    numV 2

  test (interp (parse `2) mt-env) (numV 2)
  test (interp (parse `3) mt-env) (numV 3)

  test/exn
    interp (parse `x) mt-env
    "free variable"

  test
    interp (parse `x)
           extend-env (bind 'x (numV 9)) mt-env
    numV 9

  test
    interp (parse `(+ 2 1)) mt-env
    numV 3
  test
    interp (parse `(* 2 1)) mt-env
    numV 2

  test
    interp (parse `(+ (* 2 3) (+ 5 8))) mt-env
    numV 19

  test
    interp (parse `(lambda (x) (+ x x)))
           mt-env
    closV 'x (plusE (idE 'x) (idE 'x)) mt-env

  test
    interp (parse `(let ((x 5))
                     (+ x x)))
           mt-env
    numV 10

;; num+ and num* ----------------------------------------
define (num-op (op : (Number Number -> Number)) (l : Value) (r : Value)) : Value
  cond
    {(numV? l) and (numV? r)}
      numV (op (numV-n l) (numV-n r))
    else
      error 'interp "not a number"

define (num+ (l : Value) (r : Value)) : Value
  num-op + l r

define (num* (l : Value) (r : Value)) : Value
  num-op * l r

module+ test
  test
    num+ (numV 1) (numV 2)
    numV 3

  test
    num* (numV 2) (numV 3)
    numV 6

;; lookup ----------------------------------------
define lookup : (Symbol Env -> Value)
  lambda (n env)
    cond
      (empty? env) (error 'lookup "free variable")
      else cond
             {n symbol=? (bind-name (first env))}
               bind-val (first env)
             else (lookup n (rest env))

module+ test
  test/exn (lookup 'x mt-env)
           "free variable"
  test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
       numV 8
  test (lookup 'x (extend-env
                    bind 'x (numV 9)
                    extend-env (bind 'x (numV 8)) mt-env))
       numV 9
  test (lookup 'y (extend-env
                    bind 'x (numV 9)
                    extend-env (bind 'y (numV 8)) mt-env))
       numV 8
