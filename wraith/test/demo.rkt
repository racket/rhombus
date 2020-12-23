#lang wraith racket
require syntax/parse/define

;; Based on shrubbery/demo.shrb,
;; translating Shrubbery to Wraith to see how they compare

let [x 1
     y 2]
  {x + y}

let [x 1 & y 2] {x + y}

define pi 3.14

define (fib₁ n)
  displayln "fib called"
  cond
    {n = 0} 0
    {n = 1} 1
    else    {(fib₁ {n - 1}) + (fib₁ {n - 2})}

define/match (fib₂ n)
  (0) 0
  (1) 1
  (n) {(fib₂ {n - 1}) + (fib₂ {n - 2})}

define/match (fib₃ n)
  (0) 0
  (1) 1
  (n) {(fib₃ {n - 1}) ; infix exprs, in curly braces, can span multiple lines
       + (fib₃ {n - 2})}

define fib₄
  lambda (n)
    cond
      {n = 0} 0
      {n = 1} 1
      else    {(fib₄ {n - 1}) + (fib₄ {n - 2})}

; Adding parentheses is ok, and doesn't ruin structure inside them
(define fib₅
  (lambda (n)
    (cond
      {n = 0} 0
      {n = 1} 1
      else    {(fib₅ {n - 1}) + (fib₅ {n - 2})})))

define (fib₆ n)
  match n
    0 0
    1 1
    n {(fib₆ {n - 1}) + (fib₆ {n - 2})}

define (fib₇ n)
  match n
    0
      0
    1
      1
    n
      {(fib₇ {n - 1}) + (fib₇ {n - 2})}

define (fib₈ n)
  match n (0 0) (1 1) (n {(fib₈ {n - 1}) + (fib₈ {n - 2})})

define (make-adder n)
  lambda (m)
    printf "adding to ~a\n" m
    {m + n}

define (analyze₁ n)
  if {n = 0}
     printf "zero\n"
     printf "other\n"
  printf "done\n"

define (analyze₂ n)
  if {n = 0}
     begin
       printf "zero\n"
       printf "done saying zero\n"
     begin
       printf "other\n"
       printf "done saying other\n"

define (analyze₃ n)
  cond
    {n = 0}
      printf "zero\n"
      printf "done saying zero\n"
    else
      printf "other\n"
      printf "done saying other\n"

struct posn₁ (x y)

struct color-posn posn₁ (col)
  #:mutable

;; Need a better struct macro,
;; a job for macro writers, not the parser.
;; This is how it looks with the current Racket struct macro
struct posn₂ [x #:mutable
              y #:auto]
  #:auto-value 7
  #:methods gen:equal+hash
  [define (equal-proc a b eql)
     {{(posn₂-x a) eql (posn₂-x b)} and {(posn₂-y a) eql (posn₂-y b)}}
   define (hash-proc a hc)
     17
   define (hash2-proc a hc)
     19]

struct posn₃ (x y)
  #:property prop:equal+hash
    let [hc lambda (a hc)
              {(hc (posn₃-x a)) + (hc (posn₃-y a))}
         eql lambda (a b eql)
               {(eql (posn₃-x a) (posn₃-x b)) and (eql (posn₃-y a) (posn₃-y b))}]
      list eql hc hc

struct posn₄ (x y)
  #:property prop:equal+hash
    let [hc lambda (a hc)
              {(hc (posn₄-x a)) + (hc (posn₄-y a))}]
      list lambda (a b eql)
             {(eql (posn₄-x a) (posn₄-x b)) and (eql (posn₄-y a) (posn₄-y b))}
           hc
           hc

define (fourth n)
  define m {n * n}
  define v {m * m}
  printf "~a^4 = ~a\n" n v
  v

define (exp n (base 2.718281828459045))
  if {n = 1}
     base
     {base * (exp {n - 1} base)}

define (positive? n) (if {n > 0} #true #false)

define (go)
  define (helper n)
    list n n
  define (more m)
    if {m = 0}
       "done"
       more {m - 1}
  helper (more 9)

define (approx x)
  match x
    `(something ,v)
     printf "got it\n"
     v
    'nothing 0

define (approx-thunk₁ x)
  match x
    `(something ,v) (lambda () v)
    'nothing (lambda () 0)

;; Enough indentation for `v` means that it's grouped with the lambda
define (approx-thunk₂ x)
  match x
    `(something ,v) lambda ()
                      v
    'nothing (lambda () 0)

define (approx-thunk₃ x)
  match x
    `(something ,v) case-lambda
                      () v
                      (n) {v + n}
    'nothing case-lambda
               () 0
               (n) n

define curried₁
  lambda (x)
    lambda (y)
      lambda (z)
        list x y z

define curried₂ lambda (x)
                  lambda (y)
                    lambda (z)
                      list x y z

;; This could be improved by having a hash-table macro
;; using (hash-table (key val) ...) like the match pattern
define dictionary₀ hash
                     .. 'foo 17
                     .. 'bar "string"
                     .. 'baz #true
define dictionary₁ hash
                     'foo & 17
                     'bar & "string"
                     'baz & #true

define-simple-macro
  hash-table (key val) ...
  hash (~@ key val) ...

;; Like this:
define dictionary₂ hash-table
                     'foo 17
                     'bar "string"
                     'baz #true

define colors
  list
    "red"
    "green"
    "blue"
    "orange"

define f x-something
         y-something-else
         z-also-long-name
  5

define (sum l)
  let loop [l l]
    if (null? l)
       0
       {(first l) + (loop (rest l))}

define (show-all l)
  for [x (in-list l)]
    print x
    (newline)

define (show-zip l l2)
  for [x (in-list l)
       x2 (in-list l2)]
    print x
    write-string " "
    print x2
    (newline)

define (show-combos-not-same l l2)
  for* [x (in-list l)
        x2 (in-list l2)
        #:when (not (equal? x x2))]
    print x
    write-string " "
    print x2
    (newline)

define (map f l)
  for/list [x (in-list l)]
    f x

define (partition l pred)
  for/fold [yes '()
            no '()
            #:result (list (reverse yes) (reverse no))]
           [x (in-list l)]
    if (pred x)
       values (cons x yes) no
       values yes (cons x no)

local [define x 1
       define y 2]
  {x + y}
