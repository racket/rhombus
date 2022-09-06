#lang racket/base

(provide input1s
         input1
         expected1

         input1a
         expected1a

         input2
         expected2

         input3
         expected3

         input4
         expected4

         input5
         expected5

         input6
         expected6

         input7
         expected7)

;; input1 is split into parts to accomodate O(n^2) tests
(define input1s
  (list
#<<INPUT
let (x = 1,
     y = 2):
  x+y

let (x = 1, y = 2): x+y

define pi: 3.14

define fib(n):
  log_error("fib called")
  cond | n == 0: 0
       | n == 1: 1
       | else: fib(n-1) + fib(n-2)

define
 | fib(0): 0
 | fib(1): 1
 | fib(n): fib(n-1) + fib(n-2)

define
 | fib(0): 0
 | fib(1): 1
 | fib(n): fib(n-1)
             + fib(n-2)

define fib:
  lambda (n):
    cond
     | n == 0: 0
     | n == 1: 1
     | else: fib(n-1) + fib(n-2)

// Ok to add `:` before `|`. This parses the
// same as the prevous example, but this is not the standard
// style (which is to omit unnecessary colons).
define fib:
  lambda (n):
    cond
     | n == 0: 0
     | n == 1: 1
     | else: fib(n-1) + fib(n-2)

// Adding parentheses is ok, at least with the obvious handling
// of parentheses by the use of sapling notation, but the
// parentheses are apparent in the sampling parse.
(define fib:
  (lambda (n):
    (cond
      | (n == 0): 0
      | (n == 1): 1
      | else: (fib(n-1) + fib(n-2)))))

// For maximal noise, you could add parentheses and trailing colons.
// But we won't.

INPUT

;; ----------------------------------------

#<<INPUT
// START: all of the next `fib` definitions are exactly the same

define fib(n):
  match n
   | 0: 0
   | 1: 1
   | n: fib(n-1) + fib(n-2)

define fib(n):
  match n
   | 0: 0
   | 1: 1
   | n: fib(n-1) + fib(n-2)

define fib(n):
  match n | 0: 0
          | 1: 1
          | n: fib(n-1) + fib(n-2)

define fib(n):
  match n
   | 0:
       0
   | 1:
       1
   | n:
       fib(n-1) + fib(n-2)

// END equivalent `fib` definitions

define make_adder(n):
  lambda (m):
    printf("adding to ~a\n", m)
    m+n

define analyze(n):
  if n == 0
   | printf("zero\n")
   | printf("other\n")
  printf("done\n")

define analyze(n):
  if n == 0
   | printf("zero\n")
     printf("done saying zero\n")
   | printf("other\n")
     printf("done saying other\n")

struct posn(x, y)

struct color_posn(col):
  extends posn
  mutable

struct posn(x mutable,
            y = 7):
  methods equality:
    define equal(a, b):
      is_posn(b) => (a.x == b.x && a.y == b.y)
    define hash(a):
      17
    define secondary_hash(a):
      19

struct posn(x, y):
  property prop_equal_and_hash:
    let (hc = lambda (a: posn, hc):
                 hc(a.x) + hc(a.y),
         eql = lambda (a: posn, b: posn, eql):
                 eql(a.x, b.x) && eql(a.y, b.y)):
      values(eql, hc, hc)

struct posn(x, y):
  property prop_equal_and_hash:
    let (hc = lambda (a: posn, hc):
                hc(a.x) + hc(a.y)):
      (lambda (a: posn, b: posn, eql):
         eql(a.x, b.x) && eql(a.y, b.y),
       hc,
       hc)

INPUT

;; ----------------------------------------

#<<INPUT
// Another possibile approach to syntax for `struct`:
struct posn:
  fields:
    x mutable
    y = 7
  methods equality:
    define equal(a, b):
      is_posn(b) => (a.x == b.x && a.y == b.y)
    define hash(a):
      17
    define secondary_hash(a):
      19
  property prop_quality: "high"

define fourth(n :: Integer):
  define m: n*n
  define v: m*m
  printf("~a^4 = ~a\n", n, v)
  v

define exp(n :: Integer, ~base: base = 2.718281828459045):
  if (n == 1)
   | base
   | base * exp(n-1, ~base: base)

define positive_p(n): if n > 0 | true | false

define go():
  define helper(n):
    list(n, n)
  define more(m):
    if m == 0 | "done"
              | more(m - 1)
  helper(more(9))

define approx(x):
  match x
   | something(v):
       printf("got it\n")
       v
   | nothing: 0

// With two `:`s on one line, there's no way to
// add to the first `:`
define approx_thunk(x):
  match x
   | something(v): lambda (): v
   | nothing: lambda (): 0

// Enough indentation for `v` means that it continues the
// implicit second `:`, so the `lambda` body has `v`:
define approx_thunk(x):
  match x
   | something(v): lambda ():
                      v
   | nothing: lambda (): 0

define approx_thunk(x):
  match x
   | something(v): lambda
                    | (): v
                    | (n): v+n
   | nothing: lambda
               | (): 0
               | (n): n

INPUT

;; ----------------------------------------

#<<INPUT

define curried:
  lambda (x):
    lambda (y):
      lambda (z):
        list(x, y, z)

define curried: lambda (x):
                  lambda (y):
                    lambda (z):
                      list(x, y, z)

define dictionary: dict:
                      foo: 17
                      bar: string
                      baz: true

define colors:
  list(
    red,
    green,
    blue,
    orange,
  )

define f(x_something,
         y_something_else,
         z_also_long_name):
  5

define sum(l):
  let loop(l = l):
    if is_null(l)
     | 0
     | first(l) + loop(rest(l))

define show_all(l):
  for (x = in_list(l)):
    print(x)
    newline()

define show_zip(l, l2):
  for (x = in_list(l),
       x2 = in_list(l2)):
    print(x)
    print_string(" ")
    print(x2)
    newline()

define show_combos_not_same(l, l2):
  for (x = in_list(l)):
   then (x2 = in_list(l2)):
     when !is_equal(x, x2):
       print(x)
       print_string(" ")
       print(x2)
       newline()

define map(f, l):
  for list (x = in_list(l)):
    f(x)

define partition(l, pred):
  for fold (yes = empty,
            no = empty,
            result (reverse(yes), reverse(no))):
   with (x = in_list(l)):
     if pred(x)
      | (cons(x, yes), no)
      | (yes, cons(x, no))

local:
  with:
    define x: 1
    define y: 2
  in:
   x+y

:
(:)
(:, 2)
(1, :)

(| 1 | 2)

INPUT
))

(define input1 (apply string-append input1s))

(define expected1
  '(top
    (group
     let
     (parens (group x (op =) 1) (group y (op =) 2))
     (block (group x (op +) y)))
    (group
     let
     (parens (group x (op =) 1) (group y (op =) 2))
     (block (group x (op +) y)))
    (group define pi (block (group 3.14)))
    (group
     define
     fib
     (parens (group n))
     (block
      (group log_error (parens (group "fib called")))
      (group
       cond
       (alts
        (block (group n (op ==) 0 (block (group 0))))
        (block (group n (op ==) 1 (block (group 1))))
        (block
         (group
          else
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group
     define
     (alts
      (block (group fib (parens (group 0)) (block (group 0))))
      (block (group fib (parens (group 1)) (block (group 1))))
      (block
       (group
        fib
        (parens (group n))
        (block
         (group
          fib
          (parens (group n (op -) 1))
          (op +)
          fib
          (parens (group n (op -) 2))))))))
    (group
     define
     (alts
      (block (group fib (parens (group 0)) (block (group 0))))
      (block (group fib (parens (group 1)) (block (group 1))))
      (block
       (group
        fib
        (parens (group n))
        (block
         (group
          fib
          (parens (group n (op -) 1))
          (op +)
          fib
          (parens (group n (op -) 2))))))))
    (group
     define
     fib
     (block
      (group
       lambda
       (parens (group n))
       (block
        (group
         cond
         (alts
          (block (group n (op ==) 0 (block (group 0))))
          (block (group n (op ==) 1 (block (group 1))))
          (block
           (group
            else
            (block
             (group
              fib
              (parens (group n (op -) 1))
              (op +)
              fib
              (parens (group n (op -) 2))))))))))))
    (group
     define
     fib
     (block
      (group
       lambda
       (parens (group n))
       (block
        (group
         cond
         (alts
          (block (group n (op ==) 0 (block (group 0))))
          (block (group n (op ==) 1 (block (group 1))))
          (block
           (group
            else
            (block
             (group
              fib
              (parens (group n (op -) 1))
              (op +)
              fib
              (parens (group n (op -) 2))))))))))))
    (group
     (parens
      (group
       define
       fib
       (block
        (group
         (parens
          (group
           lambda
           (parens (group n))
           (block
            (group
             (parens
              (group
               cond
               (alts
                (block (group (parens (group n (op ==) 0)) (block (group 0))))
                (block (group (parens (group n (op ==) 1)) (block (group 1))))
                (block
                 (group
                  else
                  (block
                   (group
                    (parens
                     (group
                      fib
                      (parens (group n (op -) 1))
                      (op +)
                      fib
                      (parens (group n (op -) 2))))))))))))))))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))    
    (group
     define
     make_adder
     (parens (group n))
     (block
      (group
       lambda
       (parens (group m))
       (block
        (group printf (parens (group "adding to ~a\n") (group m)))
        (group m (op +) n)))))
    (group
     define
     analyze
     (parens (group n))
     (block
      (group
       if
       n
       (op ==)
       0
       (alts
        (block (group printf (parens (group "zero\n"))))
        (block (group printf (parens (group "other\n"))))))
      (group printf (parens (group "done\n")))))
    (group
     define
     analyze
     (parens (group n))
     (block
      (group
       if
       n
       (op ==)
       0
       (alts
        (block
         (group printf (parens (group "zero\n")))
         (group printf (parens (group "done saying zero\n"))))
        (block
         (group printf (parens (group "other\n")))
         (group printf (parens (group "done saying other\n"))))))))
    (group struct posn (parens (group x) (group y)))
    (group
     struct
     color_posn
     (parens (group col))
     (block (group extends posn) (group mutable)))
    (group
     struct
     posn
     (parens (group x mutable) (group y (op =) 7))
     (block
      (group
       methods
       equality
       (block
        (group
         define
         equal
         (parens (group a) (group b))
         (block
          (group
           is_posn
           (parens (group b))
           (op =>)
           (parens
            (group
             a
             (op |.|)
             x
             (op ==)
             b
             (op |.|)
             x
             (op &&)
             a
             (op |.|)
             y
             (op ==)
             b
             (op |.|)
             y)))))
        (group define hash (parens (group a)) (block (group 17)))
        (group define secondary_hash (parens (group a)) (block (group 19)))))))
    (group
     struct
     posn
     (parens (group x) (group y))
     (block
      (group
       property
       prop_equal_and_hash
       (block
        (group
         let
         (parens
          (group
           hc
           (op =)
           lambda
           (parens (group a (block (group posn))) (group hc))
           (block
            (group
             hc
             (parens (group a (op |.|) x))
             (op +)
             hc
             (parens (group a (op |.|) y)))))
          (group
           eql
           (op =)
           lambda
           (parens
            (group a (block (group posn)))
            (group b (block (group posn)))
            (group eql))
           (block
            (group
             eql
             (parens (group a (op |.|) x) (group b (op |.|) x))
             (op &&)
             eql
             (parens (group a (op |.|) y) (group b (op |.|) y))))))
         (block (group values (parens (group eql) (group hc) (group hc)))))))))
    (group
     struct
     posn
     (parens (group x) (group y))
     (block
      (group
       property
       prop_equal_and_hash
       (block
        (group
         let
         (parens
          (group
           hc
           (op =)
           lambda
           (parens (group a (block (group posn))) (group hc))
           (block
            (group
             hc
             (parens (group a (op |.|) x))
             (op +)
             hc
             (parens (group a (op |.|) y))))))
         (block
          (group
           (parens
            (group
             lambda
             (parens
              (group a (block (group posn)))
              (group b (block (group posn)))
              (group eql))
             (block
              (group
               eql
               (parens (group a (op |.|) x) (group b (op |.|) x))
               (op &&)
               eql
               (parens (group a (op |.|) y) (group b (op |.|) y)))))
            (group hc)
            (group hc)))))))))
    (group
     struct
     posn
     (block
      (group fields (block (group x mutable) (group y (op =) 7)))
      (group
       methods
       equality
       (block
        (group
         define
         equal
         (parens (group a) (group b))
         (block
          (group
           is_posn
           (parens (group b))
           (op =>)
           (parens
            (group
             a
             (op |.|)
             x
             (op ==)
             b
             (op |.|)
             x
             (op &&)
             a
             (op |.|)
             y
             (op ==)
             b
             (op |.|)
             y)))))
        (group define hash (parens (group a)) (block (group 17)))
        (group define secondary_hash (parens (group a)) (block (group 19)))))
      (group property prop_quality (block (group "high")))))
    (group
     define
     fourth
     (parens (group n (op ::) Integer))
     (block
      (group define m (block (group n (op *) n)))
      (group define v (block (group m (op *) m)))
      (group printf (parens (group "~a^4 = ~a\n") (group n) (group v)))
      (group v)))
    (group
     define
     exp
     (parens
      (group n (op ::) Integer)
      (group #:base (block (group base (op =) 2.718281828459045))))
     (block
      (group
       if
       (parens (group n (op ==) 1))
       (alts
        (block (group base))
        (block
         (group
          base
          (op *)
          exp
          (parens (group n (op -) 1) (group #:base (block (group base))))))))))
    (group
     define
     positive_p
     (parens (group n))
     (block
      (group if n (op >) 0 (alts (block (group true)) (block (group false))))))
    (group
     define
     go
     (parens)
     (block
      (group
       define
       helper
       (parens (group n))
       (block (group list (parens (group n) (group n)))))
      (group
       define
       more
       (parens (group m))
       (block
        (group
         if
         m
         (op ==)
         0
         (alts
          (block (group "done"))
          (block (group more (parens (group m (op -) 1))))))))
      (group helper (parens (group more (parens (group 9)))))))
    (group
     define
     approx
     (parens (group x))
     (block
      (group
       match
       x
       (alts
        (block
         (group
          something
          (parens (group v))
          (block (group printf (parens (group "got it\n"))) (group v))))
        (block (group nothing (block (group 0))))))))
    (group
     define
     approx_thunk
     (parens (group x))
     (block
      (group
       match
       x
       (alts
        (block
         (group
          something
          (parens (group v))
          (block (group lambda (parens) (block (group v))))))
        (block
         (group nothing (block (group lambda (parens) (block (group 0))))))))))
    (group
     define
     approx_thunk
     (parens (group x))
     (block
      (group
       match
       x
       (alts
        (block
         (group
          something
          (parens (group v))
          (block (group lambda (parens) (block (group v))))))
        (block
         (group nothing (block (group lambda (parens) (block (group 0))))))))))
    (group
     define
     approx_thunk
     (parens (group x))
     (block
      (group
       match
       x
       (alts
        (block
         (group
          something
          (parens (group v))
          (block
           (group
            lambda
            (alts
             (block (group (parens) (block (group v))))
             (block (group (parens (group n)) (block (group v (op +) n)))))))))
        (block
         (group
          nothing
          (block
           (group
            lambda
            (alts
             (block (group (parens) (block (group 0))))
             (block (group (parens (group n)) (block (group n)))))))))))))
    (group
     define
     curried
     (block
      (group
       lambda
       (parens (group x))
       (block
        (group
         lambda
         (parens (group y))
         (block
          (group
           lambda
           (parens (group z))
           (block (group list (parens (group x) (group y) (group z)))))))))))
    (group
     define
     curried
     (block
      (group
       lambda
       (parens (group x))
       (block
        (group
         lambda
         (parens (group y))
         (block
          (group
           lambda
           (parens (group z))
           (block (group list (parens (group x) (group y) (group z)))))))))))
    (group
     define
     dictionary
     (block
      (group
       dict
       (block
        (group foo (block (group 17)))
        (group bar (block (group string)))
        (group baz (block (group true)))))))
    (group
     define
     colors
     (block
      (group
       list
       (parens (group red) (group green) (group blue) (group orange)))))
    (group
     define
     f
     (parens
      (group x_something)
      (group y_something_else)
      (group z_also_long_name))
     (block (group 5)))
    (group
     define
     sum
     (parens (group l))
     (block
      (group
       let
       loop
       (parens (group l (op =) l))
       (block
        (group
         if
         is_null
         (parens (group l))
         (alts
          (block (group 0))
          (block
           (group
            first
            (parens (group l))
            (op +)
            loop
            (parens (group rest (parens (group l))))))))))))
    (group
     define
     show_all
     (parens (group l))
     (block
      (group
       for
       (parens (group x (op =) in_list (parens (group l))))
       (block (group print (parens (group x))) (group newline (parens))))))
    (group
     define
     show_zip
     (parens (group l) (group l2))
     (block
      (group
       for
       (parens
        (group x (op =) in_list (parens (group l)))
        (group x2 (op =) in_list (parens (group l2))))
       (block
        (group print (parens (group x)))
        (group print_string (parens (group " ")))
        (group print (parens (group x2)))
        (group newline (parens))))))
    (group
     define
     show_combos_not_same
     (parens (group l) (group l2))
     (block
      (group
       for
       (parens (group x (op =) in_list (parens (group l))))
       (block
        (group
         then
         (parens (group x2 (op =) in_list (parens (group l2))))
         (block
          (group
           when
           (op !)
           is_equal
           (parens (group x) (group x2))
           (block
            (group print (parens (group x)))
            (group print_string (parens (group " ")))
            (group print (parens (group x2)))
            (group newline (parens))))))))))
    (group
     define
     map
     (parens (group f) (group l))
     (block
      (group
       for
       list
       (parens (group x (op =) in_list (parens (group l))))
       (block (group f (parens (group x)))))))
    (group
     define
     partition
     (parens (group l) (group pred))
     (block
      (group
       for
       fold
       (parens
        (group yes (op =) empty)
        (group no (op =) empty)
        (group
         result
         (parens
          (group reverse (parens (group yes)))
          (group reverse (parens (group no))))))
       (block
        (group
         with
         (parens (group x (op =) in_list (parens (group l))))
         (block
          (group
           if
           pred
           (parens (group x))
           (alts
            (block
             (group
              (parens (group cons (parens (group x) (group yes))) (group no))))
            (block
             (group
              (parens
               (group yes)
               (group cons (parens (group x) (group no))))))))))))))
    (group
     local
     (block
      (group
       with
       (block
        (group define x (block (group 1)))
        (group define y (block (group 2)))))
      (group in (block (group x (op +) y)))))
    (group (block))
    (group (parens (group (block))))
    (group (parens (group (block)) (group 2)))
    (group (parens (group 1) (group (block))))
    (group (parens (group (alts (block (group 1)) (block (group 2))))))
    ))

;; has « », so unarmoring won't work
(define input1a
#<<INPUT
define fib(n):
  match n | 0: « 0 »
          | 1: « 1 »
          | n: « fib(n-1) + fib(n-2) »

define fib(n):
  match n | « 0: 0 » | « 1: 1 » | n: fib(n-1) + fib(n-2)

define fib(n): « match n | « 0: 0 » | « 1: 1 » | n: fib(n-1) + fib(n-2) »

define fib(n): « match n | 0: «0» | 1: «1» | n: « fib(n-1) + fib(n-2) » »

define fib(n): « match n | « 0: «0» » | « 1: «1» » | « n: « fib(n-1) + fib(n-2) » » »

define fib(n): « match n | « 0: «0» » | « 1: «1» » | « n: « fib(n-1) + fib(n-2) » » »

if t | if f | a | b | y
if t |« if f | a | b » | y
if t |« tag: if f | a | b » | y
if t |« tag: «if f | a | b » » | y

x: y: a; b ; c
x: y:« a; b »; c

if t | x | y; z
if t | x |« y »; z

empty:«»

x:«
    #//
    2;
    3 »

branch |« x»;

;« top_a; top_b » top_c
;;;« top_d ;« top_e; top_f » »

if t | x | ;« y »; z

a(;«1», 2)
INPUT
  )

(define expected1a
  '(top
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0 (block (group 0))))
        (block (group 1 (block (group 1))))
        (block
         (group
          n
          (block
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
    (group if t (alts (block (group if f)) (block (group a)) (block (group b)) (block (group y))))
    (group if t (alts (block (group if f (alts (block (group a)) (block (group b))))) (block (group y))))
    (group if t (alts (block (group tag (block (group if f (alts (block (group a)) (block (group b))))))) (block (group y))))
    (group if t (alts (block (group tag (block (group if f (alts (block (group a)) (block (group b))))))) (block (group y))))
    (group x (block (group y (block (group a) (group b) (group c)))))
    (group x (block (group y (block (group a) (group b))) (group c)))
    (group if t (alts (block (group x)) (block (group y) (group z))))
    (group if t (alts (block (group x)) (block (group y))))
    (group z)
    (group empty (block))
    (group x (block (group 3)))
    (group branch (alts (block (group x))))
    (group top_a)
    (group top_b)
    (group top_c)
    (group top_d)
    (group top_e)
    (group top_f)
    (group if t (alts (block (group x)) (block (group y) (group z))))
    (group a (parens (group 1) (group 2)))))
  
(define input2
#<<INPUT

// A set of examples to see what happens with various forms,
// where many of them seem nonsensical

somthing else: 8

this is a \
  very long linear group \
  that spans multiple lines

this is | one: a \
                 long result
        | two \
            also long
        | three:
            result \
              nested \
              here

this is \
  the first group
this \ is \ the \ second \ group

this is a group \  with (a,
                         nested,
                         list)

this is a group \
  with (a,
                         nested,
                         list)

this is a group \
 with (a,
                \
       nested,
                \
       list)

this is a group \
 with (a,
                \
       /* this a comment on `nested`: */
       nested,
                \
       list)

hello | a | c\
 :
               d

this: \
      is more
             foo

foo
 | more \
 | again:
              sub

a
|
  b | x
  d

something +
more stuff

something:
  more stuff

define
 | fib(0) = 0
 | fib(1) = 1
 | fib(n) =
   fib(n-1) + fib(n-2)

define
 | fib(0) = 0
 | fib(1) = 1
 | fib(n) = fib(n-1) + fib(n-2)
 | more

nonsense:
  hello | there 4.5
        | more f(8)
          next (ok | stuff
                   | stuff: (begin:
                               more
                               things
                             ,
                             separately)
                            again)
          something

q:
  x; y
z

q:
  x; y;
z

q:
  w:
    x; y;
z

x + w | z : :
              y

let | x = 8
    | y = 9: 10
             let | x = 8
    | y = 9: 10
             show(x + y)
             x - y

letrec_syntax_and_values:
  (m = (syntax_rules ....),
   n = (syntax_rules ....)):
    (x = 10,
     y = 12)
    => body
    more

x something | a
              y:
                w:
                  q
            | c
              z
              +& b

x | indentize
    y
    z
  | indentize:
      y

define fib(n):
 match n
  | 0
    : 0
  | 1
    : 1
  | n
    : fib(n-1)
      + fib(n-2)
      more

begin:
  dictionary = [
    "foo" : 17,
    "bar" : "string",
    "baz" : #true
  ]

cond | null: 0 | list(x): 1
     | cons(a, d): f(a) + f(d)
INPUT
  )

(define expected2
  '(top
    (group somthing else (block (group 8)))
    (group this is a very long linear group that spans multiple lines)
    (group
     this
     is
     (alts
      (block (group one (block (group a long result))))
      (block (group two also long))
      (block (group three (block (group result nested here))))))
    (group this is the first group)
    (group this is the second group)
    (group this is a group with (parens (group a) (group nested) (group list)))
    (group this is a group with (parens (group a) (group nested) (group list)))
    (group this is a group with (parens (group a) (group nested) (group list)))
    (group this is a group with (parens (group a) (group nested) (group list)))
    (group hello (alts (block (group a)) (block (group c (block (group d))))))
    (group this (block (group is more) (group foo)))
    (group
     foo
     (alts (block (group more)) (block (group again (block (group sub))))))
    (group a (alts (block (group b (alts (block (group x)))) (group d))))
    (group something (op +))
    (group more stuff)
    (group something (block (group more stuff)))
    (group
     define
     (alts
      (block (group fib (parens (group 0)) (op =) 0))
      (block (group fib (parens (group 1)) (op =) 1))
      (block
       (group fib (parens (group n)) (op =))
       (group
        fib
        (parens (group n (op -) 1))
        (op +)
        fib
        (parens (group n (op -) 2))))))
    (group
     define
     (alts
      (block (group fib (parens (group 0)) (op =) 0))
      (block (group fib (parens (group 1)) (op =) 1))
      (block
       (group
        fib
        (parens (group n))
        (op =)
        fib
        (parens (group n (op -) 1))
        (op +)
        fib
        (parens (group n (op -) 2))))
      (block (group more))))
    (group
     nonsense
     (block
      (group
       hello
       (alts
        (block (group there 4.5))
        (block
         (group more f (parens (group 8)))
         (group
          next
          (parens
           (group
            ok
            (alts
             (block (group stuff))
             (block
              (group
               stuff
               (block
                (group
                 (parens
                  (group begin (block (group more) (group things)))
                  (group separately)))
                (group again))))))))
         (group something))))))
    (group q (block (group x) (group y)))
    (group z)
    (group q (block (group x) (group y)))
    (group z)
    (group q (block (group w (block (group x) (group y)))))
    (group z)
    (group x (op +) w (alts (block (group z (block (group (block (group y))))))))
    (group
     let
     (alts
      (block (group x (op =) 8))
      (block
       (group
        y
        (op =)
        9
        (block (group 10) (group let (alts (block (group x (op =) 8)))))))
      (block
       (group
        y
        (op =)
        9
        (block
         (group 10)
         (group show (parens (group x (op +) y)))
         (group x (op -) y))))))
    (group
     letrec_syntax_and_values
     (block
      (group
       (parens
        (group m (op =) (parens (group syntax_rules (op ....))))
        (group n (op =) (parens (group syntax_rules (op ....)))))
       (block
        (group (parens (group x (op =) 10) (group y (op =) 12)))
        (group (op =>) body)
        (group more)))))
    (group
     x
     something
     (alts
      (block (group a) (group y (block (group w (block (group q))))))
      (block (group c) (group z) (group (op +&) b))))
    (group
     x
     (alts
      (block (group indentize) (group y) (group z))
      (block (group indentize (block (group y))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0) (group (block (group 0))))
        (block (group 1) (group (block (group 1))))
        (block
         (group n)
         (group
          (block
           (group fib (parens (group n (op -) 1)))
           (group (op +) fib (parens (group n (op -) 2)))
           (group more))))))))
    (group
     begin
     (block
      (group
       dictionary
       (op =)
       (brackets
        (group "foo" (block (group 17)))
        (group "bar" (block (group "string")))
        (group "baz" (block (group #t)))))))
 (group
  cond
  (alts
   (block (group null (block (group 0))))
   (block (group list (parens (group x)) (block (group 1))))
   (block
    (group
     cons
     (parens (group a) (group d))
     (block (group f (parens (group a)) (op +) f (parens (group d))))))))))

(define input3
#<<INPUT
@(a, b)
@(a, b)(0)

@[7]
@{9}
@[7]{8, 10 @"more"}
then @[7]{8}
then @{8}

@apple{one}{two}
@banana(0){three}{four}{five}
@coconut{six} {seven}
@donut+eight{nine}
@and{@eggplant. ten{eleven}}

@none{}
@«5»(3){yohoo @9(a, b, c){
               this is plain text
               inside braces}
        0
        }

@«bracketed»("apple")
@«{bracketed}»("apple")

@«1 2 3»: 5
@«1 2 3»{data}: 5

@x|{hello}there}|
@x|(<[{hello}there}]>)|
@x|(<[{hello}|(<[@6 there}]>)|

@x{  2
  1
    4
   5  }

@{4 @//{ this is a comment! }
  5 @// line comment
  6}

@itemlist(@item{x
                y},
          @item{z
                w})

@(in #{s-exp} mode)
@{also in @(#{s-exp}) mode}

@elem{@a()@b()}
@elem{@a{}@b()}

@(«alpha beta»)(gamma){delta}
@(«alpha beta»){delta}
@(«alpha beta»)

@same x @group
@same @group
@same(1) x @group(2)
@same(1) @group(2)

The end
INPUT
  )

(define expected3
  '(top
    (group (parens (group a) (group b)))
    (group (parens (group a) (group b)) (parens (group 0)))
    (group (brackets (group 7)))
    (group (parens (group (brackets (group "9")))))
    (group (brackets (group 7)) (parens (group (brackets (group "8, 10 ") (group "more")))))
    (group then (brackets (group 7)) (parens (group (brackets (group "8")))))
    (group then (parens (group (brackets (group "8")))))
    (group apple (parens (group (brackets (group "one")))
                         (group (brackets (group "two")))))
    (group banana (parens (group 0)
                          (group (brackets (group "three")))
                          (group (brackets (group "four")))
                          (group (brackets (group "five")))))
    (group coconut (parens (group (brackets (group "six"))))
           (braces (group seven)))
    (group donut (op +) eight (parens (group (brackets (group "nine")))))
    (group and (parens (group (brackets
                               (group eggplant)
                               (group ". ten{eleven}")))))
    
    (group none (parens (group (brackets))))
    (group
     5
     (parens
      (group 3)
      (group
       (brackets
        (group "yohoo ")
        (group
         9
         (parens
          (group a)
          (group b)
          (group c)
          (group (brackets (group "this is plain text") (group "\n") (group "inside braces")))))
        (group "\n")
        (group "0")))))
    (group bracketed (parens (group "apple")))
    (group (braces (group bracketed)) (parens (group "apple")))
    (group 1 2 3 (block (group 5)))
    (group 1 2 3 (parens (group (brackets (group "data")))) (block (group 5)))
    (group x (parens (group (brackets (group "hello}there")))))
    (group x (parens (group (brackets (group "hello}there")))))
    (group x (parens (group (brackets (group "hello}") (group 6) (group " there")))))
    (group
     x
     (parens
      (group
       (brackets (group "  2") (group "\n") (group "1") (group "\n") (group "  ") (group "4") (group "\n") (group " ") (group "5  ")))))
    (group
     (parens
      (group (brackets (group "4 ") (group "\n") (group "5 ") (group "  6")))))
    (group
     itemlist
     (parens
      (group
       item
       (parens (group (brackets (group "x") (group "\n") (group "y")))))
      (group
       item
       (parens (group (brackets (group "z") (group "\n") (group "w")))))))
    (group (parens (group in s-exp mode)))
    (group
     (parens
      (group
       (brackets
        (group "also in ")
        (group (parens (group s-exp)))
        (group " mode")))))
    (group
     elem
     (parens
      (group
       (brackets
        (group a (parens))
        (group b (parens))))))
    (group
     elem
     (parens
      (group
       (brackets
        (group a (parens (group (brackets))))
        (group b (parens))))))
    (group alpha beta (parens (group gamma)) (braces (group delta)))
    (group alpha beta (braces (group delta)))
    (group alpha beta)
    (group same x group)
    (group same group)
    (group same (parens (group 1)) x group (parens (group 2)))
    (group same (parens (group 1)) group (parens (group 2)))
    (group The end)))

(define input4
#<<INPUT
#//
gone:
  sub
here:
  sub

block:
#//
  gone
  here

block:
  #// gone
  here

(#// gone,
 here)
(#// gone:
  too,
 here)
(#//
 gone,
 here)
(here,
 #//
 gone)
(here,
#//
 gone)
(here,
 #// gone)

cond
| #// gone
  here
cond
| 
  #// gone
  here
cond
| 
  #//
  gone
  here
| 
#//
  gone
  here

cond #// | gone | here
cond | here #// | gone
cond #// | gone
         | here
cond | #// gone; here | here

{
  hello:
    val x: f(1, 2 + 3)
    match x
    | 1: 'one'
    | 2: 'two'
}

{
  hello:
    val x:
      #//
      g(-1)
      f(
        #//
        0,
        1,
        2 + 3,
        #//
        4 + 5)
    #//
    not included in the code
    match x
    #//
    | 0: no
    | 1: 'one'
    #//
    | 1.5: no
    | 2: 'two'
    #//
    | 3: no,
  #//
  goodbye:
    the enclosing group of the block is commented out
}

{
  hello:
    val x:
      #// g(-1)
      f(#// 0, 1, 2 + 3, #// 4 + 5)
    #// not included in the code
    match x #// | 0: no | 1: 'one' #// | 1.5: no
                | 2: 'two' #// | 3: no,
  #// goodbye:
    the enclosing group of the block is commented out
}
INPUT
)

(define expected4
  '(top
    (group here (block (group sub)))
    (group block (block (group here)))
    (group block (block (group here)))
    (group (parens (group here)))
    (group (parens (group here)))
    (group (parens (group here)))
    (group (parens (group here)))
    (group (parens (group here)))
    (group (parens (group here)))
    (group cond (alts (block (group here))))
    (group cond (alts (block (group here))))
    (group cond (alts (block (group here)) (block (group here))))
    (group cond (alts (block (group here))))
    (group cond (alts (block (group here))))
    (group cond (alts (block (group here))))
    (group cond (alts (block (group here)) (block (group here))))
    (group
     (braces
      (group
       hello
       (block
        (group val x (block (group f (parens (group 1) (group 2 (op +) 3)))))
        (group match x (alts (block (group 1 (block (group (quotes (group one)))))) (block (group 2 (block (group (quotes (group two))))))))))))
    (group
     (braces
      (group
       hello
       (block
        (group val x (block (group f (parens (group 1) (group 2 (op +) 3)))))
        (group match x (alts (block (group 1 (block (group (quotes (group one)))))) (block (group 2 (block (group (quotes (group two))))))))))))
    (group
     (braces
      (group
       hello
       (block
        (group val x (block (group f (parens (group 1) (group 2 (op +) 3)))))
        (group match x (alts (block (group 1 (block (group (quotes (group one)))))) (block (group 2 (block (group (quotes (group two))))))))))))))

(define input5
#<<INPUT
1
1.
1.0
1.0e1
1.e1
1.0e+1
1.0e-1
-1.0e-1

1.2
1 .2
1 . 2
1.+2
1.=2
1..2
1.2..

1 + 2
1+ 2
1 + -2
1 -2
1 - 2
1 +- 2
1 -+ 2
1 + +2
1 ++ 2
INPUT
)

(define expected5
  '(top
    (group 1)
    (group 1.0)
    (group 1.0)
    (group 10.0)
    (group 10.0)
    (group 10.0)
    (group 0.1)
    (group -0.1)
    (group 1.2)
    (group 1 0.2)
    (group 1 (op |.|) 2)
    (group 1.0 (op +) 2)
    (group 1 (op .=) 2)
    (group 1 (op ..) 2)
    (group 1.2 (op ..))
    (group 1 (op +) 2)
    (group 1 (op +) 2)
    (group 1 (op +) -2)
    (group 1 -2)
    (group 1 (op -) 2)
    (group 1 (op +) (op -) 2)
    (group 1 (op -) (op +) 2)
    (group 1 (op +) 2)
    (group 1 (op ++) 2)))

(define input6
#<<INPUT

1 // comment on same line

2
  /* comment weidly indented */

begin:
 1+2
 // comment sticks with block
 // and this one, too

 // and even this one

(begin:
   x
   // block
 // inner comment
 ) // after comment

INPUT
)

(define expected6
  '(top
    (group 1)
    (group 2)
    (group begin (block (group 1 (op +) 2)))
    (group (parens (group begin (block (group x)))))))

(define input7
#<<INPUT

'apple'
'apple + banana'
'apple; banana'
'apple
 banana'
':
   apple
   banana'
'(a)'
'(a 'nested' b)'
'«a 'nested' b»'
'  « a 'nested' b »  '
' /* x */ « a 'nested' b » 
 '

INPUT
)

(define expected7
  '(top
    (group (quotes (group apple)))
    (group (quotes (group apple (op +) banana)))
    (group (quotes (group apple) (group banana)))
    (group (quotes (group apple) (group banana)))
    (group (quotes (group (block (group apple) (group banana)))))
    (group (quotes (group (parens (group a)))))
    (group (quotes (group (parens (group a (quotes (group nested)) b)))))
    (group (quotes (group a (quotes (group nested)) b)))
    (group (quotes (group a (quotes (group nested)) b)))
    (group (quotes (group a (quotes (group nested)) b)))))
