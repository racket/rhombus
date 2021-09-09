#lang racket/base
(require "../parse.rkt"
         racket/pretty)

(define input1
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
 | fib(n): (fib(n-1) // in parens => `+` continues this line
            + fib(n-2))

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

define fib(n):
  match n { | 0: { 0 }
            | 1: { 1 }
            | n: { fib(n-1) + fib(n-2) } }

define fib(n):
  match n { | { 0: 0 } | { 1: 1 } | n: fib(n-1) + fib(n-2) }

define fib(n): match n { | { 0: 0 } | { 1: 1 } | n: fib(n-1) + fib(n-2) }

define fib(n): { match n { | { 0: 0 } | { 1: 1 } | n: fib(n-1) + fib(n-2) } }

define fib(n): { match n { | 0: {0} | 1: {1} | n: { fib(n-1) + fib(n-2) } } }

define fib(n): { match n { | { 0: {0} } | { 1: {1} } | { n: { fib(n-1) + fib(n-2) } } } }

define fib(n): { match n | { 0: {0} } | { 1: {1} } | { n: { fib(n-1) + fib(n-2) } } }

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

define exp(n :: Integer, 'base': base = 2.718281828459045):
  if (n == 1)
   | base
   | base * exp(n-1, 'base': base)

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
INPUT
)

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
          (parens
           (group
            fib
            (parens (group n (op -) 1))
            (op +)
            fib
            (parens (group n (op -) 2))))))))))
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
      (group in (block (group x (op +) y)))))))

  
(define input2
#<<INPUT

// A set of examples to see what happens with verious forms,
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
          next (ok |
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
              & b

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
             (block)
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
      (block (group c) (group z) (group (op &) b))))
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

(define (check input expected)
  (let ([in (open-input-string input)])
    (port-count-lines! in)
    (define parsed (syntax->datum (parse-all in)))
    (unless (equal? expected parsed)
      (define (out name parsed)
        (define path (build-path (find-system-path 'temp-dir) name))
        (printf "~a\n" path)
        (call-with-output-file*
         path
         #:exists 'truncate
         (lambda (o) (pretty-write parsed o))))
      (out "expected" expected)
      (out "parsed" parsed)
      (error "failed"))))

(check input1 expected1)
(check input2 expected2)
