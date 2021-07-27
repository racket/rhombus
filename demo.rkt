#lang rhombus

// Some expressions

10 * (-3) + 2
10 + (-3) * 2

{
 #false || !(#false || #false)
}

// define and call a function

define five(x):
  5

3*five(#true && #false || 2 < 3)-2

define six(x, plus:> amt = 0):
  6 + amt

six("anything")
six("anything", plus:> 7)

// pattern-matching on a function argument

define f(cons(x :: Integer, y)): cons(x + 1, y)

f(cons(22, #false))

// structures

struct Posn(x, y)

define md(p :: Posn):
  p.x + p.y

md(Posn(1, 4))

// more definitions

define π: 3.14
π

define cons(ca, cb): cons(1, 2)
ca

define
| size(n :: Integer):
    n
| size(p ::Posn):
    p.x + p.y
| size(a, b):
    a+b

size(Posn(8, 6))
size(1, 2)

define Posn(px, py) :: Posn: Posn(1, 2)
cons(px, py)

define identity: function (x): x
identity(1 + function (x) { x } (99) )

size

// quasiquoting an expression

?(apple + banana)
?(apple + ¿(3 + 4))

// defining an infix operator

define ?(¿a +* ¿b):
  ?{
     define v: ¿b
     (¿a + v) * v
   }

3 +* 4

// with precedence and associativity

define ?(¿a ++* ¿b,
         weaker_than:> *,
         associativity:> right):
  ?{
     define v: ¿b
     (¿a + v) * v
   }

3 ++* 4 * 2 ++* 5
3 ++* ((4 * 2) ++* 5)

// ?? is an alternate spelling of ¿
define ?(??a & ??b):
  ?(cons(??a, ??b))

1 & 2
  
// defining a prefix operator

define ?(!! ¿b):
  ?(! ! ¿b)

!!#true

// defining an operator that is both prefix and infix,
// using a helper function

define power(base, exponent):
  if exponent == 0
  | 1
  | base * power(base, exponent-1)

define
 | ?(** ¿exponent):
      ?(2 ** ¿exponent)
 | ?(¿base ** ¿exponent):
      ?(power(¿base, ¿exponent))

3 ** 8
** 10 // = 2 ** 10

// match

match cons(7, 8)
 | cons(a, b):
     b
 | x:
     x
 | else:
     "other"

match ?(z + y, {[10, 11, 12]})
 | ?(x ¿a): a
 | ?(¿a + y, {[¿n, ...]}): cons(a, n)

cond
 | #true: 17
 | else: 18

// postfix efect by a transformer "infix" operator

define
 | factorial(0): 1
 | factorial(n): n*factorial(n-1)
         
define ?(¿a *! ¿tail ...):
  values(?(factorial(¿a)), tail)

10*!

// define an expression transformer, which receives
// the rest of the group after the identifier

define ?(prefix_plus ¿e ...):
  match e
  | ?(¿a ¿b ¿c ...):
      values(a, ?(+ ¿b ¿c ...))
  | else:
      values(?"this is terrible error reporting", ?())

prefix_plus 7 9
