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
