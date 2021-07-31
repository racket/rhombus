#lang rhombus

function

// Some expressions

10 * (-3) + 2
10 + (-3) * 2

{
  #false || !(#false || #false)
}

"hello" +$ " " +$ "world"

// define and call a function

define five(x):
  5

3*five(#true && #false || 2 < 3)-2

define six(x, 'plus': amt = 0):
  6 + amt

six("anything")
six("anything", 'plus': 7)

// pattern-matching on a function argument

define f(cons(x :: Integer, y)): cons(x + 1, y)

f(cons(22, #false))

// structures

struct Posn(x, y)

define md(p :: Posn):
  p.x + p.y

Posn.x(Posn(1, 4))

md(Posn(1, 4))

// more definitions

define π: 3.14
π

define (((ππ))): π * π
ππ

define (ma, mb, mc):
  values("1", "2", "3")
mb
  
value cons(ca, cb): cons(1, 2)
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

operator (a +* b):
  (a + b) * b

3 +* 4

// with precedence and associativity

operator (a ++* b,
          'weaker_than': *,
          'associativity': right):
  (a + b) * b

3 ++* 4 * 2 ++* 5
3 ++* ((4 * 2) ++* 5)

// defining a prefix operator

operator (!! b):
  ! ! b

!!#true

// defining an operator that is both prefix and infix

operator
 | (** exponent):
     2 ** exponent
 | (base ** exponent):
     if exponent == 0
     | 1
     | base * (base ** (exponent-1))

3 ** 8
** 10 // = 2 ** 10

// match

match cons(7, 8)
 | cons(a, b):
     b
 | x:
     x
 | 'else':
     "other"

match ?(z + y, {[10, 11, 12]})
 | ?(x ¿a): a
 | ?(¿a + y, {[¿n, ...]}): cons(a, n)

cond
 | #true: 17
 | 'else': 18

// postfix as a macro "infix" operator;

function
 | factorial(0): 1
 | factorial(n): n*factorial(n-1)
         
expression_macro ?(¿a *! ¿tail ...):
  values(?(factorial(¿a)), tail)

10*!

// ?? is an alias for ¿

expression_macro ?(??a **! ??tail ...):
  values(?(factorial(??a)), tail)

10**!


// a macro with an identifier name that does a weird
// thing with the result tail

expression_macro ?(prefix_plus ¿a ¿b ¿c ...):
  values(a, ?(+ ¿b ¿c ...))

prefix_plus 7 9

// another way to write that

define ?(also_prefix_plus ¿e ...):
  match e
   | ?(¿a ¿b ¿c ...):
       values(a, ?(+ ¿b ¿c ...))
   | 'else':
       values(?"this is terrible error reporting", ?())

also_prefix_plus 7 9

// define a binding operator

binding_operator ?($ ¿n):
  ?(¿n :: Integer)

function apply_interest($ n):
  n * 1.05

apply_interest(7)

// define <> as revese-cons pattern
binding_operator ?(¿a <> ¿b):
  match unpack_binding(a)
   | ?((¿a_id), ¿a_pred, {¿a_def; ...}):
       match unpack_binding(b)
        | ?((¿b_id), ¿b_pred, {¿b_def; ...}):
            pack_binding(?((¿a_id ¿b_id),
                           function(v):
                             match v
                              | cons(bv, av):
                                  define values(is_a_match, ar):
                                    ¿a_pred(av)
                                  if is_a_match
                                   | define values(is_b_match, br):
                                       ¿b_pred(bv)
                                     if is_b_match
                                      | values(#true, ar, br)
                                      | values(#false, #false, #false)
                                   | values(#false, #false, #false)
                              | 'else': values(#false, #false, #false),
                           {¿a_def; ...; ¿b_def; ...}))

// an expression operator that's consistent with the pattern
expression_operator ?(¿a <> ¿b): ?(cons(¿b, ¿a))

define rx <> (ry :: Integer) : "2" <> 1
rx

// definition form, which returns either a block of definitions
// or a block of definitions and a sequence of expressions

definition_macro ?(define_eight ¿e ...):
  match e
  | ?(¿name):
      ?{define ¿name: 8}

define_eight ate
ate

definition_macro ?(define_and_use ¿e ...):
  match e
  | ?(¿name {¿rhs ...}):
      ?{define ¿name {¿rhs ...}
        ?(¿name)}

define_and_use nine: 1+8
nine

// declaration form

declaration_macro ?(empty_require ¿e ...):
  match e
  | ?():
      ?{require:}

empty_require

// `forward` is a definition that is only visible later

define check_later():
  ok_later

forward accum: 1
forward accum: accum+1
forward accum: accum+1
accum

define ok_later: "ok"
check_later()
