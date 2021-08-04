#lang rhombus

// Some expressions

10 * (-3) + 2
10 + (-3) * 2

{
  #false || !(#false || #false)
}

"hello" +$ " " +$ "world"

// define and call a function

def five(x):
  5

3*five(#true && #false || 2 < 3)-2

def six(x, 'plus': amt = 0):
  6 + amt

six("anything")
six("anything", 'plus': 7)

// pattern-matching on a function argument

def f(cons(x :: Integer, y)): cons(x + 1, y)

f(cons(22, #false))

// structures

struct Posn(x, y)

def md(p :: Posn):
  p.x + p.y

Posn.x(Posn(1, 4))

md(Posn(1, 4))

// more definitions

def π: 3.14
π

def (((ππ))): π * π
ππ

def (ma, mb, mc):
  values("1", "2", "3")
mb
  
val cons(ca, cb): cons(1, 2)
ca

def
 | size(n :: Integer):
     n
 | size(p ::Posn):
     p.x + p.y
 | size(a, b):
     a+b

size(Posn(8, 6))
size(1, 2)

def Posn(px, py) :: Posn: Posn(1, 2)
cons(px, py)

def identity: fun (x): x
identity(1 + fun (x) { x } (99) )

size

// quasiquoting an expression

?(apple + banana)
?(apple + ¿(3 + 4))

// defining an infix operator

operator (a +* b):
  (a + b) * b

3 +* 4

// with precedence and associativity

operator (a ++* b):
  'weaker_than': *
  'associativity': 'right'
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

fun
 | factorial(0): 1
 | factorial(n): n*factorial(n-1)
         
expr.macro ?(¿a *! ¿tail ...):
  values(?(factorial(¿a)), tail)

10*!

// ?? is an alias for ¿

expr.macro ?(??a **! ??tail ...):
  values(?(factorial(??a)), tail)

10**!


// a macro with an identifier name that does a weird
// thing with the result tail

expr.macro ?(prefix_plus ¿a ¿b ¿c ...):
  values(a, ?(+ ¿b ¿c ...))

prefix_plus 7 9

// another way to write that

def ?(also_prefix_plus ¿e ...):
  match e
   | ?(¿a ¿b ¿c ...):
       values(a, ?(+ ¿b ¿c ...))
   | 'else':
       values(?"this is terrible error reporting", ?())

also_prefix_plus 7 9

// define a binding operator

bind.operator ?($ ¿n):
  ?(¿n :: Integer)

fun apply_interest($ n):
  n * 1.05

apply_interest(7)

// define <> as revese-cons pattern
bind.operator ?(¿a <> ¿b):
  pack_binding(?(pair,
                 build_reverse_cons_match,
                 build_reverse_cons_bind,
                 (¿a, ¿b, a_part, b_part)))

bind.matcher ?(build_reverse_cons_match(¿in_id, (¿a, ¿b, ¿a_part_id, ¿b_part_id),
                                        ¿IF, ¿success, ¿fail)):
  match unpack_binding(a)
   | ?(¿a_id, ¿a_matcher, ¿a_binder, ¿a_data):
       match unpack_binding(b)
        | ?(¿b_id, ¿b_matcher, ¿b_binder, ¿b_data):
            ?{
              // check for pair an extract reversed pieces
              val (is_match, ¿a_part_id, ¿b_part_id):
                match ¿in_id
                 | cons(¿b_id, ¿a_id):
                     values(#true, ¿a_id, ¿b_id)
                 | 'else':
                     values(#false, #false, #false)
              // if a match, chain to a and b matchers
              ¿IF is_match
               | ¿a_matcher(¿a_part_id,
                            ¿a_data,
                            ¿IF,
                            ¿b_matcher(¿b_part_id,
                                       ¿b_data,
                                       ¿IF,
                                       ¿success,
                                       ¿fail),
                            ¿fail)
               | ¿fail
            }

bind.binder ?(build_reverse_cons_bind(¿in_id, (¿a, ¿b, ¿a_part_id, ¿b_part_id))):
  match unpack_binding(a)
   | ?(¿a_id, ¿a_matcher, ¿a_binder, ¿a_data):
       match unpack_binding(b)
        | ?(¿b_id, ¿b_matcher, ¿b_binder, ¿b_data):
            ?{
              ¿a_binder(¿a_part_id, ¿a_data)
              ¿b_binder(¿b_part_id, ¿b_data)
            }

// an expression operator that's consistent with the pattern
expr.operator ?(¿a <> ¿b): ?(cons(¿b, ¿a))

def rx <> (ry :: Integer) : "2" <> 1
rx

// definition form, which returns either a block of definitions
// or a block of definitions and a sequence of expressions

defn.macro ?(define_eight ¿e ...):
  match e
  | ?(¿name):
      ?{def ¿name: 8}

define_eight ate
ate

defn.macro ?(define_and_use ¿e ...):
  match e
  | ?(¿name {¿rhs ...}):
      ?{def ¿name {¿rhs ...}
        ?(¿name)}

define_and_use nine: 1+8
nine

// declaration form

decl.macro ?(empty_import ¿e ...):
  match e
  | ?():
      ?{import:}

empty_import

// `forward` is a definition that is only visible later

def check_later():
  ok_later

let accum: 1
let accum: accum+1
let accum: accum+1
accum

def ok_later: "ok"
check_later()

// Nested structure types with contracts

struct IPosn(x :: Integer, y :: Integer)

struct ILine(p1 :: IPosn, p2 :: IPosn)

def l1: ILine(IPosn(1, 2), IPosn(3, 4))

l1.p2.x

IPosn(1, 2).x
