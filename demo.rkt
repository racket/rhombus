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

operator (x mod y):
  x - floor(x / y) * y

10 mod 3  // prints 1

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

// an identifier macro

expr.macro ?just_five: ?"five"

just_five +$ " is the result"

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
  match bind_ct.unpack(a)
   | ?(¿a_id, ¿a_info, (¿a_bind_info ...), ¿a_matcher, ¿a_binder, ¿a_data):
       match bind_ct.unpack(b)
        | ?(¿b_id, ¿b_info, (¿b_bind_info ...), ¿b_matcher, ¿b_binder, ¿b_data):
            bind_ct.pack(?(pair,
                           (),
                           (¿a_bind_info ... ¿b_bind_info ...),
                           build_reverse_cons_match,
                           build_reverse_cons_bind,
                           (¿a, ¿b, a_part, b_part)))

bind.matcher ?(build_reverse_cons_match(¿in_id, (¿a, ¿b, ¿a_part_id, ¿b_part_id),
                                        ¿IF, ¿success, ¿fail)):
  match bind_ct.unpack(a)
   | ?(¿a_id, ¿a_info, ¿a_bind_infos, ¿a_matcher, ¿a_binder, ¿a_data):
       match bind_ct.unpack(b)
        | ?(¿b_id, ¿b_info, ¿b_bind_infos, ¿b_matcher, ¿b_binder, ¿b_data):
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
  match bind_ct.unpack(a)
   | ?(¿a_id, ¿a_info, ¿a_bind_infos, ¿a_matcher, ¿a_binder, ¿a_data):
       match bind_ct.unpack(b)
        | ?(¿b_id, ¿b_info, ¿b_bind_infos, ¿b_matcher, ¿b_binder, ¿b_data):
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

ILine.p1(l1).x
(l1.p1 :: IPosn).x

fun (p): (p :: IPosn).x

{
  val ILine(p1, p2): l1
  p1.x + p2.y
}

// function result contracts

fun add1(x) :: Integer:
  match x
   | n :: Integer : x + 1
   | 'else': x

add1(100)
// add1("oops")

fun
 | add_two(x) :: Number:
     x + 2.0
 | add_two(x, y) :: String:
     x +$ " and " +$ y

add_two(7) == 9.0
add_two(6, 7) === "6 and 7"

{
  val f: fun (x) :: Integer: x
  f(10)
}

fun on_diag(n :: Integer) :: Posn:
  Posn(n, n)

on_diag(1).x

val known_posn: on_diag(2)
known_posn.x

// contracts and dot providers

import:
  = racket/base:
    only: atan

contract.macro ?AlsoPosn: ?Posn
Posn(1, 2) :: AlsoPosn  // prints Posn(1, 2)

bind.macro ?(AlsoPosn (¿x, ¿y) ¿tail ...):
  values(?(Posn(¿x, ¿y)), tail)
                       
contract.macro ?Vector:
  contract_ct.pack_predicate(?(fun (x): x is_a Posn),
                             ?((¿(dot_ct.provider_key), vector_dot_provider)))


dot.macro ?(vector_dot_provider ¿left ¿dot ¿right):
  match right
   | ?angle: ?(vector_angle(¿left))
   | ?magnitude: ?(vector_magnitude(¿left))

fun vector_angle(Posn(x, y)): atan(y, x)
fun vector_magnitude(Posn(x, y)): sqrt(x*x + y*y)

let vec :: Vector: Posn(3, 4)
vec.angle
vec.magnitude

def AlsoPosn(also_x, also_y): Posn(10, 20)
also_x +$ "," +$ also_y

expr.macro ?(or_zero ¿p ¿tail ...):
  values(static_info_ct.wrap(?(¿p || Posn(0,0)),
                             ?((¿(dot_ct.provider_key),
                                vector_dot_provider))),
         tail)
  
or_zero(Posn(3, 4)).magnitude

fun zero_vec(): Posn(0, 0)
static_info.macro ?zero_vec: ?((¿(expr_ct.call_result_key),
                                ¿(static_info_ct.pack(?((¿(dot_ct.provider_key),
                                                         vector_dot_provider))))))
zero_vec().magnitude

// indexables

val nums: [1, 2, 3]

nums[1]

val nums_a: Array([1, 2, 3])

nums_a[1]
nums_a[2] = 30
nums_a[2]

val map: Map["x" = "hello", "y" = "goodbye"]

map
map["y"]

val also_map: Map(1, "one", 2, "two")
also_map[2]

val mut_map: make_map(1, "mone")
mut_map[1]
mut_map[2] = "mtwo"
mut_map[2]

def [x, ys, ...]: nums
ys

def Array(ax, ay, az): nums_a
az

def local_map: Map["alice home" = Posn(4, 5),
                   "bob home" = Posn(7, 9)];

def lookup(who):
  def Map[who +$ " home" = Posn(who_x, who_y)]: local_map
  who_x +$ ", " +$ who_y

lookup("alice")

def [ps :: Posn, ...] : [Posn(1, 2), Posn(3, 4)]
ps[0].x

// rest arguments

fun f_rest(x, ys :: Integer, ...):
  ys

f_rest(10, 11, 12, 13)

fun
 | g_rest(): "no"
 | g_rest(x :: Integer): "simple"
 | g_rest(x :: Integer, ys :: Integer, ...):
    ys
 | g_rest(x): x

g_rest()
g_rest(1) === "simple"
g_rest(1, 2, 3)
g_rest("hello") === "hello"

fun
 | posns_y(ps :: Posn, ...):
     ps[1].y
 | posns_y(x):
     x

posns_y(Posn(1, 2), Posn(3, 4), Posn(5, 6))
posns_y(10)

// `matching` contracts

fun get_pt_x(pt :: matching(Posn(_, _))):
  pt.x

get_pt_x(Posn(1, 2))

fun get_pts_x(pts :: matching([Posn(_, _), ...])):
  pts[0].x

get_pts_x([Posn(1, 2)])

fun nested_pt_x(pt :: matching(Posn(Posn(_, _), _))):
  pt.x.x

contract.macro ?(ListOf (¿contract ...) ¿tail ...):
  values(?(matching([_ :: (¿contract ...), ¿(? ...)])),
         tail)

fun get_pts_x2(pts :: ListOf(Posn)):
  pts[0].x

get_pts_x2([Posn(5, 7)])
