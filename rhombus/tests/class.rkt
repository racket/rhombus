#lang rhombus
import: "check.rhm" open

use_static
  
check:
  class Posn(x, y)
  val p: Posn(1, 2)
  [p is_a Posn, p.x, p.y]
  [#true, 1, 2]

check:
  ~eval_exn
  class Posn(x, y)
  class Posn3D(z):
    extends Posn
  "superclass is final"

check:
  ~eval_exn
  class Posn(x, x)
  "duplicate field name"

check:
  ~eval_exn
  class Posn(x, y):
    field x: 0
  "duplicate field name"

check:
  ~eval_exn
  class Posn(x, y):
    nonfinal
  class Posn3D(x):
    extends Posn
  "field name already exists in superclass"

check:
  class Posn(~x, y)
  val p: Posn(~x: 1, 2)
  val p2: Posn(2, ~x: 1)
  [p.x, p.y, p2.x, p2.y]
  [1, 2, 1, 2]

check:
  class Posn(~x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
  val p: Posn3D(~x: 1, 2, 3)
  val p2: Posn3D(2, 3, ~x: 1)
  [p.x, p.y, p.z, p2.x, p2.y, p2.z]
  [1, 2, 3, 1, 2, 3]

check:
  class Posn(x, y):
    nonfinal
  class Posn3D(~z):
    extends Posn
  val p: Posn3D(1, 2, ~z: 3)
  val p2: Posn3D(~z: 3, 1, 2)
  [p.x, p.y, p.z, p2.x, p2.y, p2.z]
  [1, 2, 3, 1, 2, 3]

check:
  class Posn(~x, y):
    nonfinal
  class Posn3D(~z: zz):
    extends Posn
  val p: Posn3D(~x: 1, 2, ~z: 3)
  val p2: Posn3D(~z: 3, 2, ~x: 1)
  [p.x, p.y, p.zz, p2.x, p2.y, p2.zz]
  [1, 2, 3, 1, 2, 3]

check:
  class Posn(x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
  val p: Posn3D(1, 2, 3)
  [p is_a Posn, p is_a Posn3D, p.x, p.y, p.z,
   Posn(1, 2) is_a Posn3D]
  [#true, #true, 1, 2, 3,
   #false]

check:
  class Posn(x, y):
    constructor (make):
      fun (z):
        make(z+1, z-1)
  val p: Posn(1)
  [p.x, p.y]
  [2, 0]

check:
  class Posn(x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
    constructor (make):
      fun (z):
        make(z+1, z+2)(z+3)
  val p: Posn3D(1)
  [p.x, p.y, p.z]
  [2, 3, 4]

check:
  class Posn(x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
  class Posn4D(w):
    extends Posn3D
    constructor (make):
      fun (z):
        make(z+1, z+2, z+3)(z+4)
  val p: Posn4D(1)
  [p.x, p.y, p.z, p.w]
  [2, 3, 4, 5]

check:
  class Posn(x, y):
    nonfinal
    constructor (make):
      fun (y, x):
        make(x, y)
  class Posn3D(z):
    extends Posn
    constructor (make):
      fun (z):
        make(z+1, z+2)(z+3)
  val p: Posn3D(1)
  [p.x, p.y, p.z]
  [3, 2, 4]

check:
  class Posn(x, y):
    nonfinal
    constructor (make):
      fun (y, x):
        make(x, y)
  class Posn3D(z):
    extends Posn
    constructor (make):
      fun (z):
        make(z+1, z+2)(z+3)
  class Posn4D(w):
    extends Posn3D
    constructor (make):
      fun (z):
        make(z+1)(z+5)
  val p: Posn4D(1)
  [p.x, p.y, p.z, p.w]
  [4, 3, 5, 6]

check:
  class Posn(x, y):
    nonfinal
    constructor (make):
      fun
      | (~x: x, ~y: y):
          make(x, y)
      | (x):
          make(x, 0)
  class Posn3D(z):
    extends Posn
    constructor (make):
      fun
      | (z):
          make(z+1)(z+3)
      | (x, y, z):
          make(~x: x, ~y: y)(z)
  val p: Posn3D(1)
  val p2: Posn3D(10, 20, 30)
  [p.x, p.y, p.z,
   p2.x, p2.y, p2.z]
  [2, 0, 4,
   10, 20, 30]

check:
  use_dynamic
  class Posn(x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
  val p: dynamic(Posn3D(1, 2, 3))
  [p.x, p.y, p.z]
  [1, 2, 3]

check:
  import rhombus/meta open
  class Posn(x, y):
    nonfinal
    internal _Posn
    // external view flips `y` and `x`
    constructor:
      fun (y, x):
        _Posn(x, y)
    binding:
      rule
      | 'Posn($y, $x)':
          '_Posn($x, $y)'
      | 'Posn($v)':
          '_Posn($v, 0)'
    annotation:
      rule
      | 'Posn.of($x, $y)':
          '_Posn.of($y, $x)'
      | 'Posn': '_Posn'    
  val p: Posn(0, 2)
  val Posn(yy, xx): p
  val Posn(a): p    
  [p.y, p.x, yy, xx, a,
   p is_a Posn,
   Posn(1, "2") is_a Posn.of(Integer, String)]
  [0, 2, 0, 2, 2,
   #true,
   #true]

check:
  class Posn(x, y):
    nonfinal
    field w: 0
  class Posn3D(z):
    extends Posn
  val p1: Posn(10, 20)
  val p: Posn3D(1, 2, 3)
  [[p1.x, p1.y, p1.w],
   [p.x, p.y, p.w, p.z]]
  [[10, 20, 0],
   [1, 2, 0, 3]]

check:
  class Posn(~x, y):
    nonfinal
    field w: 0
  class Posn3D(z):
    extends Posn
  val p1: Posn(20, ~x: -10)
  val p: Posn3D(2, ~x: 1, 3)
  [[p1.x, p1.y, p1.w],
   [p.x, p.y, p.w, p.z]]
  [[-10, 20, 0],
   [1, 2, 0, 3]]

check:
  class Posn(~x, y):
    nonfinal
    field w: 0
    internal _Posn
  class Posn3D(z):
    extends Posn
    internal _Posn3D
  val p1: Posn(20, ~x: -10)
  val p: Posn3D(2, ~x: 1, 3)
  val ip1: _Posn(20, ~x: -10)
  val ip: _Posn3D(2, ~x: 1, 3)
  fun get(p1 :: Posn, p :: Posn3D):
    [[p1.x, p1.y, p1.w],
     [p.x, p.y, p.w, p.z]]
  [get(p1, p), get(ip1, ip)]
  [[[-10, 20, 0], [1, 2, 0, 3]],
   [[-10, 20, 0], [1, 2, 0, 3]]]

check:
  class Posn(~x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
    constructor (make):
      fun(x, y, z):
        make(~x: 1, y)(z)
  val p2: Posn(~x: 1, 2)
  val p3: Posn3D(1, 2, 3)
  [[p2.x, p2.y], [p3.x, p3.y, p3.z]]
  [[1, 2], [1, 2, 3]]

check:
  class Posn(~x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
    constructor (make):
      fun(x, y, z):
        make(~x: 1, y)(z)
  class Posn4D(~w):
    extends Posn3D
    constructor (make):
      fun(x, y, z, w):
        make(x, y, z)(~w: w)
  val p2: Posn(~x: 1, 2)
  val p3: Posn3D(1, 2, 3)
  val p4: Posn4D(1, 2, 3, 4)
  [[p2.x, p2.y],
   [p3.x, p3.y, p3.z],
   [p4.x, p4.y, p4.z, p4.w]]
  [[1, 2],
   [1, 2, 3],
   [1, 2, 3, 4]]

check:
  class Posn(~x, y):
    nonfinal
    constructor (make):
      fun(~ex: x, ~wy: y):
        make(~x: 1, y)
  class Posn3D(z):
    extends Posn
    constructor (make):
      fun(x, ~y: y, z):
        make(~ex: 1, ~wy: y)(z)
    internal _Posn3D
  class Posn4D(w):
    extends Posn3D
    constructor (make):
      fun(x, y, z, w):
        make(x, ~y: y, z)(w)
  val p2: Posn(~ex: 1, ~wy: 2)
  val p3: Posn3D(1, ~y: 2, 3)
  val _p3: _Posn3D(~x: 1, 2, 3)
  val p4: Posn4D(1, 2, 3, 4)
  [[p2.x, p2.y],
   [p3.x, p3.y, p3.z],
   [_p3.x, _p3.y, _p3.z],
   [p4.x, p4.y, p4.z, p4.w]]
  [[1, 2],
   [1, 2, 3],
   [1, 2, 3],
   [1, 2, 3, 4]]

check:
  class Posn(x, y = 0)
  val p: Posn(1)
  [p.x, p.y]
  [1, 0]

check:
  class Posn(x, y = x):
    nonfinal
  val p: Posn(1)
  [p.x, p.y]
  [1, 1]

check:
  class Posn(~x, y = x):
    nonfinal
  val p: Posn(~x: 1)
  val p2: Posn(~x: -1, 2)
  [p.x, p.y, p2.x, p2.y]
  [1, 1, -1, 2]

check:
  class Posn3D(~x = 10, y, ~z: z = x+y)
  val p: Posn3D(2)
  val p2: Posn3D(2, ~z: 3)
  val p3: Posn3D(2, ~x: 3)
  val p4: Posn3D(~z: 0, 2, ~x: 3)
  [[p.x, p.y, p.z],
   [p2.x, p2.y, p2.z],
   [p3.x, p3.y, p3.z],
   [p4.x, p4.y, p4.z]]
  [[10, 2, 12],
   [10, 2, 3],
   [3, 2, 5],
   [3, 2, 0]]

check:
  ~eval_exn
  class Posn(x = 0, y)
  "without default after"

check:
  ~eval_exn
  class Posn(x, y = 0):
    nonfinal
  class Posn3D(z):
    extends Posn
  "field needs a default"

check:
  ~eval_exn
  class Posn(x, y = no_such_variable)
  "no_such_variable: unbound identifier"

check:
  ~eval_exn
  class Posn(the_x, the_y = 0):
    nonfinal
  class Posn3D(z = the_y):
    extends Posn
  "the_y: unbound identifier"

check:
  ~exn
  class Posn(x :: Integer, y :: Integer)
  Posn("x", 0)
  "value does not match annotation"

check:
  ~exn
  class Posn(mutable x :: Integer, y :: Integer)
  Posn("x", 0)
  "value does not match annotation"

check:
  class Posn(mutable x :: Integer, y :: Integer)
  val p: Posn(2, 0)
  [p.x, p.x := 5, p.x, p.y]
  [2, 5, 5, 0]

check:
  class Posn(x :: Integer, mutable y :: Integer)
  val p: Posn(2, 0)
  [p.y, p.y := 5, p.y, p.x]
  [0, 5, 5, 2]

check:
  ~exn
  class Posn(mutable x :: Integer, y :: Integer)
  Posn("x", 0).x := "oops"
  "value does not match annotation"
