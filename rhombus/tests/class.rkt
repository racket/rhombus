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
    nonfinal
  class Posn3D(x):
    extends Posn
  "field name already exists in superclass"

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
