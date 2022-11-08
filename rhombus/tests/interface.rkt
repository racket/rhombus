#lang rhombus
import: "check.rhm" open

use_static

check:
  interface Shape:
    unimplemented area
    method ten_area(): 10 * area()

  interface Polygon:
    extends Shape
    unimplemented sides
    method has_corners(): #true

  interface Circle:
    extends Shape
    method has_sides(): #false

  class ApproxCircle():
    implements: Polygon Circle
    override area(): 33
    override sides(): 100

  val a: ApproxCircle()

  [a.area(),
   a.sides(),
   a.has_corners(),
   a.has_sides(),
   
   (a -: Polygon).has_corners(),
   (a -: Circle).has_sides(),

   (a -: Shape).area(),
   (a -: Polygon).area(),
   (a -: Circle).area(),

   a.ten_area()]

  [33, 100, #true, #false,
   #true, #false,
   33, 33, 33,
   330]

// conflict
check:
  ~eval_exn
  interface Shape:
    method draw(): "circle"
  interface Cowboy:
    method draw(): "bang"
  class LoneRanger():
    implements Shape
    implements Cowboy
  "method supplied by multiple superinterfaces"

// diamond
check:
  interface Shape:
    unimplemented draw
  interface Cowboy:
    unimplemented draw
  class LoneRanger():
    implements Shape
    implements Cowboy
    override draw(): 10
  LoneRanger().draw()
  10

// diamond with extra methods
check:
  interface Shape:
    unimplemented draw
  interface Circle:
    extends Shape
    method cdraw(): draw() +& "_circle"
  interface Square:
    extends Shape
    method sdraw(): draw() +& "_square"
  class Squircle():
    implements Circle
    implements Square
    override draw(): "squircle"
  [Squircle().cdraw(),
   Squircle().sdraw()]
  ["squircle_circle",
   "squircle_square"]

// super call in interface
check:
  interface Shape:
    method draw(): "shape"
  interface Circle:
    extends Shape
    override draw(): "circle " +& super.draw()
  class Oval():
    implements Circle
  Oval().draw()
  "circle shape"
