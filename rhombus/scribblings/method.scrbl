#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(val method_eval: make_rhombus_eval())

@title(~tag: "methods"){Methods and Constructors}

A @rhombus(method, ~class_clause) clause adds a method to a class. The
method can refer to fieds of the class directory, or it can use
@rhombus(this), which refers to the object whose method is called:

@(demo:
    ~eval: method_eval
    ~defn:
      class Posn(x, y):
        method mdist():
          this.x + y
        method move(dx -: Integer, dy -: Integer):
          if dx == 0 && dy == 0
          | this
          | Posn(x+dx, y+dy)
    ~repl:
      Posn(1, 2).mdist()
      Posn(1, 2).move(0, 0)
      Posn(1, 2).move(10, -10)
  )

In the same way that a class can be used as a namespace to refer to a
field accessor like @rhombus(Posn.x) or @rhombus(Posn.y), it can access
a variant of a method that expects the @rhombus(this) object as an
extra initial argument.

@(demo:
    ~eval: method_eval
    Posn.move(Posn(1, 2), 10, -10)
  )

Methods are inherited in a subclass. Use the
@rhombus(override, ~class_clause) modifier to override a method, and use
@rhombus(super) within a subclass method in place of @rhombus(this) to
call a superclass method.

@(demo:
    ~defn:
      class Posn(x, y):
        nonfinal
        method mdist():
          x + y
        method area():
          0
      class Posn3D(z):
        extends Posn
        override method mdist():
          super.mdist() + z
    ~repl:
      val p: Posn3D(1, 2, 3)
      p
      p.mdist()             
      p.area()             
  )

Other method modifiers includes @rhombus(final, ~class_clause) to
prevent overriding, @rhombus(abstract, ~class_clause) to declare a
method without a body block and require that it is implemented in a
subclass to create a class that can be instantiated. As a shorthand, the
form name @rhombus(method, ~class_clause) can be omitted just after
@rhombus(override, ~class_clause), @rhombus(final, ~class_clause),
or @rhombus(abstract, ~class_clause).

A field in a class can have a keyword, default-value expression, or
both. In that case, the class constructor accepts the argument in
keyword form, makes the argument optional, or both. Keyword fields are
printed with their keywords, too.

@(demo:
    ~defn:
      class Posn(~x: x, ~y: y = x)
    ~repl:
      Posn(~y: 2, ~x: 1)
      Posn(~x: 1)
      val Posn(~y: y1, ~x: x1): Posn(~x: 1, ~y: 2)
      y1
  )

The keywords for a field does not have to match the name of the field as
it is referenced by the @rhombus(.) operator. Typically, the names are
the same, and keyword fields support the same shothand as in function
definitions where a keyword by itself implicitly supplies the
corresponding identifier.

@(demo:
    ~defn:
      class Posn(~x, ~y)
    ~repl:
      val p: Posn(~x: 1, ~y: 2)
      p.x
      p.y
    ~defn:
      class Cell(~row: i, ~column: j)
    ~repl:
      val c: Cell(~row: 1, ~column: 2)
      c.i
      c.j
      c
    )

@close_eval(method_eval)
