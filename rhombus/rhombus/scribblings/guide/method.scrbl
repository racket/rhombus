#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def method_eval = make_rhombus_eval())

@title(~tag: "methods"){Methods}

A @rhombus(method, ~class_clause) clause adds a method to a class or
interface. Withn a class, the method can refer to fieds directly, or it
can use @rhombus(this), which refers to the object whose method is
called.

@examples(
  ~eval: method_eval
  ~defn:
    class Posn(x, y):
      method mdist():
        x + y // same as: this.x + this.y
      method move(dx :~ Int, dy :~ Int):
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

@examples(
  ~eval: method_eval
  Posn.move(Posn(1, 2), 10, -10)
)

Methods are inherited in a subclass. Use the
@rhombus(override, ~class_clause) modifier to override a method;
attempting to replace a method with just @rhombus(method, ~class_clause)
will report an error. To call the superclass implementation for a method
that is overidden (usually in the overriding implementation), use
@rhombus(super) plus the @rhombus(.) operator and the method name.

@examples(
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
    def p = Posn3D(1, 2, 3)
    p
    p.mdist()
    p.area()
)

Other method modifiers includes @rhombus(final, ~class_clause) to
prevent overriding in subclasses and @rhombus(abstract, ~class_clause)
(without a method body) to insist on overriding in a subclass before the
subclass can be instantiated. As a shorthand, the form name
@rhombus(method, ~class_clause) can be omitted just after
@rhombus(override, ~class_clause), @rhombus(final, ~class_clause), or
@rhombus(abstract, ~class_clause).

Methods of an interface are typically abstract, and so the
@rhombus(abstract, ~class_clause) modifier is implicit for
@rhombus(method, ~class_clause) within @rhombus(interface) if the method
does not have a body. An interface can also supply implemented methods,
and those implementations can refer to other methods, whether
implemented, abstract, or inherited from a superinterface.

@examples(
  ~defn:
    interface Shape:
      method area()
      method is_empty():
        area() == 0
  ~defn:
    class Square(side):
      implements Shape
      override area():
        side*side
  ~repl:
    Square(0).is_empty()
)

The declaration above of an @rhombus(area) method in @rhombus(Shape)
specifies that the method should accept zero arguments, but that intent
is not enforced on implementations. That is, a class might implement
@rhombus(area) to take additional arguments Result
annotations are different. If @rhombus(area) declares a result
annotation, a check is added to each implementation to ensure that it
results a satisfying result.

@examples(
  ~defn:
    interface Shape:
      method area() :: Real
  ~defn:
    class Square(side):
      implements Shape
      override area():
        "downtown"
  ~repl:
    ~error:
      Square(0).area()
)

This enforcement of result contracts applies to overridding in general,
not just overiding to implement an abstract method. When an overriding
method has its own result annotation, then both the overriding
annotation and the inhereited annotation(s) apply to the method and any
further overrides.


@(close_eval(method_eval))
