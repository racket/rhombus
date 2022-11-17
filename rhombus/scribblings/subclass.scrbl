#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(val subclass_eval: make_rhombus_eval())

@title(~tag: "subclass"){Subclasses}

In a @rhombus(class) body, @rhombus(extends, ~class_clause) followed by
the name of an existing class. Instances of the new class, the
@emph{subclass}, will also count as instances of the existing class, the
@emph{superclass}. Since that creates obligations on the superclass,
however, a class is @emph{final} by default, which means that it does
not permit subclasses.

@(demo:
    ~defn:
      class Posn(x, y)
    ~defn:      
      ~error:
        class Posn3D(z):
          extends Posn
  )

To allow subclasses, add a @rhombus(nonfinal, ~class_clause) clause in a
class:

@(demo:
    ~eval: subclass_eval
    ~defn:
      class Posn(x, y):
        nonfinal
    ~defn:             
      class Posn3D(z):
        extends Posn
  )

When a subclass is created, superclass fields are implicitly
included before new fields in the subclass's constructor:

@(demo:
    ~eval: subclass_eval
    ~repl:
      val p: Posn3D(1, 2, 3)
      p
      p.y
      p is_a Posn
      p is_a Posn3D
  )

The @rhombus(interface) definition form is similar to @rhombus(class),
but without fields. A @rhombus(class) form can declare that the class
implements an interface through an @rhombus(implements, ~class_clause)
clause. When a class implements an interface, then instances of the
class satify the interface annotation, similar to the way that a
subclass instances satifies the superclass annotation. A class can have
at most one superclass. but it can implement any number of interfaces.
An interface is never final, so no @rhombus(nonfinal, ~class_clause) is
needed in an interface.

@(demo:
    ~defn:
      interface Shape
      interface Dance
      class Square(side):
        implements: Shape Dance
    ~repl:
      val s: Square(10)
      s is_a Shape
      s is_a Dance
  )

Interfaces can @rhombus(extend, ~intf_clause) other interfaces. Unlike
classes extending at one most superclass, interfaces can extend any
number of superinterfaces.

@close_eval(subclass_eval)
