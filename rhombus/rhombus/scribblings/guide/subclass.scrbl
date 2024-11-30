#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def subclass_eval = make_rhombus_eval())

@title(~tag: "subclass"){Subclasses}

In a @rhombus(class) body, @rhombus(extends, ~class_clause) followed by
the name of an existing class. Instances of the new class, the
@emph{subclass}, will also count as instances of the existing class, the
@emph{superclass}. Since that creates obligations on the superclass,
however, a class is @emph{final} by default, which means that it does
not permit subclasses.

@examples(
  ~defn:
    class Posn(x, y)
  ~defn:
    ~error:
      class Posn3D(z):
        extends Posn
)

To allow subclasses, add a @rhombus(nonfinal, ~class_clause) clause in a
class:

@examples(
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

@examples(
  ~eval: subclass_eval
  ~repl:
    def p = Posn3D(1, 2, 3)
    p
    p.y
    p is_a Posn
    p is_a Posn3D
)

@(close_eval(subclass_eval))
