#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def method_eval: make_rhombus_eval())

@title(~tag: "constructors"){Constructors}

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
      def Posn(~y: y1, ~x: x1): Posn(~x: 1, ~y: 2)
      y1
  )

The keyword for a field does not have to match the name of the field as
it is referenced by the @rhombus(.) operator. Typically, the names are
the same, and keyword fields support the same shothand as in function
definitions where a keyword by itself implicitly supplies the
corresponding identifier.

@(demo:
    ~defn:
      class Posn(~x, ~y)
    ~repl:
      def p: Posn(~x: 1, ~y: 2)
      p.x
      p.y
    ~defn:
      class Cell(~row: i, ~column: j)
    ~repl:
      def c: Cell(~row: 1, ~column: 2)
      c.i
      c.j
      c
    )

Keyword and optional arguments tweak the default constructor that is
implemented for a class, but a @rhombus(constructor, ~class_clause)
clauses replaces the constrictor completely. The syntax of
@rhombus(constructor, ~class_clause) is like a @rhombus(fun) expression
form, but with @rhombus(constructor, ~class_clause) in place of
@rhombus(fun). In the body of a constructor, @rhombus(super) refers to a
function that is like the default constructor, at least in the case of a
class without a superclass.

@(demo:
    ~eval: method_eval
    ~defn:
      class Posn(~x, ~y):
        nonfinal
        constructor
        | (): super(~x: 0, ~y: 0)
        | (~x: x, ~y: y):
            super(~x: x, ~y: x)
        | (~r: r, ~θ: θ):
            super(~x: r*cos(θ), ~y: r*sin(θ))
    ~repl:
      Posn()
      Posn(~x: 1, ~y: 2)
      Posn(~r: 1, ~θ: 0.79)
  )

Using the name @rhombus(super) to access an underlying constructor makes
a kind of sense, but that name can also be misleading: calling
@rhombus(super) does not create an instance of a superclass. (In this
case, there is no superclass!) In fact, calling @rhombus(super) may
create an instance of a @emph{subclass}. That's because calling
@rhombus(super) in general continues the construction of an object that
might have been started for instantiating this class or a subclass.

When a class has a superclass, @rhombus(super) in a constructor does
call the superclass constructor, and the arguments should match whatever
is expected by the superclass's constructor. The result, however, is not
an instance of the class or superclass, but a function that expects
arguments for the fields that are added to the new class. That second
round of arguments should match the ones the that the default
constructor would accept if it were for a class with no superclass. The
@rhombus(super) function needs a programmer to explicitly separate the
two sets of arguments, because it might be ambiguous which arguments are
for the superclass and which are for the new class, depending on the
arguments allowed by the superclass constructor; currying is a simple
way to enable that distinction.

In the example below, @rhombus(Posn3D) adds a @rhombus(z) field that is
tagged with a @rhombus(~z) keyword, so the second set of arguments to
@rhombus(super) should have just one argument and use the @rhombus(~z)
keyword. Meanwhile, the first set of arguments can take any of the forms
that the @rhombus(Posn) constructor supports.

@(demo:
    ~eval: method_eval
    ~defn:
      class Posn3D(~z):
        extends Posn
        constructor
        | (): super()(~z: 0)
        | (~x: x, ~y: y, ~z: z):
            super(~x: x, ~y: x)(~z: z)
        | (~r: r, ~θ: θ, ~φ: φ):
            super(~r: r*cos(φ), ~θ: θ)(~z: r*sin(φ))
    ~repl:
      Posn3D()
      Posn3D(~x: 1, ~y: 2, ~z: 3)
      Posn3D(~r: 1, ~θ: 0.79, ~φ: 0.314)
)

When @rhombus(Posn3D) instances are created like this, the
@rhombus(super) calls in the @rhombus(Posn3D) constructor first gather
the two sets of arguments, and the the first set is passed on to the
constructor of @rhombus(Posn). Within that invocation of the
@rhombus(Posn) constructor, calling @rhombus(super) produces an the
instance of @rhombus(Posn3D), not merely a @rhombus(Posn) instance.

@close_eval(method_eval)
