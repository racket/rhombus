#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def ann_eval = make_rhombus_eval())

@demo(
  ~eval: ann_eval
  ~hidden:
    class Posn(x, y)
    fun flip(p :~ Posn): Posn(p.y, p.x)
    def origin = Posn(0, 0)
)

@title(~tag: "annotation"){Annotations and the Dot Operator}

Besides classes defined with @rhombus(class), a few predefined
annotations work with the @rhombus(:~, ~bind) and @rhombus(::, ~bind)
annotation operators, including @rhombus(Integer, ~annot) (meaning exact
integer), @rhombus(Number, ~annot), @rhombus(String, ~annot),
@rhombus(Keyword, ~annot), and @rhombus(Any, ~annot) (meaning any value).

The @rhombus(:~) and @rhombus(::) operators also work in expression
positions. In that case, the assertion or check is about the expression
on the left-hand side of @rhombus(:~) or @rhombus(::). For @rhombus(::),
the left-hand expression must produce a value that satisfies the
right-hand annotation, otherwise a run-time exception is raised. The
@rhombus(is_a) operator takes an annotation like @rhombus(::), but it
produces a boolean result indicating whether the result of the left-hand
expression matches the annotation.

@demo(
  ~eval: ann_eval
  (flip(origin) :~ Posn).x
  ~error: (1 :: Posn)
  origin is_a Posn
  1 is_a Posn
)

When @rhombus(class) defines a new class, an annotation can be
associated with each field. When the annotation is written with
@rhombus(::), then the annotation is checked when an instance is
created.

@demo(
  ~eval: ann_eval
  ~defn:
    class Posn(x :: Integer, y :: Integer)
  ~repl:
    Posn(1, 2)
    ~error: Posn(1, "2")
)

Naturally, class annotations can be used as field annotations, and then
the @rhombus(.) operator can be chained for efficient access:

@demo(
  ~eval: ann_eval
  ~defn:
    class Line(p1 :~ Posn, p2 :~ Posn)
    def l1 :: Line:
      Line(Posn(1, 2), Posn(3, 4))
  ~repl:
    l1.p2.x
)

More generally, @rhombus(.) access is efficient when the left-hand side
of @rhombus(.) is an @rhombus(import) prefix, a @tech{namespace}, or
an expression that can act as a @deftech{dot provider}. A class name
acts as a namespace to provides access to
field-accessor functions, as in @rhombus(Posn.x) (which doesn’t get a
specific @rhombus(x), but produces a function that can be called on a
@rhombus(Posn) instance to extract its @rhombus(x) field). An identifier
that is bound using @rhombus(:~) or @rhombus(::) and a class name is
a dot provider, and it provides access to fields of a class
instance. More generally, an annotation that is associated to a binding
or expression with @rhombus(:~) or @rhombus(::) might make the binding
or expression a dot provider. See @secref("static-info") for more
information on dot providers and other static information.

The @rhombus(use_static) definition form binds the @rhombus(.)
operator so that it works only in efficient mode with an import, namespace,
or dot provider. If the left-hand side of the @rhombus(.) is not one of those, then the
@rhombus(.) defined by @rhombus(use_static) reports a compile-time
error. The @rhombus(use_dynamic) form binds @rhombus(.) to the
default @rhombus(.), which allows dynamic field lookup if the left-hand
side is not a dot provider, namespace, or import prefix.

@demo(
  ~eval: ann_eval
  ~defn:
    use_static
  ~repl:
    l1.p2.x
    ~error: (1).x
)

@aside{Using @rhombus(.) to reach an imported binding, as in
 @rhombus(f2c.fahrenheit_to_celsius), is a different kind of @rhombus(.)
 than the infix expression operator.}

@close_eval(ann_eval)
