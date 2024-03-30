#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@(def ann_eval = make_rhombus_eval())
@examples(
  ~eval: ann_eval
  ~hidden:
    class Posn(x, y)
    fun flip(p :~ Posn): Posn(p.y, p.x)
    def origin = Posn(0, 0)
)

@title(~tag: "annotation"){Annotations and the Dot Operator}

Besides classes defined with @rhombus(class), a few predefined
annotations work with the @rhombus(:~, ~bind) and @rhombus(::, ~bind)
annotation operators, including @rhombus(Int, ~annot) (meaning exact
integer), @rhombus(Number, ~annot), @rhombus(String, ~annot),
@rhombus(Keyword, ~annot), and @rhombus(Any, ~annot) (meaning any value).

The @rhombus(:~) and @rhombus(::) operators also work in expression
positions. In that case, the assertion or check is about the expression
on the left-hand side of @rhombus(:~) or @rhombus(::). For @rhombus(::),
the left-hand expression must produce a value that satisfies the
right-hand annotation, otherwise a run-time exception is thrown. The
@rhombus(is_a) operator takes an annotation like @rhombus(::), but it
produces a boolean result indicating whether the result of the left-hand
expression satisfies the annotation.

@examples(
  ~eval: ann_eval
  (flip(origin) :~ Posn).x
  ~error:
    (1 :: Posn)
  origin is_a Posn
  1 is_a Posn
)

When @rhombus(class) defines a new class, an annotation can be
associated with each field. When the annotation is written with
@rhombus(::, ~bind), then the annotation is checked when an instance is
created.

@examples(
  ~eval: ann_eval
  ~defn:
    class Posn(x :: Int, y :: Int)
  ~repl:
    Posn(1, 2)
    ~error:
      Posn(1, "2")
)

Naturally, class annotations can be used as field annotations, and then
the @rhombus(.) operator can be chained for efficient access:

@examples(
  ~eval: ann_eval
  ~defn:
    class Line(p1 :~ Posn, p2 :~ Posn)
    def l1 :: Line:
      Line(Posn(1, 2), Posn(3, 4))
  ~repl:
    l1.p2.x
)

@margin_note_block{Using @rhombus(., ~datum) to reach an
 @rhombus(import)ed or @tech{namespace}d binding, as in
 @rhombus(f2c.fahrenheit_to_celsius, ~datum),
 is different than the infix expression operator, @rhombus(.).}

More generally, @rhombus(.) access is efficient when the left-hand side
of @rhombus(.) is an expression that can act as a @deftech{dot provider}.
A class name also acts as a @tech{namespace} to provides access to
field-accessor functions, as in @rhombus(Posn.x, ~datum) (which doesn't get a
specific @rhombus(x, ~datum), but produces a function that can be called on a
@rhombus(Posn, ~datum) instance to extract its @rhombus(x, ~datum) field).
An identifier that is bound using @rhombus(:~, ~bind) or
@rhombus(::, ~bind) (where a class name follows the
@rhombus(:~, ~bind) or @rhombus(::, ~bind)) is a dot
provider, and it provides access to fields of a class
instance. For example, a use of
@rhombus(#,(@rhombus(p, ~datum)).#,(@rhombus(x, ~datum))) in
the lexical context of a @rhombus(p, ~datum) that is bound
via @rhombus(#,(@rhombus(p, ~datum)) :: #,(@rhombus(Posn, ~datum)), ~bind) is an
efficient access to the @rhombus(x, ~datum) field.
In general, an annotation that is associated to a binding
or expression with @rhombus(:~, ~bind) or @rhombus(::, ~bind) might make the binding
or expression a dot provider. See @secref("static-info") for more
information on dot providers and other static information.

The @rhombus(use_static) definition form redefines
@rhombus(#%dynamism, ~datum) so that the @rhombus(.) operator works
only in efficient mode with a dot provider, among others. If the
left-hand side of the @rhombus(.) is not a dot provider, then
@rhombus(.) under @rhombus(use_static) reports a compile-time
error. The @rhombus(use_dynamic) form binds
@rhombus(#%dynamism, ~datum) to the default @rhombus(#%dynamism),
which allows dynamic field lookup if the left-hand side is not a dot
provider.

@examples(
  ~eval: ann_eval
  ~defn:
    use_static
  ~repl:
    l1.p2.x
    ~error:
      (1).x
)


@(close_eval(ann_eval))
