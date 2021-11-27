#lang scribble/rhombus/manual
@(import:
    "util.rhm": no_prefix
    "common.rhm": no_prefix)

@title[~tag: "annotation"]{Annotations and the Dot Operator}

Besides classes defined with @rhombus[class], a few predefined
annotations work with the @rhombus[-:, ~bind] and @rhombus[::, ~bind]
annotation operators, including @rhombus[Integer, ~ann] (meaning exact
integer), @rhombus[Number, ~ann], @rhombus[String, ~ann],
@rhombus[Keyword, ~ann], and @rhombus[Any, ~ann] (meaning any value).

The @rhombus[-:] and @rhombus[::] operators also work in expression
positions. In that case, the assertion or check is about the expression
on the left-hand side of @rhombus[-:] or @rhombus[::]. For @rhombus[::],
the left-hand expression must produce a value that satisfies the
right-hand annotation, otherwise a run-time exception is raised. The
@rhombus[is_a] operator takes an annotation like @rhombus[::], but it
produces a boolean result indicating whether the result of the left-hand
expression matches the annotation.

@(rhombusblock:
    (flip(origin) -: Posn).x  // prints 0
    // (1 :: Posn)            // would be a run-time error

    origin is_a Posn  // prints #true
    1 is_a Posn       // prints #false
  )

When @rhombus[class] defines a new class, an annotation can be
associated with each field. When the annotation is written with
@rhombus[::], then the annotation is checked when an instance is
created.

@(rhombusblock:
    class Posn(x :: Integer, y :: Integer)

    Posn(1, 2)       // prints Posn(1, 2)
    // Posn(1, "2")  // would be a run-time error
  )

Naturally, class annotations can be used as field annotations, and then
the @rhombus[.] operator can be chained for efficient access:

@(rhombusblock:
    class Line(p1 -: Posn, p2 -: Posn)

    def l1 :: Line:
      Line(Posn(1, 2), Posn(3, 4))

    l1.p2.x  // prints 3
  )

More generally, @rhombus[.] access is efficient when the left-hand side
of @rhombus[.] is an expression that can act as a @deftech{dot
 provider}. A class name is a dot provider, and it provides access to
field-accessor functions, as in @rhombus[Posn.x] (which doesn’t get a
specific @rhombus[x], but produces a function that can be called on a
@rhombus[Posn] instance to extract its @rhombus[x] field). An identifier
that is bound using @rhombus[-:] or @rhombus[::] and a class name is
also a dot provider, and it provides access to fields of a class
instance. More generally, an annotation that is associated to a binding
or expression with @rhombus[-:] or @rhombus[::] might make the binding
or expression a dot provider. See @secref["static-info"] for more
information on dot providers and other static information.

The @rhombus[use_static_dot] definition form binds the @rhombus[.]
operator so that it works only in efficient mode with a dot provider. If
the left-hand side of the @rhombus[.] is not a dot provider, then the
@rhombus[.] defined by @rhombus[use_static_dot] reports a compile-time
error. The @rhombus[use_dynamic_dot] form binds @rhombus[.] to the
default @rhombus[.], which allows dynamic field lookup if the left-hand
side is not a dot provider.

@(rhombusblock:
    use_static_dot

    l1.p2.x  // prints 3
    // 1.x   // disallowed statically
  )

@aside{Using @rhombus[.] to reach an imported binding, as in
 @rhombus[f2c.fahrenheit_to_celsius], is a different kind of @rhombus[.]
 than the infix expression operator.}
