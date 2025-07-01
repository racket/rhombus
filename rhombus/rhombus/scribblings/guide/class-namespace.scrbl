#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def method_eval = make_rhombus_eval())
@// Hack for initial example in a top-level environment:
@examples(
  ~eval: method_eval
  ~hidden:
    class Posn(x, y)
)

@title(~tag: "class-namespace"){Class Namespaces}

A name defined by @rhombus(class) works as a @tech(~doc: ref_doc){namespace} to access
field selectors and methods of the class. It also works like the
@rhombus(namespace) form, because the body of a @rhombus(class) can
include @rhombus(export) forms to export additional bindings. Other
definitions can also be written in the @rhombus(class) body, and those
definitions are accessible outside the class only if they are exported.

@examples(
  ~eval: method_eval
  ~defn:
    class Posn(x, y):
      fun get_origin():
        Posn(0, 0)
      export:
        get_origin
  ~repl:
    Posn.get_origin()
)

There's a subtlety, however, when definitions within the @rhombus(class)
body try to refer to the class. Unless the reference is sufficiently
nested or late enough in the @rhombus(class) body, it can't work,
because the class is not defined until enough of the
class body forms are processed.

@examples(
  ~repl:
    ~error:
      class Posn(x, y):
        def origin = Posn(0, 0)
        export:
          origin
)

In this example, the solution can be simply to move the @rhombus(export)
declaration earlier:

@examples(
  ~repl:
    class Posn(x, y):
      export:
        origin
      def origin = Posn(0, 0)
    Posn.origin
)

This change works because definitions and expressions in a
@rhombus(class) body are effectively moved after the class's definition
when there is no class clause or declaration form afterward. Moving the
@rhombus(export) declaration form before the definition of
@rhombus(origin) means that @rhombus(Posn) can be defined before
@rhombus(origin)'s right-hand side is evaluated.

A related way around the problem is to not put the definition inside the
class, but still export it from the class. Just like in
@rhombus(namespace), an @rhombus(export) form in @rhombus(class) can
export any binding that is visible in the environment, including things
defined outside the @rhombus(class) form.

@examples(
  ~defn:
    class Posn(x, y):
      export:
        origin
    def origin = Posn(0, 0)
  ~repl:
    Posn.origin
    Posn.x(Posn.origin)
    Posn.origin.x
)

The only drawback with this strategy is that the inferred name in a
definition (typically for error-reporting purposes) will not have the
class name as a prefix automatically.

Yet another solution is to use @rhombus(class.together), as described in the
next section, but putting helper definitions at the end of the @rhombus(class)
body or after the class can avoid a
small amount of overhead for instance checks.


@(close_eval(method_eval))
