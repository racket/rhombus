#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def method_eval: make_rhombus_eval())

@// Hack for initial example in a top-level environment:
@demo(
  ~eval: method_eval
  ~hidden:
    class Posn(x, y)
)

@title(~tag: "class-namespace"){Class Namespaces}

A name defined by @rhombus(class) works as a @tech{namespace} to access
field selectors and methods of the class. It also works like the
@rhombus(namespace) form, because the body of a @rhombus(class) can
include @rhombus(export) forms to export additional bindings. Other
definitions can also be written in the @rhombus(class) body, and those
definitions are accessible outside the class only if they are exported.

@demo(
  ~eval: method_eval
  ~defn:
    class Posn(x, y):
      export get_origin
      fun get_origin(): Posn(0, 0)
  ~repl:
    Posn.get_origin()
)

There's a subtlety, however, when definitions within the @rhombus(class)
body try to refer to the class. Unless the reference is sufficiently
nested, it can't work, because the class is not defined until all of the
class body forms are processed.

@demo(
  ~repl:
    ~error:
      class Posn(x, y):
        export origin
        def origin: Posn(0, 0)
)

One way around this problem is to not put the definition inside the
class, but still export it from the class. Just like in
@rhombus(namespace), an @rhombus(export) form in @rhombus(class) can
export any binding that is visible in the environment, including things
defined outside the @rhombus(class) form.

@demo(
  ~defn:
    class Posn(x, y):
      export origin
    def origin: Posn(0, 0)
  ~repl:
    Posn.origin
    Posn.x(Posn.origin)
    Posn.origin.x
)

@close_eval(method_eval)
