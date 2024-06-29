#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@(def method_eval = make_rhombus_eval())

@title(~tag: "private-implement"){Private Implementation}

Although @rhombus(private, ~class_clause) and
@rhombus(protected, ~class_clause) are normally used on methods and
fields, an @rhombus(implements, ~class_clause) clause also can be
modified by @rhombus(private, ~class_clause) or
@rhombus(protected, ~class_clause). A private or protected
implementation of an interface avoids exposing the implementation of the
interface's method to untrusted callers. The implementation of the
interface is still accessible via the interface's
@rhombus(internal, ~class_clause) name, which might be made available
only to trusted contexts.

For example, suppose that we'd like to customize printing by
implementing the @rhombus(Printable, ~class) interface, but we don't want a public
@rhombus(print) method. Printing and string conversion access
customization methods using an internal name for @rhombus(Printable, ~class), so
privately implementing @rhombus(Printable, ~class) will achieve the goal.

To privately implement an interface, use
@rhombus(private, ~class_clause) @rhombus(implements, ~class_clause),
and override the interfaces methods with
@rhombus(private, ~class_clause) methods.

@examples(
  ~defn:
    class Posn(x, y):
      private implements Printable
      private override describe(mode, recur):
        PrintDesc.list("⟨⟨⟨", [recur(x), recur(y)], "⟩⟩⟩")
  ~repl:
    Posn(1, 2)
    ~error:
      Posn(1, 2).describe(#'expr, Function.pass)
)

A @rhombus(protected, ~class_clause) implementation of an interface is
similar to a @rhombus(private, ~class_clause) one, but the implemented
methods are visible in subclasses, and the methods can be overridden in
subclasses.

@(close_eval(method_eval))
