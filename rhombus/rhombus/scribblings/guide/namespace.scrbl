#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def ns_eval = make_rhombus_eval())

@title(~tag: "namespaces-overview"){Namespaces}

A dotted module import as @rhombus(convert.fahrenheit_to_celsius) or class
field accessors as @rhombus(Posn.x) and @rhombus(Posn.y) demonstrate
the use of hierarchical names. Other hierarchical names provided by
@rhombuslangname(rhombus) include @rhombus(List.length) and
@rhombus(List.cons) via @rhombus(List) (where lists will discussed more
in @secref("list")):

@examples(
  List.length(["a", "b", "c"])
)

Use the @rhombus(namespace) form to create a namespace without creating a
separate module. The identifier after @rhombus(namespace) is bound as a
namespace, and @rhombus(export) provide forms within the
@rhombus(namespace) body determine the bindings that can be accessed from the
name with the @rhombus(.) operator.

@examples(
  ~eval: ns_eval
  ~defn:
    namespace geometry:
      export:
        tau
        Complex
      def pi = 3.14
      def tau = 2 * pi
      class Complex(real, imag)
  ~repl:
    geometry.tau
    ~error:
      geometry.pi
    geometry.Complex(0, geometry.tau)
)

A name defined with @rhombus(namespace) can be used with @rhombus(import),
but the name must be prefixed with @rhombus(., ~impo) to distinguish it from a
module path. Also, @rhombus(import) can be used in nested blocks
generally, such as a block created with @rhombus(block) or
@rhombus(def):

@examples(
  ~eval: ns_eval
  ~repl:
    block:
      import:
        .geometry open
      Complex(0, tau)
  ~defn:
    def also_pi:
      import:
        .geometry open
      tau/2
  ~repl:
    also_pi
)

Naturally, namespaces can be nested further, either by exporting an
existing namespace or by nesting @rhombus(namespace) forms.

@examples(
  ~eval: ns_eval
  ~defn:
    namespace subject:
      export:
        geometry
        english
      namespace english:
        export:
          greeting
        def greeting = "Hello"
  ~repl:
    subject.english.greeting
    subject.geometry.tau
    block:
      import:
        .subject open
      geometry.tau
)

A @rhombus(., ~impo) can be used in an @rhombus(import) form as a shorthand to
reach a nested binding without making intemediate bindings visible.

@examples(
  ~eval: ns_eval
  block:
    import:
      .List open
    length(["a", "b", "c"])
)

An existing namespace can be extended by using a dotted name in a
definition, such as defining @rhombus(geometry.e) in a context where
@rhombus(geometry) is a namespace. The extension does not mutate the
namespace; it merely extends the bindings that are available in the
scope of the extending definition.

@examples(
  ~eval: ns_eval
  block:
    def geometry.e = 2.71
    geometry.e
  ~error:
    geometry.e
)

When a namespace is exported, any extensions of the namespace visible
at the export site are also exported. Multiple extensions of a
namespace can be imported into a context as long as the extensions do
not conflict, which is partly a result of the rule that the same name
can be imported into a context multiple times as long as the binding
is always the same.


@(close_eval(ns_eval))
