#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "namespaces"){Namespaces}

A @deftech{namespace} is somewhat like a module, and it is especially
like a prefix given to an @rhombus(import)ed module. Unlike a module
prefix, however, an existing namespace can be extended externally with
additional bindings. The new bindings do not mutate the namespace, but
they are visible to any use of the namespace within the scope of the
extensions. Using @rhombus(export) on a namespace also exports any
extensions that are visible at the @rhombus(export) form.

A namespace is extended though definition forms that use a
@litchar{.}-separated sequence of identifiers as the name to bind. Where
operators are allowed, the operator to define can be a
@litchar{.}-separated sequence of identifiers followed by a @litchar{.}
and a parenthesized operator. When using a form like @rhombus(expr.macro)
to extend a namespace, the @litchar{.}-separated sequence must be
surrounded by pair of parentheses.

A namespace contains bindings that are in any @tech(~doc: meta_doc){space}, while the
namespace name itself is bound in the @rhombus(namespace, ~space) space. In a
given context, a dotted reference through a namespace takes precedence
over interpreting the namespace identifier instead in the context's
space. For example, if @rhombus(p) is bound as a variable whose value
has an @rhombus(x) field, and if @rhombus(p) is also bound to a
namespace that exports a @rhombus(x) in the expression space, then
@rhombus(p#,(@rhombus(.x, ~datum))) in an expression position refers to the export from the
@rhombus(p) namespace and not the @rhombus(x) field of the @rhombus(p)
object (in other words, the @litchar{.} is not considered as the @rhombus(.) operator).
If the namespace @rhombus(p) exports only @rhombus(x) bindings
in other spaces (such as @rhombus(bind, ~space) or @rhombus(annot, ~space)), then
@rhombus(p.x) refers to the @rhombus(x) field of the @rhombus(p) object.

@doc(
  ~nonterminal:
    export_clause: export ~decl
  defn.macro 'namespace $id_name'
  defn.macro 'namespace $id_name:
                $option; ...                
                $nestable_body
                ...'
  defn.macro 'namespace ~open:
                $option; ...
                $nestable_body
                ...'

  grammar option
  | ~name $id_name
  | ~name: $id_name
){

 Similar to the same @rhombus(nestable_body) sequence spliced into the
 enclosing context, but definitions within the body are not visible
 outside the body, and @rhombus(export) declarations are allowed and
 determine exports for the @rhombus(id_name) immediately after
 @rhombus(namespace). An exported @rhombus(name, ~var) can be reached using
 @rhombus(id_name#,(rhombus(., ~datum))#,(rhombus(name, ~var))). The name
 @rhombus(id_name) also works with @rhombus(import). The @rhombus(id)
 at the end of @rhombus(id_name) is bound in the @rhombus(namespace, ~space)
 @tech(~doc: meta_doc){space}.

 When @rhombus(~open) is supplied in place of @rhombus(id_name), then
 instead of defining a visible namespace, a private namespace name is
 created and also imported with @rhombus(import) to bind all the exported
 names in the enclosing scope. The names are bound using the same
 identifiers as listed in the namespace's @rhombus(export), but without
 scopes created by @rhombus(namespace) to isolate other bindings within
 the namespace.

 The @rhombus(~name) option is analogous to @rhombus(~name) within a
 @rhombus(fun, ~defn) definition. It specifies a prefix used for run-time
 reporting for definitions within the namespace.

@examples(
  ~defn:
    namespace geometry:
      export:
        pi
        tau
      def pi = 3.14
      def tau = 6.28
  ~repl:
    geometry.pi
    block:
      import:
        .geometry open
      [pi, tau]
  ~defn:
    namespace ~open:
      export:
        bump
      def mutable x = 0
      fun bump():
        x := x + 1
        x
  ~repl:
    bump()
    ~error:
      x // not exported
  ~defn:
    namespace ~open:
      ~name example.longer.name
      export fun f(x :: Int):
        x
  ~repl:
    ~error:
      f("oops")
)

}
