#lang scribble/rhombus/manual
@(import: "common.rhm" open)

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
and a parenthesized operator. When using a form like @rhombus(expr.rule)
to extends a namespace, the @litchar{.}-separated sequence must be
surrounded by pair of parentheses.

Forms that can extend a namespace typically refer to these non-terminals
as part of their grammar:

@doc(
  grammar identifier_path:
    $identifier
    $identifier_path . $identifier,

  grammar operator_path:
    $operator
    $identifier_path . $operator,

){}

@doc(
  defn.macro 'namespace $identifier_path:
                $body_or_export
                ...',

  grammar identifier_path:
    $identifier
    $identifier_path . $identifier,

  grammar body_or_export:
    $body
    $export
){

 Similar to the same @rhombus(body_or_export) sequence spliced into the
 enclosing context, but definitions within the body are not visible
 outside the body, and @rhombus(export) declarations are allowed and
 determine exports for the @rhombus(identifier_path) immediately after
 @rhombus(namespace). An exported @rhombus(name, ~var) can be reached using
 @rhombus(identifier_path$$(rhombus(.))$$(rhombus(name, ~var))). The name
 @rhombus(identifier_path) also works with @rhombus(import).

@examples(
  namespace math:
    export: pi tau
    val pi: 3.14
    val tau: 6.28,
  math.pi,
  begin:
    import: .math open
    [pi, tau]
)

}

