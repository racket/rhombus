#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Definitions}

@doc(
  ~nonterminal:
    rhs_expr: block expr
  defn.macro 'def $values_bind = $rhs_expr'
  defn.macro 'def $values_bind:
                $body
                ...'
){

 Binds the identifiers of @nontermref(bind)s to the values of @rhombus(rhs_expr) or the
 @rhombus(body) sequence. The @rhombus(body) itself can include
 definitions, and it normally ends with an expression to provide the
 result values.

 A @nontermref(bind) in @rhombus(values_bind) can be just an identifier or @nontermref(id_name), or it
 can be constructed with a binding operator, such as a pattern form or
 @rhombus(::) for annotations. The number of result values must match
 the number of @nontermref(bind)s. Static information is gathered from
 @rhombus(rhs_expr) or @rhombus(body) and propagated to
 @rhombus(values_bind) as described in @secref(~doc: guide_doc, "static-info-rules").

 An identifier is bound in the @rhombus(expr, ~space) @tech(~doc: meta_doc){space}, and
 most binding operators also create bindings in the
 @rhombus(expr, ~space) space.

 When @rhombus(def) is used with @rhombus(=), then @rhombus(values_bind) or @rhombus(rhs_expr) must
 not contain any immediate @rhombus(=) terms (although @rhombus(=) can
 appear nested in blocks, parentheses, etc.). When a @rhombus(def) group
 both contains a @rhombus(=) and ends in a block, the block is treated as
 part of an @rhombus(rhs_expr) after the @rhombus(=).

@examples(
  ~repl:
    def pi = 3.14
    pi
  ~repl:
    def pi:
      let tau = 6.28
      tau/2
    pi
  ~repl:
    def [x, y, z] = [1+2, 3+4, 5+6]
    y
  ~repl:
    def ns :: List = [1+2, 3+4, 5+6]
    ns
)

}


@doc(
  ~nonterminal:
    rhs_expr: block expr
  defn.macro 'let $values_bind = $rhs_expr'
  defn.macro 'let $values_bind:
                $body
                ...'
){

 Like @rhombus(def), but for bindings that become visible only after the
 @rhombus(let) form within its definition context. The @rhombus(let) form
 cannot be used in a top-level context outside of a module or local
 block. The @rhombus(let) can be used with the same name multiple times
 in a module or block, but the same name cannot be defined with both
 @rhombus(def) and @rhombus(let) within a module or block.

@examples(
  block:
    let v = 1
    fun get_v(): v
    let v = v+1
    [get_v(), v]
)

}


@doc(
  bind.macro '$id_name . $id'
){

 The @rhombus(., ~bind) operator works somewhat like a binding operator
 that works only with identifiers, and it specifies a namespace-prefixed
 identifier to bind as an extension of an already-defined namespace. More
 precisely, @litchar{.} to join identifiers in a binding position is
 recognized literally as an operator, along the same lines as @litchar{.}
 used to reference a binding within a namespace or import.

 See @secref("namespaces") for more information about extending
 namespaces.

@examples(
  namespace geometry:
    export:
      pi
    def pi = 3.14
  def geometry.tau = 2 * geometry.pi
  geometry.tau
)

}
