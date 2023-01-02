#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Definitions}

@doc(
  defn.macro 'def $binding = $expr'
  defn.macro 'def $binding:
                $body
                ...'
){

 Binds the identifiers of @rhombus(binding) to the value of @rhombus(expr) or the
 @rhombus(body) sequence. The @rhombus(body) itself can include
 definitions, and its normally it ends with an expression to provide the
 result value.

 A @rhombus(binding) can be just an identifier, or it can be constructed
 with a binding operator, such as a pattern form or @rhombus(::) for
 annotations.

@examples(
  ~repl:
    def pi = 3.14
    pi
  ~repl:
    def pi:
      def tau = 6.28
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
  defn.macro 'let $binding = $expr'
  defn.macro 'let $binding:
                $body
                ...'
){

 Like @rhombus(def), but for bindings that become visible only after the
 @rhombus(let) form within its definition context. The @rhombus(let) form
 cannot be used in a top-level context outside of a module or local block.

@examples(
  begin:
    let v = 1
    fun get_v(): v
    let v = v+1
    [get_v(), v]
)

}


@doc(
  bind.macro '$identifier_path . $identifier'
 
  grammar identifier_path:
    $identifier
    $identifier_path . $identifier
){

 The @rhombus(., ~bind) operator works somewhat like a binding operator
 that works only with identifiers, and it specifies a namespace-prefixed
 identifier to bin as an extension of an already-defined namespace. More
 precisely, @litchar{.} to join identifiers in a binding position is
 recognized literally as an operator, along the same lines as @litchar{.}
 used to reference a binding within a namespace or import.

 See @secref("namespaces") for more information about extending
 namespaces.

@examples(
  namespace math:
    export: pi
    def pi: 3.14
  def math.tau: 2 * math.pi
  math.tau
)

}
