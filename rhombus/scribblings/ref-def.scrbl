#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Definitions}

@doc[
  defn.macro '(val $binding:
                 $body
                 ...)
]{

 Binds the identifiers of @rhombus[binding] to the value of the
 @rhombus[body] sequence. The @rhombus[body] itself can include
 definitions, and its normally it ends with an expression to provide the
 result value.

 A @rhombus[binding] can be just an identifier, or it can be constructed
 with a binding operator, such as a pattern form or @rhombus[::] for
 annotations.

@examples[
  val pi: 3.14,
  pi
]

@examples[
  ~label: #false,
  val pi:
    val tau: 6.28
    tau/2,
  pi
]

@examples[
  ~label: #false,
  val [x, y, z]: [1+2, 3+4, 5+6],
  y
]

@examples[
  ~label: #false,
  val ns :: List: [1+2, 3+4, 5+6],
  ns
]

}


@doc[
  defn.macro '(def $binding:
                 $body
                 ...),
  defn.macro '(def $identifier ($arg_binding, ...) $maybe_result_annotation:
                 $body
                 ...),
  defn.macro '(def ' $expr_pattern:
                 $body
                 ...),
  grammar expr_pattern:
    $identifier
    ($identifier_or_operator $pattern ...)
    ($ $identifier $identifier_or_operator $pattern ...),
  grammar identifier_or_operator:
    $identifier
    $operator
]{

 Like @rhombus[val], @rhombus[def], or @rhombus[expr.rule], depending on
 the form. The @rhombus[fun]-like form matches only if
 @rhombus[identifier] is not bound as a pattern operator.

}



@doc[
  defn.macro '(let $binding:
                 $body
                 ...),
  defn.macro '(let $identifier ($arg_binding, ...) $maybe_result_annotation:
                 $body
                 ...),
  defn.macro '(let ' $expr_pattern:
                 $body)
]{

 Like @rhombus[def], but for bindings that become visible only after the
 @rhombus[let] form within its definition context.

}
