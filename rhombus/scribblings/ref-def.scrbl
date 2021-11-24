#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Definitions}

@doc[
  defn.macro '(val binding:
                 body
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
  defn.macro '(fun identifier(arg_binding, ...) maybe_result_annotation:
                 body
                 ...),

  grammar arg_binding:
    binding
    keyword: binding
    binding = default_expr
    keyword: binding = default_expr,
  
  grammar maybe_result_annotation:
    :: annotation
    $$("ϵ")
]{

 Binds @rhombus[identifier] as a function.

}

@doc[
  defn.macro '(operator (opname arg_binding)  maybe_result_annotation:
                 body
                 ...),
  defn.macro '(operator (arg_binding opname arg_binding) maybe_result_annotation:
                 body
                 ...),
]{

 Binds @rhombus[opname] as an operator, either prefix or infix. The
 @rhombus[arg_binding] and @rhombus[maybe_result_annotation] parts are
 the same as in @rhombus[function] definitions.

}
