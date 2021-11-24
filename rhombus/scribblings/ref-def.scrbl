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

}

@doc[
  defn.macro '(fun identifier(arg_binding, ...):
                 body
                 ...),
  defn.macro '(fun identifier(arg_binding, ...) :: result_annotation:
                 body
                 ...),

  grammar arg_binding:
    binding
    keyword: binding
    binding = default_expr
    keyword: binding = default_expr
]{

 Binds @rhombus[identifier] as a function.

}
