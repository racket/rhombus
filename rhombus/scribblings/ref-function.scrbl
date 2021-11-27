#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Functions}

@doc[
  defn.macro '(fun $identifier($arg_binding, ...) $maybe_result_annotation:
                 $body
                 ...),

  expr.macro '(fun ($arg_binding, ...) $maybe_result_annotation:
                 $body
                 ...),
  
  grammar arg_binding:
    $binding
    $keyword: $binding
    $binding $$(@tt{=}) $default_expr
    $keyword: $binding $$(@tt{=}) $default_expr,
  
  grammar maybe_result_annotation:
    :: $annotation
    $$("ϵ")
]{

 Binds @rhombus[identifier] as a function, or when @rhombus[identifier]
 is not supplied, serves as an expression that produces a function value.

}

@doc[
  defn.macro '(operator ($opname $arg_binding) $maybe_result_annotation:
                 $body
                 ...),
  defn.macro '(operator ($arg_binding $opname $arg_binding) $maybe_result_annotation:
                 $body
                 ...),
]{

 Binds @rhombus[opname] as an operator, either prefix or infix. The
 @rhombus[arg_binding] and @rhombus[maybe_result_annotation] parts are
 the same as in @rhombus[function] definitions.

}
