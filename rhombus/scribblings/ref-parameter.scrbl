#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Parameters}

For now, let's just say that a Rhombus @deftech{parameter} is a Racket
parameter.

@doc(
  ~nonterminal:
    parameter_expr: begin expr
    val_body: begin body
  expr.macro 'parameterize {$parameter_expr: $val_body, ...}:
                $body
                ...'
){

 Returns the result of the @rhombus(body) block as evaluated in a
 fresh parameterization that binds the @tech{parameter} produced by
 each @rhombus(parameter_expr) to the value produced by the
 corresponding @rhombus(val_body) block.

}
