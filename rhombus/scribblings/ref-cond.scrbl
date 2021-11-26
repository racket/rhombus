#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Conditionals}

@doc[
  decl.macro '(if test_expr
               | then_body
                 ...
               | else_body
                 ...)
]{

 If @rhombus[test_expr] produces a true value (which is value other than
 @rhombus[#false]), returns the result of the @rhombus[then_body] clause,
 otherwise returns the result of the @rhombus[else_body] clause.

@examples[
  if #true
  | "yes"
  | "no",
  if 1+2 == 3
  | val yep: "yes"
    yep
  | "no"
]

}

