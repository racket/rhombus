#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Dot}

@doc[
  expr.macro '(target . identifier)
]{

 Accesses a component of @rhombus[target], either statically or
 dyanamically. The access is static when @rhombus[target] is a @tech{dot
  provider}.

}
