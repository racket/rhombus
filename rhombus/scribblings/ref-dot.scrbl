#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Dot}

@doc[
  expr.macro '($target . $identifier)
]{

 Accesses a component of @rhombus[target], either statically or
 dyanamically. The access is static when @rhombus[target] is a @tech{dot
  provider}.

}


@doc[
  defn.macro 'use_static_dot
]{

 (Re-)defines @rhombus[.] so that it accesses a component of a target
 only when the access can be resolved statically.

}
