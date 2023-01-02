#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Static and Dynamic Lookup}

@doc(
  defn.macro 'use_static'
){

 (Re-)defines @rhombus(.) so that it accesses a component of a target
 only when the access can be resolved statically, otherwise the
 @rhombus(.) form is a syntax error. Also, (re-)defines
 @rhombus(#{#%ref}) so that it looks up a value only when the lookup
 operator can be specialized statically (e.g., to a @tech{list} or
 @tech{map} lookup), otherwise the lookup form is an error.

 See also @secref("static-lib").


@examples(
  class Posn(x, y)
  fun (ps -: List.of(Posn)):
    use_static
    ps[0].x
  ~error: fun (ps):
            use_static
            ps[0].x
)

}

@doc(
  defn.macro 'use_dynamic'
){

 (Re-)defines @rhombus(.) and @rhombus(#{#%ref}) to their default
 modes, which allows either static or dynamic resolution of a
 component access or lookup specialization.

@examples(
  class Posn(x, y)
  fun (ps):
    use_dynamic
    ps[0].x
)

}

@doc(
  fun dynamic(v)
){

 The identity function.

 A call to @rhombus(dynamic) has no static information, even if its
 argument has static information, so wrapping a call to
 @rhombus(dynamic) around an expression has the effect of discarding
 any static information available for the expression.

@examples(
  class Posn(x, y)
  ~error: fun (ps -: List.of(Posn)):
            use_static
            dynamic(ps)[0]
  ~error: fun (ps -: List.of(Posn)):
            use_static
            dynamic(ps[0]).x
  fun (ps -: List.of(Posn)):
    dynamic(dynamic(ps)[0]).x
)

}
