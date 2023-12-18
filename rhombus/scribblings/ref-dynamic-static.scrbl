#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Static and Dynamic Lookup}

@doc(
  expr.macro '#%dynamism'
){

 Initially indicates dynamic mode, but intended to be redefined by
 or @rhombus(use_dynamic).

 This binding is not exported by @rhombuslangname(rhombus/static), and
 a binding to indicate static mode is exported, instead.

}

@doc(
  defn.macro 'use_static'
){

 (Re-)defines @rhombus(#%dynamism) to require certain static
 information and consistency with static information:

@itemlist(

 @item{A static @rhombus(.) accesses a component of a target only when
  the access can be resolved statically, otherwise the @rhombus(.) form is
  a syntax error.}

 @item{A static @rhombus(#%index) looks up a value only when
  the lookup operator can be specialized statically (e.g., to a
  @tech{list} or @tech{map} lookup), otherwise the lookup form is an
  error.}

 @item{A static @rhombus(++) works only when the append operation can be
  specialized statically (e.g., to a @tech{list} or @tech{map} append),
  otherwise the operator use is an error.}

 @item{A static @rhombus(#%call) does not require static
  information about functions and methods for calls, but it reports an
  error when the number of supplied arguments is inconsistent with static
  information that is available for the called function or method.
  Similarly, @rhombus(:=) assignment to a property is rejected if static
  information does not declare the property as supporting assignment.}

 @item{A static @rhombus(with) requires static information for a class
  on the left-hand side of @rhombus(with) (i.e., the object to be
  functionally updated). An error is reported if a field name supplied on
  the right-hand side of @rhombus(with) does not correspond to the field
  of the class.}

 @item{A static @rhombus(for) requires static information for each
  sequence on the right-hand side of an @rhombus(each, ~for_clause) form
  to specialize the iteration statically.}

)

 For each of these forms, static mode is detected by checking the
 binding of @rhombus(#%dynamism) using the scopes of form's operator or
 identifier.

 Other forms not provided by @rhombuslangname(rhombus) might also be
 sensitive to dynamic mode versus static mode. Macro implementations
 can use @rhombus(syntax_meta.is_static) to determine a mode.

 See also @secref("static-lib").

@examples(
  class Posn(x, y)
  fun ok_lookup(ps :~ List.of(Posn)):
    use_static
    ps[0].x
  ~error:
    fun bad_lookup(ps):
      use_static
      ps[0].x
  ~error:
    fun still_bad_lookup(ps :~ List):
      use_static
      ps[0].x
  ~error:
    block:
      use_static
      Posn(1, 2, 3)
)

}

@doc(
  defn.macro 'use_dynamic'
){

 (Re-)defines @rhombus(#%dynamism) to not require certain static
 information and consistency with static information. See
 @rhombus(use_static).

@examples(
  class Posn(x, y)
  fun (ps):
    use_dynamic
    ps[0].x
)

}

@doc(
  fun dynamic(v :: Any) :: Any
){

 The identity function.

 A call to @rhombus(dynamic) has no static information, even if its
 argument has static information, so wrapping a call to
 @rhombus(dynamic) around an expression has the effect of discarding
 any static information available for the expression.

@examples(
  class Posn(x, y)
  ~error:
    fun bad_lookup(ps :~ List.of(Posn)):
      use_static
      dynamic(ps)[0].x
  ~error:
    fun still_bad_lookup(ps :~ List.of(Posn)):
      use_static
      dynamic(ps[0]).x
  fun (ps :~ List.of(Posn)):
    dynamic(dynamic(ps)[0]).x
)

}
