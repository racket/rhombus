#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Arrays}

An expression followed by a square-bracketed sequence of expressions
is parsed as an implicit use of the @rhombus(#{#%ref}) form, which is
normally bound to implement an array reference or assignment, as well
as other operations.

The @rhombus(.) operator can be used on a array expression with
@rhombus(length) to call @rhombus(Array.length).

@doc(
  expr.macro '#{#%ref} $expr[$at_expr]',
  expr.macro '#{#%ref} $expr[$at_expr] := $rhs_expr',
){

Without @rhombus(:=), accesses the element of the map, array, list, or
string produced by @rhombus(expr) at the index or key produced by
@rhombus(at_expr).

With @rhombus(:=), a mutable array, map, or set element is assigned to
the value produced by @rhombus(rhs_expr), and the expression result is
@rhombus(#void).

}

@doc(
  fun Array(v :: Any, ...) :: Array
){

 Constructs a mutable array containing given arguments.

@examples(
  val a: Array(1, 2, 3),
  a,
  a[0],
  a[0] := 0,
  a
)

}

@doc(
  bind.macro 'Array($binding, ...)'
){

 Matches an array with as many elements as @rhombus(binding)s, where
 each element matches its corresponding @rhombus(binding).

@examples(
  val Array(1, x, y): Array(1, 2, 3),
  y
)

}

@doc(
  annotation.macro 'Array',
  annotation.macro 'Array.of($annotation)',
){

 Matches any array in the form without @rhombus(of). The @rhombus(of)
 variant matches an array whose elements satisfy @rhombus(annotation).

}

@doc(
  fun Array.make(length :: Integer, val = 0) :: Array
){

  Creates a fresh array with @rhombus(length) slots, where each slot
  is initialized to @rhombus(val).

@examples(
  Array.make(3, "x")
)

}
