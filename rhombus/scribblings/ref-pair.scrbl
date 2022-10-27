#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Pairs}

A @deftech{pair} combines two values: a ``first'' value and a ``rest''
value. Every non-empty @tech{list} is a pair, a pair is a list only if
its ``rest'' is a list.

A list works with map-referencing square brackets to access a list
element by position (in time proportional to the position) via
@rhombus(#{#%ref}). A list also works with the @rhombus(++) operator
to append lists.

@dispatch_table(
  "pair",
  @rhombus(Pair),
  [pr.first, Pair.first(pr)],
  [pr.rest, Pair.rest(pr)]

)

@doc(
  fun Pair(fst_v :: Any, rst_v :: Any) :: Pair
){

 Constructs a pair containg @rhombus(fst_v) and @rhombus(rst_v).

@examples(
  val pr: Pair(1, 2),
  pr,
  pr.first,
  pr.rest
)

}

@doc(
  bind.macro 'Pair($fst_bind, $rst_bind)',
){

 Matches a pair whose first component matches @rhombus(fst_bind) and
 rest component matches @rhombus(rst_bind).

@examples(
  val Pair(x, y): Pair(1, 2),
  y,
  ~error: val Pair(sx :: String, sy :: String): Pair(1, 2),
  val Pair(lx, ly): [1, 2],
  y
)

}

@doc(
  annot.macro 'Pair',
  annot.macro 'Pair.of($fst_annotation, $rst_annotation)',
){

 Matches any pair in the form without @rhombus(of). The @rhombus(of)
 variant matches a pair whose components satisfy @rhombus(fst_annotation)
 and @rhombus(rst_annotation).

}

@doc(
  fun Pair.cons(fst :: Any, rst :: Any) :: Pair,
  bind.macro 'Pair.cons($fst_bind, $rst_bind)',
){

 Aliases for @rhombus(Pair) in expression and binding positions.

 Note that the difference between @rhombus(Pair.cons) and
 @rhombus(List.cons) is that @rhombus(List.cons) requires a list as is
 second argument, which means that it always forms a @tech{list}. In
 contrast, @rhombus(Pair.cons) allows any value as its second
 argument---but it creates a list if that argument is a list.

@examples(
  Pair.cons(1, 2),
  Pair.cons(1, [2, 3])
)

}

