#lang scribble/rhombus/manual
@(import: "common.rhm" open
          "nonterminal.rhm" open)

@title{Pairs}

A @deftech{pair} combines two values: a ``first'' value and a ``rest''
value. Every non-empty @tech{list} is a pair, a pair is a list only if
its ``rest'' is a list.

@dispatch_table(
  "pair"
  @rhombus(Pair)
  [pr.first, Pair.first(pr)]
  [pr.rest, Pair.rest(pr)]

)

@doc(
  ~nonterminal:
    fst_annot: :: annot
    rst_annot: :: annot
  annot.macro 'Pair'
  annot.macro 'Pair.of($fst_annot, $rst_annot)'
){

 Matches any pair in the form without @rhombus(of). The @rhombus(of)
 variant matches a pair whose components satisfy @rhombus(fst_annotation)
 and @rhombus(rst_annotation).

}

@doc(
  fun Pair(fst_v :: Any, rst_v :: Any) :: Pair
){

 Constructs a pair containg @rhombus(fst_v) and @rhombus(rst_v).

@examples(
  def pr: Pair(1, 2)
  pr
  pr.first
  pr.rest
)

}

@doc(
  bind.macro 'Pair($fst_bind, $rst_bind)'
){

 Matches a pair whose first component matches @rhombus(fst_bind) and
 rest component matches @rhombus(rst_bind).

@examples(
  def Pair(x, y): Pair(1, 2)
  y
  ~error: def Pair(sx :: String, sy :: String): Pair(1, 2)
  def Pair(lx, ly): [1, 2]
  y
)

}

@doc(
  ~nonterminal:
    fst_bind: def bind
    rst_bind: def bind
  fun Pair.cons(fst :: Any, rst :: Any) :: Pair
  bind.macro 'Pair.cons($fst_bind, $rst_bind)'
){

 Aliases for @rhombus(Pair) in expression and binding positions.

 Note that the difference between @rhombus(Pair.cons) and
 @rhombus(List.cons) is that @rhombus(List.cons) requires a list as is
 second argument, which means that it always forms a @tech{list}. In
 contrast, @rhombus(Pair.cons) allows any value as its second
 argument---but it creates a list if that argument is a list.

@examples(
  Pair.cons(1, 2)
  Pair.cons(1, [2, 3])
)

}

