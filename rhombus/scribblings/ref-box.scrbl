#lang scribble/rhombus/manual
@(import: "common.rhm" open
          "nonterminal.rhm" open)

@title{Boxes}

A @deftech{box} is an object with a single value field, which can be
accessed from a box @rhombus(bx) as
@rhombus(#,(@rhombus(bx, ~var)).value) or set to the value of
@nontermref(expr) using
@rhombus(#,(@rhombus(bx, ~var)).value := @nontermref(expr))
or other @tech{assignment operators} like @rhombus(:=).

A box is normally mutable, but immutable boxes can originate from
Racket. Assignment is statically allowed by fails dynamically for an
immutable box. The @rhombus(Box, ~annot) annotation is satisfied by both
mutable and immutable boxes, while @rhombus(MutableBox, ~annot) and
@rhombus(ImmutableBox, ~annot) require one or the other.

@doc(
  ~nonterminal:
    val_annot: :: annot
  annot.macro 'Box'
  annot.macro 'Box.now_of($val_annot)'
  annot.macro 'Box.of($val_annot)'
  annot.macro 'MutableBox'
  annot.macro 'ImmutableBox'

){

 The @rhombus(Box, ~annot) annotation (without @rhombus(of) or
 @rhombus(now_of)) matches any box.
 
 The @rhombus(Box.now_of, ~annot) form constructs a @tech{predicate
  annotation} that matches a box whose values satisfies
 @rhombus(annotation), but it does not ensure in any way that future
 values installed into the box will satisfy @rhombus(annot). The
 given @rhombus(annot) must not be a converting annotation. Static
 information from @rhombus(annot) is not propagated to accesses of
 the box's values, since there's no gauarantee that the value will still
 satisfy the annotation.

 The @rhombus(Array.of, ~annot) form constructs a @tech{converter
  annotation} that immediately matches an array @emph{without checking
  that its value currently satisfies} @rhombus(annot). The conversion
 result of the annotation is a view on the original box, but one where
 @rhombus(annotation) is checked against a value when it is accessed from
 the box or for a value to be installed into the box. (A different view
 of the box might changes its value to one that does not astisfy
 @rhombus(annot).) Static information from @rhombus(annot) is propagated
 to accesses of the box's value.

 @rhombus(MutableBox, ~annot) matches only mutable boxes, and
 @rhombus(ImmutableBox, ~annot) matches only immutable boxes (that may
 originate from Racket).
}

@doc(
  fun Box(v :: Any) :: Box
){

 Constructs a box containg @rhombus(v)).

@examples(
  def bx: Box(1)
  bx
  bx.value
  bx.value := 2
  bx
)

}

@doc(
  bind.macro 'Box($val_bind)'
){

 Matches a box whose value matches @rhombus(val_bind).

@examples(
  def Box(x): Box(1)
  x
  ~error: def Box(sv :: String): Box(1)
)

}
