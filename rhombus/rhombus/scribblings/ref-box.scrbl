#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Boxes}

A @deftech{box} is an object with a single value field, which can be
accessed from a box @rhombus(bx) as
@rhombus(#,(@rhombus(bx, ~var)).value) or set to the value of
@nontermref(expr) using
@rhombus(#,(@rhombus(bx, ~var)).value := #,(@nontermref(expr)))
or other @tech{assignment operators} like @rhombus(:=). The function
@rhombus(Box.value) can also be directly used.

A box is normally mutable, but immutable boxes can originate from
Racket. Assignment is statically allowed by fails dynamically for an
immutable box. The @rhombus(Box, ~annot) annotation is satisfied by both
mutable and immutable boxes, while @rhombus(MutableBox, ~annot) and
@rhombus(ImmutableBox, ~annot) require one or the other.

@doc(
  annot.macro 'Box'
  annot.macro 'Box.now_of($annot)'
  annot.macro 'Box.later_of($annot)'
  annot.macro 'MutableBox'
  annot.macro 'ImmutableBox'

){

 The @rhombus(Box, ~annot) annotation (without @rhombus(now_of, ~datum) or
 @rhombus(later_of, ~datum)) matches any box.

 The @rhombus(Box.now_of, ~annot) form constructs a @tech{predicate
  annotation} that matches a box whose values satisfies
 @rhombus(annotation), but it does not ensure in any way that future
 values installed into the box will satisfy @rhombus(annot). The
 given @rhombus(annot) must not be a converting annotation. Static
 information from @rhombus(annot) is not propagated to accesses of
 the box's values, since there's no gauarantee that the value will still
 satisfy the annotation.

 The @rhombus(Box.later_of, ~annot) form constructs a @tech{converter
  annotation} that immediately matches a box without checking
  that its value currently satisfies @rhombus(annot). The conversion
 result of the annotation is a view on the original box, but one where
 @rhombus(annot) is checked against a value when it is accessed from
 the box or for a value to be installed into the box. (A different view
 of the box might changes its value to one that does not astisfy
 @rhombus(annot).) Static information from @rhombus(annot) is propagated
 to accesses of the box's value. Note that a converter @rhombus(annot)
 is applied for each access or update.

 @rhombus(MutableBox, ~annot) matches only mutable boxes, and
 @rhombus(ImmutableBox, ~annot) matches only immutable boxes (that may
 originate from Racket).

@examples(
  ~repl:
    Box(1) :: Box.now_of(Number)
    ~error:
      Box("a") :: Box.now_of(Number)
  ~defn:
    def a :: Box.later_of(Number) = Box("b")
  ~repl:
    ~error:
      a.value
    ~error:
      a.value := "c"
)

}

@doc(
  fun Box(v :: Any) :: Box
){

 Constructs a box containg @rhombus(v)).

@examples(
  def bx = Box(1)
  bx
  bx.value
  bx.value := 2
  bx
)

}

@doc(
  bind.macro 'Box($bind)'
){

 Matches a box whose value matches @rhombus(bind).

@examples(
  def Box(x) = Box(1)
  x
  ~error:
    def Box(sv :: String) = Box(1)
)

}


@doc(
  fun Box.value(box :: Box) :: Any
  fun Box.value(box :: MutableBox, val :: Any) :: Void
){

 Accesses or updates the value field of @rhombus(box).

@examples(
  def bx = Box(1)
  Box.value(bx)
  Box.value(bx, 2)
  Box.value(bx)
)

}
