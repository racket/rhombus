#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Annotations}

@doc(
  annot.macro 'SizeInteger'
){

 Equivalent to @rhombus(Int.in(0, 1000000), ~annot).

}

@doc(
  annot.macro 'Size'
){

 Satisfied by a list containing two @rhombus(maybe(SizeInteger), ~annot)s.

}

@doc(
  annot.macro 'PositionInteger'
){

 Equivalent to @rhombus(Int.in(-1000000, 1000000), ~annot).

}

@doc(
  annot.macro 'Position'

){

 Satisfied by @rhombus(#'center) or a list containing two
 @rhombus(PositionInteger, ~annot)s.

}

@doc(
  annot.macro 'SpacingInteger'
){

 Equivalent to @rhombus(Int.in(0, 1000), ~annot).

}

@doc(
  annot.macro 'Margin'
){

 Satisfied by a list containing two @rhombus(SpacingInteger, ~annot)s.

}

@doc(
  annot.macro 'Alignment'
){

 Satisfied by a list containing two symbols: @rhombus(#'left),
 @rhombus(#'center), or @rhombus(#'right), and @rhombus(#'top),
 @rhombus(#'center), or @rhombus(#'bottom).

}

@doc(
  annot.macro 'Stretch'
){

 Satisfied by a list containing two @rhombus(Boolean, ~annot)s.

}

@doc(
  annot.macro 'LabelString'
){

  Satisfied by a string whose length is less than 200 characters.

}
