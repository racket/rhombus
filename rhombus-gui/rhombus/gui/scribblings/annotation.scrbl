#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Annotations}

@(~version_at_least "8.14.0.4")

@doc(
  annot.macro 'gui.View.SizeInt'
){

 Equivalent to @rhombus(Int.in(0 ..= 1000000), ~annot).

}

@doc(
  annot.macro 'gui.View.Size'
){

 Satisfied by a list containing two @rhombus(maybe(View.SizeInt), ~annot)s.

}

@doc(
  annot.macro 'gui.View.PositionInt'
){

 Equivalent to @rhombus(Int.in(-1000000 ..= 1000000), ~annot).

}

@doc(
  annot.macro 'gui.View.Position'

){

 Satisfied by @rhombus(#'center) or a list containing two
 @rhombus(View.PositionInt, ~annot)s.

}

@doc(
  annot.macro 'gui.View.SpacingInt'
){

 Equivalent to @rhombus(Int.in(0 ..= 1000), ~annot).

}

@doc(
  annot.macro 'gui.View.Margin'
){

 Satisfied by a list containing two @rhombus(View.SpacingInt, ~annot)s.

}

@doc(
  annot.macro 'gui.View.Alignment'
  enum gui.View.HorizAlignment
  | left
  | center
  | right
  enum gui.View.HorizAlignment
  | top
  | center
  | bottom
){

 A @rhombus(View.Alighment, ~annot) is a 2-element list containing a
 @rhombus(View.HorizAlignment) and a @rhombus(View.VertAlignment).

}

@doc(
  annot.macro 'gui.View.Stretch'
){

 Satisfied by a list containing two @rhombus(Boolean, ~annot)s.

}

@doc(
  annot.macro 'gui.View.LabelString'
){

  Satisfied by a string whose length is less than 200 characters.

}
