#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "annotation"){Sizes, Positions, and Alignment}

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

 In many contexts, when @litchar{&} occurs in a label string, it is
 specially parsed. On Windows and Unix, the character following
 @litchar{&} is underlined in the displayed control to indicate a
 keyboard mnemonic. (On Mac OS, mnemonic underlines are not shown.) The
 underlined mnemonic character must be a letter or a digit. The user can
 effectively click the button by typing the mnemonic when the control's
 enclosing window contains the keyboard focus. The user must also hold
 down the Meta or Alt key if the keyboard focus is currently in a control
 that handles normal alphanumeric input. The @litchar{&} itself is
 removed from label before it is displayed for the control; a
 @litchar{&&} in label is converted to @litchar{&} (with no mnemonic
 underlining). On Mac OS, a parenthesized mnemonic character is removed
 (along with any surrounding space) before the label is displayed, since
 a parenthesized mnemonic is often used for non-Roman languages. Finally,
 for historical reasons, any text after a tab character is removed on all
 platforms.

}
