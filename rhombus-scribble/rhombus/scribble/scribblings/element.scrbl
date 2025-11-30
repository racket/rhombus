#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "element"){Elements}

@doc(~include rhombus/scribble/private/element: elem){

 Constructs an @rhombus(Element, ~annot) with a specific style.

}

@doc(~include rhombus/scribble/private/element:
       italic
       bold
       emph
       tt
       subscript
       superscript
       smaller
       larger){

 Element constructors that each are equivalent to calling @rhombus(elem)
 with a suitable @tech{style} to achieve the named effect.

}

@doc(~include rhombus/scribble/private/element: literal){

 Constructs an element with a string to be used literally, as opposed to
 decoding as usual for @rhombus(PreContent, ~annot).

}
