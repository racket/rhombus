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

 Element constructors that adjust the given pre-content to change its
 rendered style.

}

@doc(~include rhombus/scribble/private/element: literal){

 Constructs an element with a string to be used literally, as opposed to
 decoding as usual for @rhombus(PreContent).

}
