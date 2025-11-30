#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "image"){Images}

@doc(~include rhombus/scribble/private/image: image){

 Converts an image from a file into a Scribble element. The given
 @rhombus(pre_content) is used as an alternative to the image (such as
 ``alt text'' in HTML), depending on the @tech{renderer}.

 An image constructed with the @rhombusmodname(pict) library can also be
 used in a Scribble document, so use functions from that library to
 construct an image on the fly.

}
