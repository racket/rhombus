#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "dc"){Generic Drawing Context}

@doc(
  interface draw.DC
){

 Represents a @deftech{drawing context} that renders to some destination,
 such as a bitmap or the screen.

 One way to get a drawing context is @rhombus(Bitmap.make_dc).

 See @secref("draw") for most methods of @rhombus(DC, ~class).

}

@doc(
  property (dc :: draw.DC).width :: NonnegReal
  property (dc :: draw.DC).height :: NonnegReal
  property (dc :: draw.DC).size :: Size
){

 The size of the drawing area: width, height, or both.

}

@doc(
  property (dc :: draw.DC).handle :: Any
  fun draw.DC.from_handle(hand :: Any) :: DC
){

 The @rhombus(DC.handle) property returns a Racket object that
 corresponds to the drawing context for use directly with
 @racketmodname(racket/draw). The @rhombus(DC.from_handle) function
 creates a @rhombus(DC, ~class) from such a Racket object.

}
