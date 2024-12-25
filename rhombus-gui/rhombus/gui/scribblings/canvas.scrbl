#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Canvases}

@doc(
  class Canvas():
    implements View
    constructor (
      data :: MaybeObs.of(Any),
      draw :: Function.of_arity(2),
      ~mouse: mouse :: Function.of_arity(2) = Function.pass,
      ~key: key :: Function.of_arity(2) = Function.pass,
      ~label: label :: MaybeObs.of(maybe(LabelString)) = "canvas",
      ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
      ~styles: styles :: MaybeObs.of(List.of(Canvas.Style)) = [],
      ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
      ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
      ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
      ~mixin: mix :: Function = values,
    )
){

 Creates a canvas view. When the view is rendered, the @rhombus(draw)
 function is called as

@rhombusblock(
  draw(#,(@rhombus(dc, ~var)) :: DC, #,(@rhombus(data_val, ~var)))
)

 to draw the canvas's content to a backing store @rhombus(dc ~var),
 where @rhombus(data_val, ~var) is the value of @rhombus(Obs.peek(data))
 when @rhombus(data) is an @tech{observable}. The @rhombus(draw) function
 is called to update the canvas content when @rhombus(data) is an
 @tech{observable} and its value changes.

 When a mouse or key event is received by a rendered canvas, the
 @rhombus(mouse) or @rhombus(key) function is called with a
 @rhombus(MouseEvent, ~class) or @rhombus(KeyEvent, ~class),
 respectively, and a @rhombus(CanvasContext, ~class).

}

@doc(
  enum Canvas.Style:
    control_border
    combo
    vscroll
    hscroll
    resize_corner
    gl
    no_autoclear
    transparent
    no_focus
    deleted
){

 A canvas style symbol.

}

@doc(
  interface CanvasContext
  property (ctx :: CanvasContext).client_size :: Size
){

 A @rhombus(CanvasContext, ~class) represents a canvas instance that
 receives mouse or keyboard events so that properties of the instance can
 be accessed, including its size.

}
