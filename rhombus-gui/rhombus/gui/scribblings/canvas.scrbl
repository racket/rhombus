#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def draw_DC = @rhombus(draw.DC, ~annot))

@title{Canvases}

@doc(
  class gui.Canvas():
    implements WindowChildView
    constructor (
      data :: ObsOrValue.of(Any),
      draw :: #,(draw_DC) Any -> ~any,
      ~mouse: mouse :: (KeyEvent, CanvasContext) -> ~any = Function.pass,
      ~key: key :: (MouseEvent, CanvasContext) -> ~any = Function.pass,
      ~label: label :: ObsOrValue.of(maybe(View.LabelString)) = "canvas",
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~styles: styles :: ObsOrValue.of(List.of(Canvas.Style)) = [],
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true]
    )
){

 Creates a canvas view. When the view is rendered, the @rhombus(draw)
 function is called as

@rhombusblock(
  draw(#,(@rhombus(dc, ~var)) :: #,(draw_DC), #,(@rhombus(data_val, ~var)))
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
  enum gui.Canvas.Style
  | control_border
  | combo
  | vscroll
  | hscroll
  | resize_corner
  | gl
  | no_autoclear
  | transparent
  | no_focus
  | deleted
){

 A canvas style symbol.

}

@doc(
  interface gui.CanvasContext
  property (ctx :: gui.CanvasContext).client_size :: Size
){

 A @rhombus(CanvasContext, ~class) represents a canvas instance that
 receives mouse or keyboard events so that properties of the instance can
 be accessed, including its size.

}
