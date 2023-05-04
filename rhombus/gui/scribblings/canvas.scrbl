#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Canvases}

@doc(
  class Canvas():
    implements View
    constructor (data :: MaybeObs.of(Any),
                 draw :: Function.of_arity(2),
                 ~mouse: mouse :: Function.of_arity(1) = Function.pass,
                 ~key: key :: Function.of_arity(1) = Function.pass,
                 ~label: label :: MaybeObs.of(maybe(LabelString)) = "canvas",
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~style: style :: MaybeObs.of(List.of(Canvas.StyleSymbol)) = [],
                 ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
                 ~mixin: mix :: Function = values)
){

 Creates a canvas view. When the view is rendered, the @rhombus(draw)
 function is called as

@rhombusblock(
  draw(#,(@rhombus(dc, ~var)) :: DC, #,(@rhombus(data_val, ~var)))
)

 to draw the canvas's content to a backing store @rhombus(dc ~var),
 where @rhombus(draw_val, ~var) is the value of @rhombus(Obs.peek(data))
 when @rhombus(data) is an @tech{observable}. The @rhombus(draw) function
 is called to update the canvas content when @rhombus(data) is an
 @tech{observable} and its value changes.

 When a mouse or key event is received by a rendered canvas, the
 @rhombus(mouse) or @rhombus(key) funciton is called with a
 @rhombus(MouseEvent, ~class) or @rhombus(KeyEvent, ~class),
 respectively.

}

@doc(
  annot.macro 'Canvas.StyleSymbol'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'control_border)}
 @item{@rhombus(#'combo)} 
 @item{@rhombus(#'vscroll)}
 @item{@rhombus(#'hscroll)}
 @item{@rhombus(#'resize_corner)}
 @item{@rhombus(#'gl)}
 @item{@rhombus(#'no_autoclear)}
 @item{@rhombus(#'transparent)}
 @item{@rhombus(#'no_focus)}
 @item{@rhombus(#'deleted)}

)

}
