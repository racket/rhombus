#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Drawing Context}

@doc(
  interface DC
){

 Represents a @deftech{drawing context} that renders to some desination,
 such as a bitmap or the screen.

 One way to get a drawing context is @rhombus(Bitmap.make_dc).

}

@doc(
  method DC.clear(dc :: DC) :: Void
){

 Resets the output to an empty state.

}

@doc(
  method DC.set_pen(dc :: DC, p :: Pen) :: Void
  method DC.set_brush(dc :: DC, b :: Brush) :: Void
){

 Sets the drawing context's state.

}

@doc(
  method DC.draw_point(dc:: DC, x :: Real, y :: Real) :: Void
  method DC.draw_line(dc:: DC,
                      x :: Real, y :: Real,
                      x2 :: Real, y2 :: Real) :: Void
  method DC.draw_lines(dc:: DC,
                       [[x :: Real, y :: Real], ...],
                       ~dx: dx :: Real = 0.0,
                       ~dy: dy :: Real = 0.0) :: Void
  method DC.draw_rectangle(dc:: DC,
                           x :: Real, y :: Real,
                           width :: Real.at_least(0.0),
                           height :: Real.at_least(0.0)) :: Void
  method DC.draw_ellipse(dc:: DC,
                         x :: Real, y :: Real,
                         width :: Real.at_least(0.0),
                         height :: Real.at_least(0.0)) :: Void
  method DC.draw_arc(dc:: DC,
                     x :: Real, y :: Real,
                     width :: Real.at_least(0.0),
                     height :: Real.at_least(0.0),
                     start :: Real, end :: Real) :: Void
){

 Draws lines into a drawing context using the current pen. In the case
 of drawing a rectangle, ellipse, or arc, a shape is fulled using the
 current brush, too.

}

@doc(
  method DC.draw_bitmap(
    dc: DC,
    bm :: Bitmap,
    dest_x :: Real, dest_y :: Real,
    ~source_x: source_x :: Real = 0,
    ~source_y: source_y :: Real = 0,
    ~source_width: source_width :: Real.at_least(0.0) = Bitmap.width(bm),
    ~source_height: source_height :: Real.at_least(0.0) = Bitmap.height(bm),
    ~style: style :: DC.CopyStyle = #'solid,
    ~color: color :: Color = Color("black"),
    ~mask: mask :: Maybe(Bitmap) = #false
  ) :: Void
){

 Draws a bitmap into the drawing context.

}

@doc(
  method DC.copy(dc :: DC,
                 source_x :: Real, source_y :: Real,
                 width :: Real.at_least(0.0),
                 height :: Real.at_least(0.0),
                 dest_x2 :: Real, dest_y :: Real) :: Void
){

 Copies a portion of the draw context's content to another portion of
 the drawing context. The source and destination regions can overlap.

}

@doc(
  method DC.font_metrics_key(dc :: DC) :: Any
){

 Returns a value that changes when the selected font is changed to one
 with different metrics.

}

@doc(
  annot.macro 'DC.CopyStyle'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'solid)}  
  @item{@rhombus(#'opaque)}
  @item{@rhombus(#'xor)}
)

}
