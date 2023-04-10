#lang scribble/rhombus/manual
@(import:
    "common.rhm" open:
      except: Path
    meta_label:
      rhombus/draw:
        expose: Path)

@title{Drawing Context}

@doc(
  interface DC
){

 Represents a @deftech{drawing context} that renders to some desination,
 such as a bitmap or the screen.

 One way to get a drawing context is @rhombus(Bitmap.make_dc).

}

@doc(
  property (dc :: DC).width :: Real.at_least(0.0)
  property (dc :: DC).height :: Real.at_least(0.0)
  property (dc :: DC).size :: matching([_ :: Real.at_least(0.0),
                                        _ :: Real.at_least(0.0)])
){

 The size of the drawing area: width, height, or both.

}

@doc(
  method (dc :: DC).clear() :: Void
){

 Resets the output to an empty state.

}

@doc(
  property | (dc :: DC).pen :: Pen
           | (dc :: DC).pen := p :: Pen
  property | (dc :: DC).brush :: Brush
           | (dc :: DC).brush := b :: Brush
  property | (dc :: DC).font :: Font
           | (dc :: DC).font := f :: Font
  property | (dc :: DC).clipping_region :: Maybe(Region)
           | (dc :: DC).clipping_region := rgn :: Maybe(Region)
){

 Properties to get or set the drawing context's configuration.

}

@doc(
  method (dc :: DC).push() :: Void
  method (dc :: DC).pop() :: Void
){

 Saves and restores the draw context's configuration.

}

@doc(
  method (dc :: DC).point(x :: Real, y :: Real) :: Void
  method (dc :: DC).line(x :: Real, y :: Real,
                         x2 :: Real, y2 :: Real) :: Void
  method (dc :: DC).lines([[x :: Real, y :: Real], ...],
                          ~dx: dx :: Real = 0.0,
                          ~dy: dy :: Real = 0.0) :: Void
  method (dc :: DC).polygon([[x :: Real, y :: Real], ...],
                            ~dx: dx :: Real = 0.0,
                            ~dy: dy :: Real = 0.0,
                            ~fill: fill :: DC.Fill = #'even_odd) :: Void
  method (dc :: DC).rectangle(x :: Real, y :: Real,
                              width :: Real.at_least(0.0),
                              height :: Real.at_least(0.0)) :: Void
  method (dc :: DC).rounded_rectangle(x :: Real, y :: Real,
                                      width :: Real.at_least(0.0),
                                      height :: Real.at_least(0.0),
                                      radius :: Real = -0.25) :: Void
  method (dc :: DC).ellipse(x :: Real, y :: Real,
                            width :: Real.at_least(0.0),
                            height :: Real.at_least(0.0)) :: Void
  method (dc :: DC).arc(x :: Real, y :: Real,
                        width :: Real.at_least(0.0),
                        height :: Real.at_least(0.0),
                        start :: Real, end :: Real) :: Void
  method (dc :: DC).path(p :: Path,
                         ~dx: dx :: Real = 0.0,
                         ~dy: dy :: Real = 0.0,
                         ~fill: fill :: DC.Fill = #'odd_even) :: Void
){

 Draws lines into a drawing context using the current pen. In the case
 of drawing a polygon, rectangle, rounded rectangle, ellipse, or arc, a
 shape is fulled using the current brush, too.

}

@doc(
  method (dc:: DC).text(str :: String,
                        x :: Real, y :: Real,
                        ~combine: combine :: DC.TextCombine = #'kern,
                        ~angle: angle :: Real = 0.0) :: Void
){

 Draws text into a drawing context using the current font.
 
}

@doc(
  method (dc :: DC).bitmap(
    bm :: Bitmap,
    dest_x :: Real, dest_y :: Real,
    ~source_x: source_x :: Real = 0,
    ~source_y: source_y :: Real = 0,
    ~source_width: source_width :: Real.at_least(0.0) = Bitmap.width(bm),
    ~source_height: source_height :: Real.at_least(0.0) = Bitmap.height(bm),
    ~style: style :: DC.BitmapOverlay = #'solid,
    ~color: color :: Color = Color("black"),
    ~mask: mask :: Maybe(Bitmap) = #false
  ) :: Void
){

 Draws a bitmap into the drawing context.

}

@doc(
  method (dc :: DC).copy(source_x :: Real, source_y :: Real,
                         width :: Real.at_least(0.0),
                         height :: Real.at_least(0.0),
                         dest_x2 :: Real, dest_y :: Real) :: Void
){

 Copies a portion of the draw context's content to another portion of
 the drawing context. The source and destination regions can overlap.

}

@doc(
  method (dc :: DC).font_metrics_key() :: Any
){

 Returns a value that changes when the selected font is changed to one
 with different metrics.

}

@doc(
  annot.macro 'DC.BitmapOverlay'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'solid)}  
  @item{@rhombus(#'opaque)}
  @item{@rhombus(#'xor)}
)

}

@doc(
  annot.macro 'DC.TextCombine'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'kern)}  
  @item{@rhombus(#'grapheme)}
  @item{@rhombus(#'char)}
)

}


@doc(
  annot.macro 'DC.Fill'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'even_odd)}  
  @item{@rhombus(#'winding)}
)

}
