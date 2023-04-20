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
  property (dc :: DC).width :: NonnegReal
  property (dc :: DC).height :: NonnegReal
  property (dc :: DC).size :: matching([_ :: NonnegReal,
                                        _ :: NonnegReal])
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
  property | (dc :: DC).transformation :: DC.Transformation
           | (dc :: DC).transformation := rgn :: DC.Transformation
){

 Properties to get or set the drawing context's configuration.

}

@doc(
  method (dc :: DC).scale(s :: Real) :: Void
  method (dc :: DC).scale(sx :: Real, sy :: Real) :: Void
  method (dc :: DC).translate(dx :: Real, dy :: Real) :: Void
  method (dc :: DC).rotate(radians :: Real) :: Void
  method (dc :: DC).transform(t :: DC.Transformation) :: Void
){

 Applies a (further) transformation to the drawing context's conversion
 from drawing coordinates to deivice coordinates. In other words, these
 methods change the result that is returned by the
 @rhombus(DC.transformation) property, and they affect drawing accodingly.

}

@doc(
  method (dc :: DC).save() :: Void
  method (dc :: DC).restore() :: Void
  dot (dc :: DC).save_and_restore:
    $body
    ...
){

 Saves and restores the draw context's configuration.

 The @rhombus(DC.save) method pushes the current drawing state (pen,
 brush, clipping region, and transformation) onto an internal stack, and
 @rhombus(DC.restore) pops the stack and restores the popped drawing
 state. The @rhombus(DC.save_and_restore) form wraps a @rhombus(body)
 sequence to save the drawing state on entry to the sequence and restore
 it on exit, returning the value(s) produced by the @rhombus(body)
 sequence; entry and exit cover continuation jumps, like @rhombus(try).

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
                              width :: NonnegReal,
                              height :: NonnegReal) :: Void
  method (dc :: DC).rounded_rectangle(x :: Real, y :: Real,
                                      width :: NonnegReal,
                                      height :: NonnegReal,
                                      radius :: Real = -0.25) :: Void
  method (dc :: DC).ellipse(x :: Real, y :: Real,
                            width :: NonnegReal,
                            height :: NonnegReal) :: Void
  method (dc :: DC).arc(x :: Real, y :: Real,
                        width :: NonnegReal,
                        height :: NonnegReal,
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
    ~source_width: source_width :: NonnegReal = Bitmap.width(bm),
    ~source_height: source_height :: NonnegReal = Bitmap.height(bm),
    ~style: style :: DC.BitmapOverlay = #'solid,
    ~color: color :: Color = Color("black"),
    ~mask: mask :: Maybe(Bitmap) = #false
  ) :: Void
){

 Draws a bitmap into the drawing context.

}

@doc(
  method (dc :: DC).copy(source_x :: Real, source_y :: Real,
                         width :: NonnegReal,
                         height :: NonnegReal,
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


@doc(
  annot.macro 'DC.Transformation'
){

 Satisfied by an array of six @rhombus(Real, ~annot)s:

@itemlist(

  @item{@rhombus(xx, ~var): a scale from the logical @rhombus(x, ~var) to the device @rhombus(x, ~var)}

  @item{@rhombus(yx, ~var): a scale from the logical @rhombus(y, ~var) added to the device @rhombus(x, ~var)}

  @item{@rhombus(xy, ~var): a scale from the logical @rhombus(x, ~var) added to the device @rhombus(y, ~var)}

  @item{@rhombus(yy, ~var): a scale from the logical @rhombus(y, ~var) to the device @rhombus(y, ~var)}

  @item{@rhombus(x0, ~var): an additional amount added to the device @rhombus(x, ~var)}
  
  @item{@rhombus(y0, ~var): an additional amount added to the device @rhombus(y, ~var)}

)

}
