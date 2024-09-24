#lang rhombus/scribble/manual
@(import:
    "common.rhm" open:
      except: Path)

@title{Drawing Context}

@doc(
  interface draw.DC
){

 Represents a @deftech{drawing context} that renders to some destination,
 such as a bitmap or the screen.

 One way to get a drawing context is @rhombus(Bitmap.make_dc).

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

@doc(
  property (dc :: draw.DC).width :: NonnegReal
  property (dc :: draw.DC).height :: NonnegReal
  property (dc :: draw.DC).size :: Size
){

 The size of the drawing area: width, height, or both.

}

@doc(
  method (dc :: draw.DC).clear() :: Void
){

 Resets the output to an empty state.

}

@doc(
  property
  | (dc :: draw.DC).pen :: Pen
  | (dc :: draw.DC).pen := (p :: Pen)
  property
  | (dc :: draw.DC).brush :: Brush
  | (dc :: draw.DC).brush := (b :: Brush)
  property
  | (dc :: draw.DC).font :: Font
  | (dc :: draw.DC).font := (f :: Font)
  property
  | (dc :: draw.DC).clipping_region :: maybe(Region)
  | (dc :: draw.DC).clipping_region := (rgn :: maybe(Region))
  property
  | (dc :: draw.DC).transformation :: DC.Transformation
  | (dc :: draw.DC).transformation := (rgn :: DC.Transformation)
){

 Properties to get or set the drawing context's configuration.

}

@doc(
  method (dc :: draw.DC).scale(s :: Real) :: Void
  method (dc :: draw.DC).scale(sx :: Real, sy :: Real) :: Void
  method (dc :: draw.DC).translate(dpt :: PointLike) :: Void
  method (dc :: draw.DC).translate(dx :: Real, dy :: Real) :: Void
  method (dc :: draw.DC).rotate(radians :: Real) :: Void
  method (dc :: draw.DC).transform(t :: DC.Transformation) :: Void
){

 Applies a (further) transformation to the drawing context's conversion
 from drawing coordinates to deivice coordinates. In other words, these
 methods change the result that is returned by the
 @rhombus(DC.transformation) property, and they affect drawing accodingly.

}

@doc(
  method (dc :: draw.DC).save() :: Void
  method (dc :: draw.DC).restore() :: Void
  dot (dc :: draw.DC).save_and_restore:
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
  method (dc :: draw.DC).point(pt :: PointLike)
    :: Void
  method (dc :: draw.DC).line(pt1 :: PointLike,
                              pt2 :: PointLike)
    :: Void
  method (dc :: draw.DC).lines(
    [pt :: PointLike, ...],
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0
  ) :: Void
  method (dc :: draw.DC).polygon(
    [pt :: PointLike, ...],
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~fill: fill :: DC.Fill = #'even_odd
  ) :: Void
  method (dc :: draw.DC).rectangle(r :: RectLike)
    :: Void
  method (dc :: draw.DC).rounded_rectangle(r :: RectLike,
                                           radius :: Real = -0.25)
    :: Void
  method (dc :: draw.DC).ellipse(r :: RectLike)
    :: Void
  method (dc :: draw.DC).arc(r :: RectLike,
                             start :: Real, end :: Real)
    :: Void
  method (dc :: draw.DC).path(p :: draw.Path,
                              ~dpt: dpt :: PointLike = Point.zero,
                              ~dx: dx :: Real = 0,
                              ~dy: dy :: Real = 0,
                              ~fill: fill :: DC.Fill = #'odd_even)
    :: Void
){

 Draws lines into a drawing context using the current pen. In the case
 of drawing a polygon, rectangle, rounded rectangle, ellipse, or arc, a
 shape is fulled using the current brush, too.

 Offsets through @rhombus(dpt) and also @rhombus(dx) or @rhombus(dy) are
 combined.

}

@doc(
  method (dc:: draw.DC).text(
    str :: String,
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~combine: combine :: DC.TextCombine = #'kern,
    ~angle: angle :: Real = 0.0
  ) :: Void
){

 Draws text into a drawing context using the current font.

}

@doc(
  method (dc :: draw.DC).bitmap(
    bm :: Bitmap,
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~source: source :: RectLike:
               Rect(Point.zero, Bitmap.size(bm)),
    ~style: style :: DC.BitmapOverlay = #'solid,
    ~color: color :: Color = Color("black"),
    ~mask: mask :: maybe(Bitmap) = #false,
  ) :: Void
){

 Draws a region of a bitmap into the drawing context. The default
 @rhombus(source) region is the entire bitmap.

}

@doc(
  method (dc :: draw.DC).copy(source :: RectLike,
                              dest :: PointLike)
    :: Void
){

 Copies a portion of the draw context's content to another portion of
 the drawing context. The source and destination regions can overlap.

}

@doc(
  method (dc :: draw.DC).font_metrics_key() :: Any
){

 Returns a value that changes when the selected font is changed to one
 with different metrics.

}

@doc(
  enum draw.DC.BitmapOverlay:
    solid
    opaque
    xor
){

 Bitmap transfer modes.

}

@doc(
  enum draw.DC.TextCombine:
    kern
    grapheme
    char
){

 Typesetting modes for text.

}


@doc(
  enum draw.DC.Fill:
    even_odd
    winding
){

 Polygon fill modes.

}


@doc(
  annot.macro 'draw.DC.Transformation'
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
