#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "draw"){Drawing Operations}

Drawing operations are methods on a @rhombus(DC, ~class). See
@secref("dcs") for information on creating a drawing context.

@doc(
  method (dc :: draw.DC).clear() :: Void
){

 Resets the output to an empty state.

}

@doc(
  method (dc :: draw.DC).point(pt :: PointLike)
    :: Void
){

 Draws a point in a drawing context using the
 @rhombuslink(DC.pen){current pen}.

}

@doc(
  method (dc :: draw.DC).line(
    pt1 :: PointLike,
    pt2 :: PointLike
  ) :: Void
  method (dc :: draw.DC).lines(
    [pt :: PointLike, ...],
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0
  ) :: Void
){

 Draws a single line or a sequence of lines a drawing context using the
 @rhombuslink(DC.pen){current pen}. When a single line is drawn, the
 pen's cap is used for the two ends. When multiple lines are drawn to
 connect points, lines that connect at a point use the pen's join.

 Each @rhombus(pt) describing a line endpoint is adjusted by adding
 @rhombus(dpt) as well as @rhombus(dx) and @rhombus(dy).

}

@doc(
  method (dc :: draw.DC).polygon(
    [pt :: PointLike, ...],
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~fill: fill :: DC.Fill = #'even_odd
  ) :: Void

  enum draw.DC.Fill
  | even_odd
  | winding
){

 Like @rhombus(DC.lines), but the first and last @rhombus(pt) are also
 joined (so the pen's cap is not relevant), and the shape surrounded by
 the lines is filled the drawing context's @rhombuslink(DC.brush){current
  brush}. When the current pen and brush are both non-transparent, then
 the brush is used to fill, first, and then the pen is used afterward.

 Each @rhombus(pt) describing an edge endpoint is adjusted by adding
 @rhombus(dpt) as well as @rhombus(dx) and @rhombus(dy).

 The @rhombus(fill) style determines how points within the lines are
 handled when they are enclosed by multiple loops formed by the lines. In
 @rhombus(#'odd_even) mode, a point is considered enclosed within the
 polygon if it is enclosed by an odd number of loops. In
 @rhombus(#'winding) mode, a point is considered enclosed within the
 polygon if it is enclosed by more or fewer clockwise loops than
 counter-clockwise loops.

}

@doc(
  method (dc :: draw.DC).rectangle(
    r :: RectLike
  ) :: Void
){

 Draws a rectangle with the given top-left corner and size. The
 @rhombuslink(DC.pen){current pen} is used for the outline and the
 @rhombuslink(DC.brush){current brush} for filling the shape. When the
 current pen and brush are both non-transparent, then the brush is used
 to fill, first, and then the pen is used afterward.

 In @rhombus(#'unsmoothed) or @rhombus(#'aligned)
 @rhombuslink(DC.smoothing){smoothing mode}, when the pen is size
 @rhombus(0) or @rhombus(1), the filling precisely overlaps the entire
 outline, instead of overlapping only the inner half of the outline. More
 generally, in @rhombus(#'unsmoothed) or @rhombus(#'aligned) mode, the
 path for the outline is adjusted by shrinking the rectangle width and
 height by, after scaling, one drawing unit divided by the
 @rhombuslink(DC.alignment_scale){alignment scale}.

}

@doc(
  method (dc :: draw.DC).rounded_rectangle(
    r :: RectLike,
    radius :: Real = -0.25
  ) :: Void
){

 Draws a rectangle with the given top-left corner and size, except that
 the corners are drawn as quarter-circles using the given radius. The
 @rhombuslink(DC.pen){current pen} is used for the outline and the
 @rhombuslink(DC.brush){current brush} for filling the shape. When the
 current pen and brush are both non-transparent, then the brush is used
 to fill, first, and then the pen is used afterward.

 If @rhombus(radius) is positive, the value is used as the radius of the
 rounded corner. If @rhombus(radius) is negative, the absolute value is
 used as the proportion of the smallest dimension of the rectangle.

 If @rhombus(radius) is less than -0.5 or more than half of the width or
 height of @rhombus(r), an exception is thrown.

 In @rhombus(#'unsmoothed) or @rhombus(#'aligned)
 @rhombuslink(DC.smoothing){smoothing mode}, the path for the outline is
 adjusted by, after scaling, shrinking the rectangle width and height by
 one drawing unit divided by the
 @rhombuslink(DC.alignment_scale){alignment scale}.

}

@doc(
  method (dc :: draw.DC).ellipse(
    r :: RectLike
  ) :: Void
){

 Draws an ellipse contained in a rectangle with the given top-left
 corner and size. The @rhombuslink(DC.pen){current pen} is used for the
 outline and the @rhombuslink(DC.brush){current brush} for filling the
 shape. When the current pen and brush are both non-transparent, then the
 brush is used to fill, first, and then the pen is used afterward.

 In @rhombus(#'unsmoothed) or @rhombus(#'aligned)
 @rhombuslink(DC.smoothing){smoothing mode}, the path for the outline is
 adjusted by, after scaling, shrinking the rectangle width and height by
 one drawing unit divided by the
 @rhombuslink(DC.alignment_scale){alignment scale}.

}

@doc(
  method (dc :: draw.DC).arc(
    r :: RectLike,
    start_radians :: Real,
    end_radians :: Real
  ) :: Void
){

 Draws a counter-clockwise circular arc, a part of the ellipse inscribed
 in the rectangle specified by the rectangle @rhombus(r). The arc starts
 at the angle specified by @rhombus(start_radians) (@rhombus(0) is three
 o’clock and @rhombus(math.pi/2) is twelve o’clock) and continues
 counter-clockwise to @rhombus(end_radians). If @rhombus(start_radians)
 and @rhombus(end_radians) are the same, a full ellipse is drawn.

 The @rhombuslink(DC.pen){current pen} is used for the arc. If the
 @rhombuslink(DC.brush){current brush} is not transparent, it is used to
 fill the wedge bounded by the arc plus lines (not drawn with the pen)
 extending to the center of the inscribed ellipse. If both the pen and
 brush are non-transparent, the wedge is filled with the brush before the
 arc is drawn with the pen.

 In @rhombus(#'unsmoothed) or @rhombus(#'aligned)
 @rhombuslink(DC.smoothing){smoothing mode}, the path for the outline is
 adjusted by, after scaling, shrinking the rectangle width and height by
 one drawing unit divided by the
 @rhombuslink(DC.alignment_scale){alignment scale}.

}

@doc(
  method (dc :: draw.DC).path(
    p :: draw.Path,
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~fill: fill :: DC.Fill = #'odd_even
  ) :: Void
){

 Draws the subpaths of the given @rhombus(draw.Path) object, adding
 @rhombus(dpt), @rhombus(dx), and @rhombus(dy) to each point. The
 @rhombuslink(DC.pen){current pen} is used for the outline and the
 @rhombuslink(DC.brush){current brush} for filling the shape. When the
 current pen and brush are both non-transparent, then the brush is used
 to fill, first, and then the pen is used afterward.

 In @rhombus(#'unsmoothed) or @rhombus(#'aligned)
 @rhombuslink(DC.smoothing){smoothing mode}. points in the path are
 rounded left and down.

 The @rhombus(fill) style determines how points are handled when they
 are enclosed by multiple loops formed by the path. In
 @rhombus(#'odd_even) mode, a point is considered enclosed within the
 path if it is enclosed by an odd number of loops. In @rhombus(#'winding)
 mode, a point is considered enclosed within the path if it is enclosed
 by more or fewer clockwise loops than counter-clockwise loops.

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
    ~mask: mask :: maybe(Bitmap) = #false
  ) :: Void

  enum draw.DC.BitmapOverlay
  | solid
  | opaque
  | xor
){

 Draws a region of a bitmap into the drawing context. The default
 @rhombus(source) region is the entire bitmap.

 For color bitmaps, the @rhombus(style) and @rhombus(color) arguments
 are ignored. For monochrome bitmaps, drawing uses the style and color
 arguments in the same way that a brush uses its style and color settings
 to draw a monochrome stipple (see @rhombus(Brush)).

 If a @rhombus(mask) bitmap is supplied, it must have the same width and
 height as @rhombus(source), and its @rhombus(Bitmap.is_ok) must return
 true, otherwise an exception is thown. The @rhombus(source) bitmap and
 @rhombus(mask) bitmap can be the same object, but if the drawing
 destination is also a @rhombus(Bitmap) object, both bitmaps must be
 distinct from the destination bitmap, otherwise an exception is thrown.

 The effect of @rhombus(mask) on drawing depends on the type of the
 @rhombus(mask) bitmap:

@itemlist(

 @item{If the @rhombus(mask) bitmap is monochrome, drawing occurs in
  the destination only where the @rhombus(mask) bitmap contains black
  pixels (independent of @rhombus(style), which controls how the white
  pixels of a monochrome source are handled).}

 @item{If the @rhombus(mask) bitmap is color with an alpha channel, its
  alpha channel is used as the mask for drawing source, and its color
  channels are ignored.}

 @item{If the @rhombus(mask) bitmap is color without an alpha channel,
  the color components of a given pixel are averaged to arrive at an
  inverse alpha value for the pixel. In particular, if the mask bitmap is
  grayscale, then the blackness of each mask pixel controls the opacity of
  the drawn pixel (i.e., the mask acts as an inverted alpha channel).}

)

 The @rhombuslink(DC.pen){current pen} and
 @rhombuslink(DC.brush){current brush} have no effect on how the bitmap
 is drawn, but the DC’s @rhombuslink(DC.alpha){alpha} setting determines
 the opacity of the drawn pixels (in combination with an alpha channel of
 @rhombus(source), any given @rhombus(mask), and the alpha component of
 @rhombus(color) when source is monochrome).

}

@doc(
  method (dc :: draw.DC).copy(
    source :: RectLike,
    dest :: PointLike
  ) :: Void
){

 Copies a portion of the draw context's content to another portion of
 the drawing context. The source and destination regions can overlap.

}

@doc(
  method (dc:: draw.DC).text(
    str :: String,
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~angle: angle :: Real = 0.0,
    ~combine: combine :: DC.TextCombine = #'kern,
  ) :: Void

  enum draw.DC.TextCombine
  | kern
  | grapheme
  | char
){

 Draws a text string at a specified point @rhombus(dpt) plus
 @rhombus(dx) and 2rhombus(dy). Text is drawn using the
 @rhombuslink(DC.font){current font} and
 @rhombuslink(DC.text_color){current text color}. The
 @rhombuslink(DC.pen){current pen} and @rhombuslink(DC.brush){current
  brush} have no effect on how text is drawn. A null character
 @rhombus(Char"\0") within @rhombus(str) truncates drawing at that point,
 drawing only the preceding characters.

 For unrotated text where @rhombus(angle) is @rhombus(0), the specified
 point is used as the starting top-left point for drawing characters
 (e.g, if ``W'' is drawn, the point is roughly the location of the
 top-left pixel in the ``W''). If @rhombus(angle) is not @rhombus(0), the
 drawn text is rotated counter-clockwise around the specified starting
 point.

 The @rhombus(combine) determines how individual characters in the text
 are treated:

@itemlist(

 @item{@rhombus(#'kern): Text may be drawn with adjacent characters
  combined to form ligature glyphs, with Unicode combining characters as a
  single glyph, with kerning, with right-to-left rendering of characters,
  etc.}

 @item{@rhombus(#'grapheme): Unicode grapheme clusters (with may span
  multiple characters) are drawn individually without (further)
  combination or kerning.}

 @item{@rhombus(#'char): Unicode characters are drawn individually
  without combination or kerning.}

)

 See @rhombus(DC.text_extent) for information on the size of the drawn
 text.

}

@doc(
  method (dc:: draw.DC).text_extent(
    str :: String,
    ~combine: combine :: DC.TextCombine = #'kern
  ) :: values(NonnegReal, NonnegReal, NonnegReal, NonnegReal)
){

 Returns the size of @rhombus(str) as it would be drawn by
 @rhombus(DC.text) using the given @rhombus(combine) mode.

 The result is four numbers:

@itemlist(

  @item{the total width of the text (depends on both the font and the text);}

  @item{the total height of the font (depends only on the font);}

  @item{the distance from the baseline of the font to the bottom of the
  descender (included in the height, depends only on the font); and}

  @item{extra vertical space added to the font by the font designer
  (included in the height, and often zero; depends only on the font).}

)

 The returned width and height define a rectangle is that guaranteed to
 contain the text string when it is drawn, but the fit is not necessarily
 tight. Some undefined number of pixels on the left, right, top, and
 bottom of the drawn string may be ``whitespace,'' depending on the whims
 of the font designer and the platform-specific font-scaling mechanism.

}

@doc(
  method (dc :: draw.DC).font_metrics_key() :: Any
){

 Returns a value that changes according to @rhombus(===) when the
 @rhombuslink(DC.font){current font} is changed to one with different
 metrics.

}
