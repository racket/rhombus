#lang rhombus/scribble/manual

@(import:
    "pict_eval.rhm".pict_eval
    "plot.rhm".plot
    pict.bend
    meta_label:
      rhombus open
      pict open
      draw)

@title(~tag: "shape"){Pict Constructors}

@doc(
  fun blank(
    size :: Real = 0,
    ~width: width :: Real = size,
    ~height: height :: Real = size,
    ~ascent: ascent :: Real = height,
    ~descent: descent :: Real = 0
  ) :: StaticPict
){

 Creates a blank @tech{static pict} with the specified bounding box.

@(block:
    let width = "shadow arg"
    @examples(
      ~eval: pict_eval
      blank(10)
      blank(10).width
    ))

}

@doc(
  fun rectangle(
    ~around: around :: maybe(Pict) = #false,
    ~width: width :: AutoReal = #'auto,
    ~height: height :: AutoReal = #'auto,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~rounded: rounded :: maybe(Rounded) = #false,              
    ~order: order :: OverlayOrder = #'front,
    ~refocus: refocus_on :: maybe(Refocus) = #'around,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain
  ) :: Pict
){

 Creates a @tech{pict} to draw a rectangle. If an @rhombus(around) pict is
 provided, then it both supplies default @rhombus(width) and
 @rhombus(height) values and is @rhombus(overlay)ed on top of the rectangle
 image,

 The rectangle has an outline if @rhombus(line) is not @rhombus(#false),
 and it is filled in if @rhombus(fill) is not @rhombus(#false). If the
 rectangle has an outline, @rhombus(line_width) is used for the outline. A
 @rhombus(line), @rhombus(fill), or @rhombus(line_width) can be
 @rhombus(#'inherit) to indicate that a context-supplied color and line
 width should be used. See also @rhombus(Pict.colorize) and
 @rhombus(Pict.colorize) @rhombus(Pict.line_width).

 If @rhombus(rounded) is a non-negative number, it is used as the radius
 of an arc to use for the rectangle's corners. If @rhombus(rounded) is a
 negative number, it is negated and multiplied by the rectangle's width
 and height to get a radius (in each direction) for the rounded corner.

 When the @rhombus(refocus_on) argument is a pict, then
 @rhombus(Pict.refocus) is used on the resulting pict with
 @rhombus(refocus_on) as the second argument. If @rhombus(refocus_on) is
 @rhombus(#'around) and @rhombus(around) is not @rhombus(#false), then the
 pict is refocused on @rhombus(around), and then padded if necessary to
 make the width and height match @rhombus(width) and @rhombus(height).

 The @rhombus(epoch_align) and @rhombus(duration_align) arguments are
 used only when @rhombus(around) is supplied, and they are passed on to
 @rhombus(overlay) to combine a static rectangle pict with
 @rhombus(around). The @rhombus(order) argument is similarly passed along to
 @rhombus(overlay), where @rhombus(#'front) places @rhombus(around) in front
 of the rectangle. If @rhombus(around) is @rhombus(#false), the
 resulting pict is always a @tech{static pict}.

@examples(
  ~eval: pict_eval
  rectangle()
  rectangle(~fill: "lightblue", ~line: #false)
  rectangle(~line: "blue", ~rounded: #'default)
  rectangle(~around: text("Hello"), ~fill: "lightgreen")
)

}


@doc(
  fun square(
    ~around: around :: maybe(Pict) = #false,
    ~size: size :: AutoReal = #'auto,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~rounded: rounded :: maybe(Rounded) = #false,              
    ~order: order :: OverlayOrder = #'front,
    ~refocus: refocus_on :: maybe(Refocus) = #'around,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain
  ) :: Pict
){

 A shorthand for @rhombus(rectangle) where the width and height are
 specified as @rhombus(size).

@examples(
  ~eval: pict_eval
  square(~around: text("Hello"), ~fill: "lightgreen")
)

}

@doc(
  fun ellipse(
    ~around: around :: maybe(Pict) = #false,
    ~width: width :: AutoReal = #'auto,
    ~height: height :: AutoReal = #'auto,
    ~arc: arc :: maybe(ArcDirection) = #false,
    ~start: start :: Real = 0,
    ~end: end :: Real = 2 * math.pi,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~order: order :: OverlayOrder = #'front,
    ~refocus: refocus_on :: maybe(Refocus) = #'around,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain
  ) :: Pict
){

 Like @rhombus(rectangle), but for an ellipse or arc/wedge. The pict
 draws an arc or widge if @rhombus(arc) is @rhombus(#'cw) (clockwise) or
 @rhombus(#'ccw) (counterclockwise).

 When @rhombus(around) is provided, it determines default
 @rhombus(width) and @rhombus(height) values, but since the ellipse fits
 inside a @rhombus(width) by @rhombus(height) rectangle, the
 @rhombus(around) pict may extend beyond the ellipse's edge at the
 corners.

@examples(
  ~eval: pict_eval
  ellipse(~around: text("Hello"), ~fill: "lightgreen")
)

}

@doc(
  fun circle(
    ~around: around :: maybe(Pict) = #false,
    ~size: size :: AutoReal = #'auto,
    ~arc: arc :: maybe(ArcDirection) = #false,
    ~start: start :: Real = 0,
    ~end: end :: Real = 2 * math.pi,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~order: order :: OverlayOrder = #'front,
    ~refocus: refocus_on :: maybe(Refocus) = #'around,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain
  ) :: Pict
){

 Like @rhombus(square), but a shorthand for @rhombus(ellipse).

@examples(
  ~eval: pict_eval
  circle(~around: text("Hello"), ~fill: "lightgreen")
)

}

@doc(
  fun triangle(
    ~around: around :: maybe(Pict) = #false,
    ~size: size :: AutoReal = #'auto,
    ~width: width :: AutoReal = size,
    ~height: height :: AutoReal = size,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~rounded: rounded :: maybe(Rounded) = #false,              
    ~order: order :: OverlayOrder = #'front,
    ~refocus: refocus_on :: maybe(Refocus) = #'around,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain
  ) :: Pict
){

 Like @rhombus(rectangle), but for an isosceles triangle with a base
 along the bottom of a rectangle defined by @rhombus(width) and
 @rhombus(height) and a vertex at the center of the top of the rectangle.
 As a shorthand, the @rhombus(size) argument can be used as both the
 @rhombus(width) and @rhombus(height).

@examples(
  ~eval: pict_eval
  triangle(~fill: "orange")
  triangle(~fill: "orange").rotate(-1/2 * math.pi)
)


}

@doc(
  fun polygon(
    [pt :: draw.PointLike.to_point, ...],
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit
  ) :: Pict
){

 Creates a @tech{pict} that draws a polygon. The maximum x and y values
 among the @rhombus(pt)s determine the resulting pict's bounding box.

@examples(
  ~eval: pict_eval
  polygon([[0, 0], [50, 0], [50, 50]], ~fill: "lightgreen")
)

}

@doc(
  fun line(
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~line: color :: maybe(ColorMode) = #'inherit,
    ~line_width: width :: LineWidth = #'inherit
  ) :: Pict
){

 Creates a @tech{pict} that draws a line from the top-left of the pict.
 The @rhombus(dx) and @rhombus(dy) arguments determine both the shape of
 the line and the width and height of the pict.

@examples(
  ~eval: pict_eval
  line(~dx: 10, ~line_width: 3)
  line(~dy: 10, ~line: "blue", ~line_width: 3)
  line(~dx: 10, ~dy: 10)
)

}

@doc(
  fun text(content :: String,
           ~font: font :: draw.Font = draw.Font()) :: Pict
){

 Creates a @tech{pict} that draws text using @rhombus(font)

@examples(
  ~eval: pict_eval
  text("Hello")
  text("Hi!", ~font: draw.Font(~kind: #'roman, ~size: 20, ~style: #'italic))
)

}

@doc(
  fun bitmap(path :: Path || String) :: Pict
){

 Creates a @tech{pict} that draws a bitmap as loaded from @rhombus(path).

}

@doc(
  fun dc(draw :: (draw.DC, Real, Real) -> ~any,
         ~width: width :: Real,
         ~height: height :: Real,
         ~ascent: ascent :: Real = height,
         ~descent: descent :: Real = 0) :: Pict
){

 Creates a @tech{pict} with an arbitrary drawing context. The
 @rhombus(draw) function receives a @rhombus(draw.DC), an x-offset, and
 a y-offset.

@examples(
  ~eval: pict_eval
  dc(fun (dc :: draw.DC, dx, dy):
       dc.line([dx, dy+10], [dx+20, dy+10])
       dc.line([dx+10, dy], [dx+10, dy+20])
       dc.ellipse([[dx, dy], [21, 21]]),
     ~width: 20,
     ~height: 20)
)

}


@doc(
  fun explain_bbox(
    pic :: Pict,
    ~line: line :: maybe(ColorMode) = "firebrick",
    ~baseline: baseline :: maybe(ColorMode) = "royalblue",
    ~topline: topline :: maybe(ColorMode) = "seagreen",
    ~scale: scale_n :: Real = 3,
    ~line_width: line_width :: LineWidth = 1,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain
  ) :: Pict
){

 Returns a pict like @rhombus(pict), but the pict’s @tech{bounding box} is
 framed in the color @rhombus(line), a baseline showing the bounding
 box's descent is draw as a line using the color @rhombus(baseline), and
 a topline showing the bounding box's ascent is draw as a line using the
 color @rhombus(topline). If any line color is @rhombus(#false), then
 that part is not drawn. The pict is then scaled by @rhombus(scale_n).
 The bounding box lines use width @rhombus(line_width) before scaling.

 Note that for a single line of text, the baseline and topline are the
 same, so only one line will be visible.

 The @rhombus(epoch_align) and @rhombus(duration_align) arguments are
 used as in @rhombus(rectangle).

@examples(
  ~eval: pict_eval
  ~repl:
    def p = stack(text("Hello"), text("Pict"), text("World"))
    explain_bbox(p)
)

}
  
@doc(
  enum ColorMode:
    ~is_a Color
    ~is_a String
    inherit
){

 A color specification, where @rhombus(#'inherit) allows the color to be
 configured externally, such as through @rhombus(Pict.colorize).

}

@doc(
  enum LineWidth:
    ~is_a Real
    inherit
){

 A line-width specification, where @rhombus(#'inherit) allows the color
 to be configured externally, such as through @rhombus(Pict.line_width).

}

@doc(
  enum AutoReal:
    ~is_a Real
    auto
){

 A dimension with a computed default indicated by @rhombus(#'auto) for
 functions like @rhombus(rectangle).

}

@doc(
  enum Refocus:
    ~is_a Pict
    around
){

 Refocusing options for functions like @rhombus(rectangle).

}


@doc(
  enum Rounded:
    ~is_a Real
    default
){

 Corner-rounding options for @rhombus(rectangle).

}

@doc(
  enum ArcDirection:
    cw
    ccw
){

 Arc directions, clockwise or counterclockwise.

}
