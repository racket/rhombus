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
      blank(10).width
    ))

}

@doc(
  fun rectangle(
    ~around: around :: maybe(Pict) = #false,
    ~width: width :: AutoReal = #'#,(@rhombus(auto, ~value)),
    ~height: height :: AutoReal = #'#,(@rhombus(auto, ~value)),
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~rounded: rounded :: maybe(Rounded) = #false,              
    ~order: order :: OverlayOrder = #'back,
    ~refocus: refocus_on :: maybe(Refocus) = #'#,(@rhombus(around, ~value)),
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
 @rhombus(refocus_on) as the second argument. If @rhombus(refocus) is
 @rhombus(#'#,(@rhombus(around, ~value))) and @rhombus(around) is not @rhombus(#false), then the
 pict is refocused on @rhombus(around), and then padded if necessary to
 make the width and height match @rhombus(width) and @rhombus(height).

 The @rhombus(epoch_align) and @rhombus(duration_align) arguments are
 used only when @rhombus(around) is supplied, and they are passed on to
 @rhombus(overlay) to combine a static rectangle pict with
 @rhombus(around). The @rhombus(order) argument is similarly passed along to
 @rhombus(overlay). If @rhombus(around) is @rhombus(#false), the
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
    ~size: size :: AutoReal = #'#,(@rhombus(auto, ~value)),
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~rounded: rounded :: maybe(Rounded) = #false,              
    ~order: order :: OverlayOrder = #'back,
    ~refocus: refocus_on :: maybe(Refocus) = #'#,(@rhombus(around, ~value)),
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
    ~width: width :: AutoReal = #'#,(@rhombus(auto, ~value)),
    ~height: height :: AutoReal = #'#,(@rhombus(auto, ~value)),
    ~arc: arc :: maybe(ArcDirection) = #false,
    ~start: start :: Real = 0,
    ~end: end :: Real = 2 * math.pi,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~order: order :: OverlayOrder = #'back,
    ~refocus: refocus_on :: maybe(Refocus) = #'#,(@rhombus(around, ~value)),
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~duration: duration_align :: DurationAlignment = #'sustain
  ) :: Pict
){

 Like @rhombus(rectangle), but for an ellipse or arc/wedge. The pict
 draws an arc or widge if @rhombus(arc) is @rhombus(#'cw) (clockwise) or
 @rhombus(#'ccw) (counterclockwise).

@examples(
  ~eval: pict_eval
  ellipse(~around: text("Hello"), ~fill: "lightgreen")
)

}

@doc(
  fun circle(
    ~around: around :: maybe(Pict) = #false,
    ~size: size :: AutoReal = #'#,(@rhombus(auto, ~value)),
    ~arc: arc :: maybe(ArcDirection) = #false,
    ~start: start :: Real = 0,
    ~end: end :: Real = 2 * math.pi,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~order: order :: OverlayOrder = #'back,
    ~refocus: refocus_on :: maybe(Refocus) = #'#,(@rhombus(around, ~value)),
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
  fun dc(draw :: Function.of_arity(3),
         ~width: width :: Real,
         ~height: height :: Real,
         ~ascent: ascent :: Real = height,
         ~descent: descent :: Real = 0) :: Pict
){

 Creates a @tech{pict} with an arbitrary drawing context. The
 @rhombus(draw) function receives a s @rhombus(draw.DC), an x-offset, and
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
  fun animate(
    ~children: children :: List.of(Pict) = [],
    proc :: Function.of_arity(1 + children.length()),
    ~extent: extent :: NonnegReal = 0.5,
    ~bend: bender = bend.fast_middle,
    ~sustain_edge: sustain_edge :: TimeOrder = #'before
  ) :: Pict
){

 Creates an @tech{animated pict}. The @rhombus(proc) should accept a
 @rhombus(Real.in(), ~annot) as its first argument and produce a
 @rhombus(StaticPict, ~annot). The @rhombus(proc) is called with the
 elements of @rhombus(children) as additional argument, but each of those
 elements can be adjusted through @rhombus(Pict.replace).

 The resulting pict's @tech{duration} is 1, and the @tech{extent} of
 that duration is determined by the @rhombus(extent) argument. Before the
 pict's @tech{time box}, its drawing and @tech{bounding box} are the same
 as @rhombus(Pict.ghost(proc(0, & children))); after its time box, they are the
 same as @rhombus(Pict.ghost(proc(1, & children))).

 The @rhombus(bender) function adjusts the argument that is passed to
 @rhombus(proc). If @rhombus(bender) is the identity function, then the
 number passed to @rhombus(proc) ranges from @rhombus(0) to @rhombus(1)
 linearly with animation time. The default @rhombus(bend.fast_middle)
 keeps the argument in the range @rhombus(0) to @rhombus(1), but it
 causes the argument to @rhombus(proc) change more slowly near
 @rhombus(0) and @rhombus(1) and more quickly around @rhombus(0.5).

 If @rhombus(sustain_edge) is @rhombus(#'before), then
 @rhombus(Pict.sustain) for the resulting pict will extend its time box
 by adding an epoch to the start with the static representation
 @rhombus(Pict.ghost(proc(0))). If @rhombus(sustain_edge) is
 @rhombus(#'after), then @rhombus(Pict.sustain) instead adds to the end
 of the time box with the static representation
 @rhombus(Pict.ghost(proc(1))).

}

@doc(
  fun bend.fast_start(n :: Real.in(0, 1)) :: Real.in(0, 1)
  fun bend.fast_middle(n :: Real.in(0, 1)) :: Real.in(0, 1)
  fun bend.fast_edges(n :: Real.in(0, 1)) :: Real.in(0, 1)
  fun bend.fast_end(n :: Real.in(0, 1)) :: Real.in(0, 1)
){

 Functions that are useful as a @rhombus(~bend) argument to
 @rhombus(animate) to map a linear animation effect to a non-linear one.

@tabular(~sep: @hspace(2),
         ~column_properties: [#'center],
         [[hspace(1), rhombus(bend.fast_start), rhombus(bend.fast_middle),  rhombus(bend.fast_edges),  rhombus(bend.fast_end)],
          [hspace(1), "", "", "", ""],
          ["", plot(bend.fast_start), plot(bend.fast_middle),  plot(bend.fast_edges),  plot(bend.fast_end)]])

}


@doc(
  fun rebuildable(~children: children :: List.of(Pict) = [],
                  ~config: config :: maybe(Map) = #false,
                  proc :: Function.of_arity(children.length()
                                              + (if config | 1 | 0)))
    :: Pict
){

 Creates a pict that is the same as the result of
 @rhombus(proc(& children)) or @rhombus(proc(& children, config)),
 but where @rhombus(children) contains picts that can be adjusted via
 @rhombus(Pict.rebuild), and where @rhombus(config) can be updated
 by @rhombus(Pict.rebuild), in which case @rhombus(proc) is called with a
 list of replacements for the picts in @rhombus(children) and (if
 applicable) a replacement @rhombus(config).
 
}
  

@doc(
  fun Pict.from_handle(handle) :: Pict
){

 Converts a static pict value compatible with the Racket
 @racketmodname(pict) library into a @rhombus(Pict, ~annot) value.

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
