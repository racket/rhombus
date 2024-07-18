#lang scribble/rhombus/manual

@(import:
    meta_label:
      rhombus open
      pict open
      pict/radial open)

@(def radial_eval = make_rhombus_eval())
@examples(
  ~eval: radial_eval
  ~hidden:
    import:
      pict:
        expose:
          beside
          rectangle
          ellipse
      pict/radial open
)

@title(~tag: "radial"){Pict Radial Shapes}

@docmodule(pict/radial)

@doc(
  fun star(~points: n :: PosInt = 5, ....) :: Pict
  fun flash(~bumps: n :: PosInt = 10, ....) :: Pict
  fun sun(~rays: n :: PosInt = 10, ....) :: Pict
  fun flower(~petals: n :: PosInt = 6, ....) :: Pict
  fun cloud(~bumps: n :: PosInt = 6, ....) :: Pict
  fun regular_polygon(~sides: n :: PosInt = 10, ....) :: Pict
  fun gear(~arms: n :: PosInt = 10, ....,
           ~hole: hole :: Real = 0.5) :: Pict
){

 The same as @rhombus(radial_pict), but with defaults for arguments so that
 the result looks like a flower, cloud, etc., and sometimes with an
 different keyword like @rhombus(~petals) instead of @rhombus(~points).
 The @rhombus(gear) function has an extra @rhombus(hole) argument, which
 specifies a relative side for a hole in the middle of the gear.

@examples(
  ~eval: radial_eval
  star(~fill: "gold")
  flash(~fill: "red")
  flower(~fill: "purple")
  cloud(~fill: "gray")
  regular_polygon(~fill: "blue")
  gear(~fill: "brown")
)

}

@doc(
  fun radial_pict(
    ~points: n :: PosInt = 6,
    ~width: width :: Real = 64,
    ~height: height :: Real = width,
    ~rotate: rotate :: Real = 0,
    ~angle_at: angle_at :: Function.of_arity(2) = evenly_spaced,
    ~inner_radius: inner :: Real = 0.5,
    ~outer_radius: outer :: Real = 1,
    ~inner_pause: inner_pause :: Real = 0,
    ~outer_pause: outer_pause :: Real = 0,
    ~flat_inner_edge: flat_inner_edge = #false,
    ~flat_outer_edge: flat_outer_edge = #false,
    ~outer_pull: outer_pull :: Real = 0,
    ~inner_pull: inner_pull :: Real = 0,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~bound: bound :: BoundingBoxMode = #'unit
  ) :: Pict
){

 A general function for creating shapes like stars, flowers, polygons
 and gears---shapes that have a radial symmetry involving ``points'' that
 extend to an outer radius from a smaller base radius. Functions like
 @rhombus(star), @rhombus(flower), and @rhombus(regular_polygon) have the same
 arguments as @rhombus(radial), but with defaults that produce the shapes
 suggested by the names.

 Various arguments have expected ranges, but none of the ranges are
 enforced, and interesting images can be created by using values outside
 the normal ranges:

@centered(
  @tabular(~sep: @hspace(1),
           [[@rhombus(inner), @elem{@rhombus(0) to @rhombus(outer)}],
            [@rhombus(inner_pause), @elem{@rhombus(0) to @rhombus(1)}],
            [@rhombus(outer_pause), @elem{@rhombus(0) to @rhombus(1)}],
            [@rhombus(inner_pull), @elem{@rhombus(0) to @rhombus(1)}],
            [@rhombus(outer_pull), @elem{@rhombus(0) to @rhombus(1)}]])
)

 The result of @rhombus(radial) is squashed to ellipse form if
 @rhombus(height) is not the same as @rhombus(width). The
 @rhombus(rotate) argument determines an amount of rotation (in radians
 counter-clockwise) applied to the imagine before it is squashed. The
 bounding box for the resulting pict corresponds to a square around the
 original circle, and it is not affected by @rhombus(rotate). (It is
 affected by @rhombus(bound), however, as described further below.)

 Points radiating from the inner to outer radius are evenly spaced by
 default, but @rhombus(angle_at) is called for each point's index to get
 a location for each point, and it can choose a different spacing (e.g.,
 with some ``jitter'' from even spacing in the case of @rhombus(cloud)).

 The points extend from a base of radius @rhombus(inner) to an edge at
 radius @rhombus(outer). By default, the connection from a point at the
 inner radius to a point at outer radius uses up half the radial space
 allocated to the point. If @rhombus(inner_pause) is creater than
 @rhombus(0), it represents a fraction of half the space between points
 that stays at the inner radius before extending out. Similarly,
 @rhombus(outer_pause) is a fraction allocated to staying at the out
 radius. When staying at the inner or outer radius,
 @rhombus(flat_inner_edge) and @rhombus(flat_outer_edge) determine
 whether the start and end points are connected by a straight line or an
 arc at the radius.

 When the @rhombus(inner_pull) and @rhombus(outer_pull) arguments are
 @rhombus(0), then the inner and outer points are straight corners.
 Otherwise, they determine an amount of curvature. Each of
 @rhombus(outer_pull) and @rhombus(inner_pull) represent an amount of
 curvature touward a rounder ``petal.''

 The @rhombus(line), @rhombus(line_width), and @rhombus(fill) arguments
 are the same as for functions like @rhombus(rectangle).

 The @rhombus(bound) argument can be @rhombus(#'unit), @rhombus(#'unit),
 @rhombus(#'shrink), or @rhombus(#'stretch). The default,
 @rhombus(#'unit), gives the resulting pict a bounding box that
 corresponds to the outer radius of the image. If @rhombus(bound) is
 @rhombus(#'shrink), then the bounding box is instead one that encloses
 the inner and outer points of the figure, and so the resulting pict may
 have bounds smaller than @rhombus(width) and @rhombus(height). The
 @rhombus(#'stretch) mode is similar to @rhombus(#'shrink), but the pict
 is scaled and stretched to ensure that its bounding box has dimentions
 @rhombus(width) and @rhombus(height).

@examples(
  ~eval: radial_eval
  radial_pict()
  radial_pict(~fill: #'inherit).colorize("red")
  radial_pict(~width: 64, ~height: 32, ~rotate: math.pi * 1/2)
  radial_pict(~inner_pause: 0.5, ~outer_pause: 0.25, ~fill: "black")
  radial_pict(~outer_pull: 0.25, ~fill: "purple")
  radial_pict(~inner_pull: 0.25, ~fill: "forestgreen")
  radial_pict(~outer_pull: 0.25, ~inner_radius: -0.5, ~fill: "lightblue")
  beside(~sep: 16, ~vert: #'top,
         ellipse(~around: radial_pict(~points: 3)),
         rectangle(~around: radial_pict(~points: 3)),
         rectangle(~around: radial_pict(~points: 3, ~bound: #'shrink)),
         rectangle(~around: radial_pict(~points: 3, ~bound: #'stretch)))
)

}

@doc(
  annot.macro 'Radial'
  fun radial(
    ~points: n :: PosInt = 6,
    ~width: width :: Real = 64,
    ~height: height :: Real = width,
    ~rotate: rotate :: Real = 0,
    ~angle_at: angle_at :: Function.of_arity(2) = evenly_spaced,
    ~inner_radius: inner :: Real = 0.5,
    ~outer_radius: outer :: Real = 1,
    ~inner_pause: inner_pause :: Real = 0,
    ~outer_pause: outer_pause :: Real = 0,
    ~flat_inner_edge: flat_inner_edge = #false,
    ~flat_outer_edge: flat_outer_edge = #false,
    ~outer_pull: outer_pull :: Real = 0,
    ~inner_pull: inner_pull :: Real = 0,
  ) :: Radial
){

 Similar to @rhombus(radial_pict), but instead of a pict, produces a
 @rhombus(Radial, ~annot). A single @rhombus(Radial) can be converted to a pict
 with @rhombus(Radial.pict), but multiple radials can be combined using
 @rhombus(radials_pict).

 For example, @rhombus(gear) uses @rhombus(radials_pict) to combine gear
 arms with a hole when a non-zero hold is requested.

@examples(
  ~eval: radial_eval
  def r = radial(~points: 7)
  r.pict(~fill: "blue")
  r.pict(~line: "blue", ~line_width: 3)
)


}

@doc(
  fun star_radial(~points: n :: PosInt = 5, ....) :: Radial
  fun flash_radial(~bumps: n :: PosInt = 10, ....) :: Radial
  fun sun_radial(~rays: n :: PosInt = 10, ....) :: Radial
  fun flower_radial(~petals: n :: PosInt = 6, ....) :: Radial
  fun cloud_radial(~bumps: n :: PosInt = 6, ....) :: Radial
  fun regular_polygon_radial(~sides: n :: PosInt = 10, ....) :: Radial
  fun circle_radial(~sides: n :: PosInt = 10, ....) :: Radial
  fun gear_radial(~arms: n :: PosInt = 10, ....) :: Radial
){

 The same as @rhombus(radial), but with defaults for arguments so that
 the result looks like a flower, cloud, etc., and sometimes with an
 different keyword like @rhombus(~petals) instead of @rhombus(~points).

@examples(
  ~eval: radial_eval
  flower_radial(~petals: 5).pict(~fill: "pink")
)

}

@doc(
  method (radial :: Radial).pict(
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~bound: bound :: BoundingBoxMode = #'unit
  ) :: Pict
  method (radial :: Radial).path() :: Path
){

 Converts a @rhombus(Radial, ~annot) to a pict or a DC path.

 A DC path for a radial places the middle of the shape at the origin, so
 it extends up to half the shape's width in each direction horizontally,
 and up to half the shape's height in each direction vertically.

@examples(
  ~eval: radial_eval
  regular_polygon_radial().pict(~fill: "orange")
  regular_polygon_radial(~width: 64).path().bounding_box()
)

}

@doc(
  fun radials_pict(
    radial :: Radial, ...,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~bound: bound :: BoundingBoxMode = #'unit
  ) :: Pict
){

 Combines multiple @rhombus(Radial, ~annot)s to a pict. Nested radials
 creates holes in the same way as @rhombus(#'odd_even) polygon rendering.

@examples(
  ~eval: radial_eval
  radials_pict([regular_polygon_radial(~width: 64),
                gear_radial(~width: 52)],
               ~fill: "orange")
)

}

@doc(
  fun arrow(
    ~length: length :: Real = 64,
    ~breadth: breadth :: Real = length,
    ~tail: tail :: Real = 0.5,
    ~head: head :: Real = (if tail .= 0 | 1 | 0.5),
    ~indent: indent = (if tail .= 0 | 0.3 | 0),
    ~rotate: rotate :: Real = 0,
    ~fill: fill :: maybe(ColorMode) = #false,
    ~line: line :: maybe(ColorMode) = !fill && #'inherit,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~bound: bound :: ArrowBoundingBoxMode = #'unit
  ) :: Pict
  fun arrow_path(
    ~length: length :: Real = 64,
    ~breadth: breadth :: Real = length,
    ~tail: tail :: Real = 0.5,
    ~head: head :: Real = (if tail .= 0 | 1 | 0.5),
    ~indent: indent = (if tail .= 0 | 0.3 | 0),
    ~rotate: rotate :: Real = 0
  ) :: Path
){

 The @rhombus(arrow) and @rhombus(arrow_path) functons are not in the
 same category as other @rhombusmodname(pict/radial) functions, but they
 are related. The @rhombus(arrow) function produces a pict that draws as
 an arrow, and @rhombus(arrow_path) takes mostly the same arguments to
 produce a drawing path for an arrow.

 The @rhombus(length) argument determines the length of the arrow. The
 @rhombus(head) argument determines the fraction of @rhombus(length) that
 is used for the triangular head, while @rhombus(breadth) determines its
 breadth of that triangle in a direction perpendicular to the length. The
 @rhombus(tail) argument determines the thickness of the tail line as a
 fraction of @rhombus(breadth); if @rhombus(tail) is 0, then no tail is
 drawn (and @rhombus(head) defaults to @rhombus(1) in that case). The
 @rhombus(indent) argument determines a fraction of the head's length by
 which the the tail side of the arrow's is indented at its center,
 forming a barb along the line if @rhombus(indent) is positive or a more
 diamond-shaped head if @rhombus(indent) is negative.

 The arrow is rotated by @rhombus(rotate) radians. With a zero rotation,
 the arrow poitns to the right (i.e., toward a greater x-offset). The
 arrow is rotated around its center.

 The @rhombus(bound) argument determines the bounding bod of the arrow
 pict. The default @rhombus(#'unit) mode makes the bounds correspond to
 an unrotated arrow, and as the arrow is rotated around its center, it is
 likely to extend outside that box. The @rhombus(#'shrink) mode shrinks
 or extends the bounding box as needed to cover all of the corners of the
 arrow. The @rhombus(#'line) mode for @rhombus(bound) creates a bounding
 box that coveres just the tip of the arrow and the center of the ending
 edge of the tail.

@examples(
  ~eval: radial_eval
  arrow(~fill: "forestgreen")
  arrow(~tail: 0, ~fill: "blue")
)

}

@doc(
  fun evenly_spaced(i :: Int, out_of_n :: Int) :: Real
  fun jitter_spaced(jitter :: Real) :: Function.of_arity(2)
){

 Functions useful for @rhombus(~angle_at) arguments to
 @rhombus(radial_pict).

 The function returned by @rhombus(jitter_spaced) takes the angle that
 it would otherwise return an adjust it based on a @rhombus(math.sin) of
 the angle times @rhombus(jitter). The default @rhombus(~angle_at)
 argument for @rhombus(cloud) is @rhombus(jitter_spaced(0.3)).

@examples(
  ~eval: radial_eval
  radial_pict(~points: 5, ~angle_at: jitter_spaced(0.2))
)

}


@doc(
  annot.macro 'BoundingBoxMode'
){

 Satisfied by a bounding-box mode: @rhombus(#'unit),
 @rhombus(#'shrink), or @rhombus(#'stretch).

}

@doc(
  annot.macro 'ArrowBoundingBoxMode'
){

 Satisfied by a bounding-box mode for @rhombus(arrow): @rhombus(#'unit),
 @rhombus(#'shrink), or @rhombus(#'line).

}


@close_eval(radial_eval)
