#lang scribble/rhombus/manual

@(import:
    meta_label:
      rhombus open
      pict open:
        except:
          pin
      pict/balloon
      pict/balloon open)

@(def ballooon_eval = make_rhombus_eval())
@examples(
  ~eval: ballooon_eval
  ~hidden:
    import:
      pict open
      pict/balloon
)

@title(~tag: "balloon"){Pict Speech Balloons}

@docmodule(pict/balloon)

@doc(
  fun pin(
    content :: Pict,
    ~on: base :: Pict,
    ~at: finder :: Find,
    ~spike: spike :: CompassDirection,
    ~margin: margin :: maybe(Real) = #false,
    ~horiz_margin: hmargin :: Real = margin || current_horiz_margin(),
    ~vert_margin: vmargin :: Real = margin || current_vert_margin(),
    ~corner_radius: corner_radius :: Real = 10,
    ~spike_radius: spike_radius :: Real = corner_radius,
    ~dx: dx :: Real = spike_to_dx(spike),
    ~dy: dy :: Real = spike_to_dy(spike),
    ~sprout: sprout :: Real = 0.5,
    ~thought: thought = #false,
    ~fill: fill :: MaybeColor = current_color(),
    ~line: line :: MaybeColor = #false,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~epoch: epoch :: EpochAlignment = #'center,
    ~duration: duration :: DurationAlignment = #'sustain
  ) :: Pict
){

 The @rhombus(pin) function (intended to be used as
 @rhombus(balloon.pin)) wraps a cartoon speech balloon around
 @rhombus(content), extends a pointer from the balloon in the direction
 indicated by @rhombus(spike), and pins the balloon onto @rhombus(base)
 so that the point is at the location specified by @rhombus(finder).

 When @rhombus(content) is @rhombus(nothing), then @rhombus(base) is
 returned as-is. When @rhombus(base) is @rhombus(nothing), then the
 result is @rhombus(nothing).

 The @rhombus(sprout) argument applies only when @rhombus(spike) is
 @rhombus(#'n), @rhombus(#'s), @rhombus(#'e), or @rhombus(#'w). In that
 case, @rhombus(sprout) determines a position along the corresponding
 endge of the balloon as a fraction of the edge running left-to-right or
 top-to-bottom.

}

@doc(
  fun spike_to_dx(spike :: CompassDirection) :: Real
  fun spike_to_dy(spike :: CompassDirection) :: Real
){

 Reports the default spike offset for a given direction. The offset is
 relative to the point on the boundary of a balloon where the spike
 emerges.

}

@doc(
  def current_color :: Parameter
  fun current_color() :: Color || String
  fun current_color(color :: Color || String) :: Void

  def current_horiz_margin :: Parameter
  fun current_horiz_margin() :: Real
  fun current_horiz_margin(margin :: Real)

  def current_vert_margin :: Parameter
  fun current_vert_margin() :: Real
  fun current_vert_margin(margin :: Real)
){

 Parameters for argument defaults in @rhombus(pin).

}

@doc(
  annot.macro 'CompassDirection'
){

 Matches a symbol for a compass direction, one of @rhombus(#'n),
 @rhombus(#'ne), @rhombus(#'e), @rhombus(#'se), @rhombus(#'s),
 @rhombus(#'sw), @rhombus(#'w), or @rhombus(#'nw). The north direction,
 @rhombus(#'n), corersponds to a negative y-offset.

}
