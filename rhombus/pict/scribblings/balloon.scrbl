#lang scribble/rhombus/manual

@(import:
    meta_label:
      rhombus open
      pict
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
    ~find: find_mode :: FindMode = #'always,
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
    ~fill: fill :: maybe(ColorMode) = current_color(),
    ~line: line :: maybe(ColorMode) = #false,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~epoch: epoch :: EpochAlignment = #'center,
    ~duration: duration :: DurationPinAlignment = #'sustain
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

 The @rhombus(duration) argument is used in a way analogous to
 @rhombus(pict.overlay), which can insert a balloon with @rhombus(content)
 into a part of the timeline of @rhombus(base).

 The @rhombus(find_mode) argument is used in a way analogous to
 @rhombus(pict.pin), where @rhombus(#'maybe) mode means that a balloon is
 not pinned when @rhombus(finder) fails to find a location.

 When the resulting pict's @tech{time box} is padded (as opposed to
 @tech{sustain}ed), then @rhombus(content) is still pinned onto
 @rhombus(on_pict) as during the time box, but no balloon is drawn behind
 @rhombus(content).

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
  Parameter.def current_color :: Color || String
  Parameter.def current_horiz_margin :: Real
  Parameter.def current_vert_margin :: Real
){

 Context parameters for argument defaults in @rhombus(pin).

}

@doc(
  annot.macro 'CompassDirection'
){

 Matches a symbol for a compass direction, one of @rhombus(#'n),
 @rhombus(#'ne), @rhombus(#'e), @rhombus(#'se), @rhombus(#'s),
 @rhombus(#'sw), @rhombus(#'w), or @rhombus(#'nw). The north direction,
 @rhombus(#'n), corersponds to a negative y-offset.

}
