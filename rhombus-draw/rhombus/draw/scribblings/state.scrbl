#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "state"){Drawing State}

@seclink("draw"){Drawing operations} on a @rhombus(DC, ~class) depend on
its current pen, brush, and other state.

@doc(
  property
  | (dc :: draw.DC).pen :: Pen
  | (dc :: draw.DC).pen := (p :: Pen)
  property
  | (dc :: draw.DC).brush :: Brush
  | (dc :: draw.DC).brush := (b :: Brush)
){

 The current pen used for lines or shape outlines, and the current brush
 used for shape fills.

}

@doc(
  property
  | (dc :: draw.DC).font :: Font
  | (dc :: draw.DC).font := (f :: Font)
  property
  | (dc :: draw.DC).text_color :: Color
  | (dc :: draw.DC).text_color := (c :: String || Color)
){

 The current font and color used for drawing text.

}

@doc(
  property
  | (dc :: draw.DC).clipping_region :: maybe(Region)
  | (dc :: draw.DC).clipping_region := (rgn :: maybe(Region))
){

 The current clipping region, or @rhombus(#false) if no clipping is in
 effect. Drawing effects that would fall outside the current clipping
 region are discarded.

}

@doc(
  property
  | (dc :: draw.DC).alpha :: Real.in(0, 1)
  | (dc :: draw.DC).alpha := (a :: Real.in(0, 1))
){

 The current opacity used for drawing, where @rhombus(0.0) is fully
 transparent, so it causes no effect on the target, and @rhombus(1.0) is
 fully opaque. A pen or brush's color can have its own opacity, which is
 multiplied by the drawing context's opacity. Similarly, a drawn bitmap
 can have its own alpha channel, which is multiplied by the drawing
 context's opacity.

}

@doc(
  method (dc :: draw.DC).start_alpha(alpha :: Real.in(0, 1)) :: Void
  method (dc :: draw.DC).end_alpha() :: Void
  dot (dc :: draw.DC).using_alpha (alpha :: Real.in(0, 1)):
    $body
    ...
){

 The @rhombus(DC.start_alpha) method starts a compositing drawing
 sequence that is not rendered until @rhombus(DC.end_alpha) is called. At
 that point, the accumulated sequence is conceptually rendered to a
 separate context, and then transferred at once with opacity times the
 current opacity as produced by @rhombus(DC.alpha). The
 @rhombus(DC.start_alpha) call meanwhile sets the current opacity to
 @rhombus(1.0), and @rhombus(DC.end_alpha) restores the drawing context’s
 opacity to the setting before @rhombus(DC.start_alpha).

 This effect is different than setting @rhombus(DC.alpha) (times the
 current @rhombus(DC.alpha) result) in the case that drawing between
 @rhombus(DC.start_alpha) and @rhombus(DC.end_alpha) produces overlapping
 output. In that case, adjusting @rhombus(DC.alpha) would affect the
 drawing operations separately, while @rhombus(DC.start_alpha) creates an
 opacity adjustment on the overlapped result, instead.

 The @rhombus(DC.using_alpha) form returns the result of the
 @rhombus(body) sequence, but adds a @rhombus(DC.start_alpha(alpha)) call
 before and @rhombus(DC.end_alpha()) call afterward.

}

@doc(
  property
  | (dc :: draw.DC).transformation :: Transformation
  | (dc :: draw.DC).transformation := (t :: Transformation)
  property
  | (dc :: draw.DC).layered_transformation :: LayeredTransformation
  | (dc :: draw.DC).layered_transformation := (lt :: LayeredTransformation)
){

 The current transformation that is applied to any drawing coordinate
 to arrive at coordinated in the target (such as a bitmaps).

 Beware that getting and setting @rhombus(DC.transformation) does not
 save and restore precisely the same internal transformation state of
 @rhombus(dc). For cases where that is needed,
 @rhombus(DC.layered_transformation) produces and consumes an opaque
 value representing the internal state The @rhombus(DC.save) and
 @rhombus(DC.restore) methods use @rhombus(DC.layered_transformation),
 for example.

}

@doc(
  method (dc :: draw.DC).scale(s :: Real) :: Void
  method (dc :: draw.DC).scale(sx :: Real, sy :: Real) :: Void
  method (dc :: draw.DC).translate(dpt :: PointLike) :: Void
  method (dc :: draw.DC).translate(dx :: Real, dy :: Real) :: Void
  method (dc :: draw.DC).rotate(radians :: Real) :: Void
  method (dc :: draw.DC).transform(t :: Transformation) :: Void
){

 Applies a (further) transformation to the drawing context's conversion
 from drawing coordinates to device coordinates. In other words, these
 methods change the result that is returned by the
 @rhombus(DC.transformation) property, and they affect drawing
 accordingly.

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
  property
  | (dc :: draw.DC).smoothing :: Smoothing
  | (dc :: draw.DC).smoothing := (s :: Smoothing)

  property
  | (dc :: draw.DC).alignment_scale :: PosReal
  | (dc :: draw.DC).alignment_scale := (s :: PosReal)

  enum draw.DC.Smoothing
  | smoothed
  | unsmoothed
  | aligned
){

 A drawing context's smoothing mode (the @rhombus(DC.smoothing)
 property) determines whether drawing coordinates are aligned to its
 alignment scale (the @rhombus(DC.alignment_scale) property) and whether
 anti-aliasing is used to make shapes appear smoother:

@itemlist(

 @item{@rhombus(#'smoothed): The default smoothing mode, which uses
  anti-aliasing and does not adjust drawing coordinates to align them.}

 @item{@rhombus(#'unsmoothed): Draws without anti-aliasing. Drawing
  coordinates are rounded to the nearest pixel after the alignment scale,
  after taking into account scaling after the drawing context's
  @rhombuslink(DC.transformation){current transformation}.}

 @item{@rhombus(#'aligned): Draws with anti-aliasing, but drawing
  coordinates are rounded to the nearest pixel after the alignment scale,
  after taking into account scaling after the drawing context's
  @rhombuslink(DC.transformation){current transformation}.}

)

 An alignment scale of @rhombus(2.0) aligns drawing coordinates to
 half-integer values. A value of @rhombus(2.0) could be suitable for
 drawing to a bitmap with a backing scale of @rhombus(2.0), since
 half-integer values correspond to pixel boundaries. Even when a
 destination bitmap has a backing scale of @rhombus(2.0), however, an
 alignment scale of @rhombus(1.0) may be desirable to maintain
 consistency with drawing contexts that have a backing scale and
 alignment scale of @rhombus(1.0).

}
