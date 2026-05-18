#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Recording Drawing Contexts}

@doc(
  class draw.RecordDC():
    implements DC
    constructor (
      size :: SizeLike,
      ~record_ink: record_ink :: Any.to_boolean = #false
    )
){

 Creates a drawing context that records drawing operations for replay
 later. The @rhombus(size) argument merely determines the result of
 @rhombus(DC.size), and it does not clip drawing operations.

 If @rhombus(record_ink) is true, then drawing records ink effects so
 that @rhombus(RecordDC.ink_extent) can report ink bounds.

 Use @rhombus(RecordDC.get_draw) to get a drawing function that replays
 the recorded drawing, or use @rhombus(RecordDC.get_record) to get a
 value that can be serialized, deserialized, and converted to a drawing
 function with @rhombus(RecordDC.record_to_draw).

}

@doc(
  method (dc :: draw.RecordDC).get_draw()
    :: DC -> Void
){

 Returns a drawing function that plays a recorded drawing (as of the
 point where @rhombus(RecordDC.get_draw) is called) onto a given
 @rhombus(DC, ~class). The function can be called any number of times.

 When drawing recorded actions, the target drawing context's pen, brush,
 font, text, background, text background, and text foreground do not
 affect the recorded actions. The target drawing context’s
 transformation, alpha, and clipping region compose with settings in the
 recorded actions (so that, for example, a recorded action to set the
 clipping region actually intersects the region with the drawing
 context's clipping region at the time that the recorded commands are
 replayed). After recoded commands are replayed, all settings in the
 target drawing context, such as its clipping region or current font, are
 as before the replay.

}

@doc(
  method (dc :: draw.RecordDC).get_record() :: Any
  fun draw.RecordDC.record_to_draw(rec :: Any) :: DC -> Void
){

 The @rhombus(RecordDC.get_record) method returns a
 @tech(~doc: ref_doc){serializable} value that represents a recorded
 drawing (as of the point where @rhombus(RecordDC.get_record) is called).
 The @rhombus(RecordDC.record_to_draw) function takes such a value and
 returns a function that performs the drawing into a given
 @rhombus(DC, ~class).

 A drawing function returned by @rhombus(RecordDC.record_to_draw)
 behaves like one from @rhombus(RecordDC.get_draw).

}

@doc(
  method (dc :: draw.RecordDC).ink_extent() :: Rect
){

 Returns a rectangle that bounds all
 drawing into @rhombus(dc), as long as ink recording was enabled through
 the @rhombus(~record_ink) constructor argument for @rhombus(RecordDC).

 Bounding drawing ``ink'' takes into account the visible effect of
 drawing with different pen widths and the shape of drawn text, as
 opposed to just collecting path coordinates and nominal text extents.

 If ink record is not enabled for @rhombus(dc),
 @rhombus(RecordDC.ink_extent) throws an exception.

}
