#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{SVG Drawing Context}

@(~version_at_least "8.14.0.4")

@doc(
  class draw.SVGDC():
    implements PagedDC
    constructor (
      size :: SizeLike,
      ~output: output :: Path || Port.Output
    )
){

 Creates a drawing context that produces SVG output.  Be sure to
 use @rhombus(PagedDC.start) and @rhombus(PagedDC.end) around all
 drawing.

}
