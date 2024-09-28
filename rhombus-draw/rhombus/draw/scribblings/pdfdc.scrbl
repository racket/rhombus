#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{PDF Drawing Context}

@(~version_at_least "8.14.0.4")

@doc(
  class draw.PDFDC():
    implements PagedDC
    constructor (
      size :: PaperSize,
      ~output: output :: Path || Port.Output
    )
){

 Creates a drawing context that produces PDF output.  Be sure to
 use @rhombus(PagedDC.start) and @rhombus(PagedDC.end) around all
 drawing.

}
