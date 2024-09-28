#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{PostScript (PS) Drawing Context}

@(~version_at_least "8.14.0.4")

@doc(
  class draw.PSDC():
    implements PagedDC
    constructor (
      size :: PaperSize,
      ~output: output :: Path || Port.Output,
      ~as_eps: as_eps = #true
    )
){

 Creates a drawing context that produces PostScript output. Be sure to
 use @rhombus(PagedDC.start) and @rhombus(PagedDC.end) around all
 drawing.

}
