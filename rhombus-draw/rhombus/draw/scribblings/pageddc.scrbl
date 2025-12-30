#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Paged Drawing Contexts}

@doc(
  interface draw.PagedDC
){

 Drawing contexts that output to a file format, drawing must be
 explicitly started with @rhombus(PagedDC.start) and ended with
 @rhombus(PagedDC.end).

}


@doc(
  method (dc :: PagedDC).start(message :: String = "Printing")
    :: Void
  method (dc :: PagedDC).end() :: Void

  method (dc :: PagedDC).start_doc(message :: String = "Printing")
    :: Void
  method (dc :: PagedDC).end_doc() :: Void

  method (dc :: PagedDC).start_page() :: Void
  method (dc :: PagedDC).end_page() :: Void
){

 Use @rhombus(PagedDC.start) and @rhombus(PagedDC.end) to bound all
 drawing for single-page output.

 Use the more fine-grained combination of @rhombus(PagedDC.start_doc)
 and @rhombus(PagedDC.start_page) for multi-page output, where
 @rhombus(PagedDC.end_page) ends a page and then
 @rhombus(PagedDC.start_page) can be used again to start the next page.
 After the last page's @rhombus(PagedDC.end_page), use
 @rhombus(PagedDC.end_doc) to finish the document.

}

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

@doc(
  enum PaperSize
  | ~is_a SizeLike
  | paper
){

 A size specification to be used with @rhombus(PSDC, ~class) and
 @rhombus(PDFDC, ~class), which allows @rhombus(#'paper) to specify the
 size indirectly as the current paper configuration's size.

}
