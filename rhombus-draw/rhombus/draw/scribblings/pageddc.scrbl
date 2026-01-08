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
  method (dc :: draw.PagedDC).start(msg :: String = "Printing")
    :: Void
  method (dc :: draw.PagedDC).end() :: Void

  method (dc :: draw.PagedDC).start_doc(msg :: String = "Printing")
    :: Void
  method (dc :: draw.PagedDC).end_doc() :: Void

  method (dc :: draw.PagedDC).start_page() :: Void
  method (dc :: draw.PagedDC).end_page() :: Void
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
      size :: PagedDC.PaperSize,
      ~output: output :: Path || Port.Output,
      ~config: config :: PagedDC.Config
                 = PagedDC.Config.current()
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
      size :: PagedDC.PaperSize,
      ~output: output :: Path || Port.Output,
      ~as_eps: as_eps = #true,
      ~config: config :: PagedDC.Config
                 = PagedDC.Config.current()
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
  enum draw.PagedDC.PaperSize
  | ~is_a SizeLike
  | paper
){

 A size specification to be used with @rhombus(PSDC, ~class) and
 @rhombus(PDFDC, ~class), which allows @rhombus(#'paper) to specify the
 size indirectly as the current paper configuration's size.

}

@doc(
  class draw.PagedDC.Config(
    ~margin: margin :: SizeLike.to_size = [16.0, 16.0],
    ~editor_margin: editor_margin :: SizeLike.to_size = [20.0, 20.0],
    ~translate: translate :: PointLike.to_point = [0.0, 0.0],
    ~scale: scale :: SizeLike.to_size = [0.8, 0.8],
    ~orientation: orientation :: PagedDC.Orientation = #'portrait,
    ~paper: paper :: PagedDC.Paper = #'letter
  )
  Parameter.def draw.PagedDC.Config.current
    :: PagedDC.Config = PagedDC.Config()
){

 A @rhombus(PagedDC.Config) object describes a page configuration to be
 used by a @rhombus(PDFDC) or @rhombus(PSDC) drawing context.

 Most fields affect all drawing, but @rhombus(editor_margin) has no
 direct effect. The @rhombus(editor_margin) field of the current
 configuration can be checked when printing a text editor's content, for
 example.

}

@doc(
  property (config :: draw.PagedDC.Config).handle :: Any
  fun draw.PagedDC.Config.from_handle(hand :: Any) :: PagedDC.Config
){

 Converts between page-configuration objects and Racket-level
 configuration objects.

}
