#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Paged Drawing Contexts}

@(~version_at_least "8.14.0.4")

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
  enum PaperSize:
    ~is_a SizeLike
    paper
){

 A size specification to be used with @rhombus(PSDC, ~class) and
 @rhombus(PDFDC, ~class), which allow @rhombus(#'paper) to specify the
 size indirectly as the current paper configuration's size.

}
