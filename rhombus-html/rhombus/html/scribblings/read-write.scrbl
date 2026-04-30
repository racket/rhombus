#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      html)

@title(~tag: "read-write"){Reading and Writing HTML}

@doc(
  fun html.read(
    ~in: inp :: Port.Input = Port.Input.current(),
  ) :: html.Document
  fun html.write(
    content :: html.Document || html.Content,
    ~out: outp :: Port.Output = Port.Output.current(),
    ~newlines: newlines :: html.NewlineMode = #'safe
  ) :: Void
){

 Reads an HTML document into an @rhombus(html.Document, ~class) object or
 writes an @rhombus(html.Document, ~class) or
 @rhombus(html.Content, ~annot) object as an HTML document. Supplying
 @rhombus(html.Content, ~annot) to @rhombus(html.write) is a shortcut for
 supplying a document that contains just the content.

 By default, @rhombus(html.write) add newlines in a way that cannot
 change the meaning of the HTML document: by placing newlines just before
 the ending @litchar{>} of some closing tags. See
 @rhombus(html.NewlineMode, ~annot) for details.

}

@doc(
  fun html.from_string(str :: String) :: html.Document
  fun html.from_bytes(bstr :: Bytes) :: html.Document
  fun html.to_string(
    doc :: html.Document || html.Content,
    ~newlines: newlines :: html.NewlineMode = #'safe
  ) :: String
  fun html.to_bytes(
    doc :: html.Document || html.Content,
    ~newlines: newlines :: html.NewlineMode = #'safe
  ) :: Bytes
){

 Shortcuts for @rhombus(html.read) and @rhombus(html.write) to read or
 write HTML documents as strings or byte strings.

}

@doc(
  enum html.NewlineMode
  | none
  | safe
  | pretty
){

 Newline-insertion modes for @rhombus(html.write):

@itemlist(

 @item{@rhombus(#'none): No extra newlines are added.}

 @item{@rhombus(#'safe): Newlines are added to the closing tag of some
  elements, just before the ending @litchar{>}. This newline will always
  be ignored when the generated HTML format is parsed again. A newline is
  added only for elements using one of the names in
  @rhombus({ "head", "body", "meta", "script", "td", "tr",
             "div", "li", "h1", "h2", "h3", "h4", "h5", "h6" }).}

 @item{@rhombus(#'pretty): Newlines are added between elements where it
  is expected to not change the rendering of the document, but this
  expectation is potentially wrong depending on CSS styling. A newline is
  added between immediate elements in a @rhombus("head") element, a
  newline is added between elements that have the same name among the
  names in @rhombus({ "td", "tr", "div", "li" }), and a newline is added
  at the end of the document.}

)

}
