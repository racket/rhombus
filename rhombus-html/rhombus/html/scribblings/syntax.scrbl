#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      html)

@title(~tag: "htmlstx"){HTML Syntax Objects}

An @deftech{HTML syntax object} encodes an HTML document as a
human-readable shrubbery form represented by a Rhombus syntax object.
The @rhombus(html.from_syntax) function converts an HTML syntax object to
a @rhombus(html.Document), and @rhombus(html.to_syntax) goes in the other
direction. HTML syntax objects can be produced from HTML directly with
@rhombus(html.read_syntax), @rhombus(html.string_to_syntax), and
@rhombus(html.bytes_to_syntax), and they can converted to XML directly
with @rhombus(html.write_syntax), @rhombus(html.syntax_to_string), and
@rhombus(html.syntax_to_bytes).
@//
@margin_note{HTML syntax objects are similar to XML syntax objects as
 supported by the @rhombusmodname(xml, ~indirect) library.}

The @rhombus(html.syntax) form is essentially an alias of @rhombus('')
(see @rhombus(#%quotes)), but it checks that the result syntax object
conforms to HTML syntax object constraints.

@examples(
  ~hidden:
    import:
      html
      rhombus/date
  ~defn:
    def my_doc:
      html.syntax:
        head:
          title:
            "Greeting"
        body:
          h1:
            "Hello, World!"
          div:
            ~class: "main"
            "Today is sometime after "
            $(to_string(date.Date.from_seconds(0, ~local: #false)))
  ~repl:
    html.write_syntax(my_doc)
)

@doc(
  expr.macro 'html.syntax:
                $content
                ...'

  grammar element
  | $id:
      $attribute
      ...
      $content
      ...

  grammar attribute
  | $keyword: $value_string

  grammar content
  | $element
  | $string
  | $content ...
  | ($content, ...)
  | [$content, ...]
  | $other
){

 Returns the same syntax object as @rhombus('#,(@rhombus(content)); ...'),
 but with the constraint that the result conforms to the grammar of an
 HTML syntax object. Although not shown above in the grammar for
 @rhombus(element), an escape using @rhombus($) can appear anywhere in
 the block after @rhombus(html.syntax), and it works the same as in
 @rhombus('#,(@rhombus(content)); ...') (see @rhombus(#%quotes)). Conformance
 is checked on the result after substituting escapes.

 An @rhombus(element) corresponds to an @rhombus(html.Element, ~class).
 It always starts with an identifier, and it is followed by a block
 (possibly empty) for the element's attributes and content. The
 attributes must appear first, where each attribute starts with a keyword
 and is followed by a block for the attribute's value, together
 corresponding to @rhombus(html.Attribute, ~class).

 Each @rhombus(content) can be one the following:

@itemlist(

 @item{@rhombus(element): Corresponds to a nested @rhombus(html.Element, ~class).}

 @item{@rhombus(string): Used directly as string content.}

 @item{@rhombus(content ...), @rhombus((content, ...)), or
  @rhombus([content, ...]): Content can be spliced from multiple
  @rhombus(content) forms with a group, within parentheses, or within
  square brackets. Note that this splicing accommodates @litchar("@")
  forms, as in the example below.}

 @item{@rhombus(other): Any value that is satisfies
  @rhombus(html.Content, ~annot) is allowed. For example, a
  @rhombus(html.Comment, ~class) value can appear here. Constructing such
  objects will require a @rhombus($) escape with @rhombus(Syntax.inject).}

)

@examples(
  ~hidden:
    import html
  ~repl:
    html.write_syntax(
      html.syntax:
        a: "hello <name>"
    )
    html.write_syntax(
      html.syntax:
        a: ("b", "c", [["d"], "e"])
    )
    html.write_syntax(
      html.syntax:
        a: @{say this: "hello <name>"}
    )
    html.write_syntax(
      html.syntax:
        a: $(Syntax.inject(html.Comment(~data: "123")))
    )
)

}

@doc(
  ~nonterminal:
    content: html.syntax
  expr.macro 'html.doc:
                $content
                ...'
){

 Like @rhombus(html.syntax), but converts the HTML syntax object to a
 @rhombus(html.Document) using @rhombus(html.from_syntax).

}

@doc(
  fun html.from_syntax(
    html_stx :: Syntax,
  ) :: html.Document
  fun html.to_syntax(
    doc :: html.Document || html.Content,
  ) :: Syntax
){

 Converts to and from the @tech{HTML syntax object} representation.

}

@doc(
  fun html.read_syntax(
    ~in: inp :: Port.Input = Port.Input.current()
  ) :: Syntax
  fun html.write_syntax(
    html_stx :: Syntax,
    ~out: outp :: Port.Output = Port.Output.current(),
    ~newlines: add_newlines :: NewlineMode = #'safe
  ) :: Void
){

 Reads or prints HTML directly from the @tech{HTML syntax object}
 representation.

}

@doc(
  fun html.syntax_to_string(
    html_stx :: Syntax,
    ~newlines: add_newlines :: NewlineMode = #'safe
  ) :: String
  fun html.string_to_syntax(
    html_str :: String,
  ) :: Syntax
  fun html.syntax_to_bytes(
    html_stx :: Syntax,
    ~newlines: add_newlines :: NewlineMode = #'safe
  ) :: Bytes
  fun html.bytes_to_syntax(
    html_bstr :: Bytes,
  ) :: Syntax
){

 Converts directly between HTML in a string or byte string and a
 @tech{HTML syntax object} representation.

}
