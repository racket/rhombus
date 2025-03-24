#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      xml)

@title(~tag: "xmlstx"){XML Syntax Objects}

An @deftech{XML syntax object} encodes an XML document as a
human-readable shrubbery form represented by a Rhombus syntax object.
The @rhombus(xml.from_syntax) function converts an XML syntax object to
a @rhombus(xml.Document), and @rhombus(xml.to_syntax) goes in the other
direction. XML syntax objects can be produced from XML directly with
@rhombus(xml.read_syntax), @rhombus(xml.string_to_syntax), and
@rhombus(xml.bytes_to_syntax), and they can converted to XML directly
with @rhombus(xml.write_syntax), @rhombus(xml.syntax_to_string), and
@rhombus(xml.syntax_to_bytes).

The @rhombus(xml.syntax) for is essentially an alias of @rhombus('')
(see @rhombus(#%quotes)), but it checks that the result syntax object
conforms to XML syntax object constraints.

@examples(
  ~hidden:
    import xml
  ~defn:
    def my_doc:
      xml.syntax:
        words:
          greeting:
            ~weight: "bold"
            "hello"
          parting:
            ~weight: "normal"
            ~size: $(to_string(2 * 12))
            "bye"
  ~repl:
    xml.write_syntax(my_doc, ~indentation: #'peek)
)

XML application vary in whether tag and attribute names contain
@litchar{-}, @litchar{_}, or both. XML syntax objects use shrubbery
identifiers and keywords for tags and attributes, so writing names with
@litchar{-} would normally require escapes. To improve usability for
contexts where @litchar{-} names are common, conversion from XML syntax
objects to a @rhombus(xml.Document) or to XML can optionally swap
@litchar{-} and @litchar{_} characters in tags and attribute names. For
example, @rhombus(xml.from_syntax) accepts a
@rhombus(~swap_underscore_dash) argument.

@doc(
  expr.macro 'xml.syntax:
                $element'

  grammar element:
    $id:
      $attribute
      ...
      $content
      ...

  grammar attribute:
    $keyword: $value_string

  grammar content:
    $element
    $string
    #,(@rhombus(&, ~datum)) $entity_id_or_int
    $content ...
    ($content, ...)
    [$content, ...]
    $other
){

 Returns the same syntax object as @rhombus('#,(@rhombus(element))'),
 but with the constraint that the result conforms to the grammar of an
 XML syntax object. Although not shown above in the grammar for
 @rhombus(element), an escape using @rhombus($) can appear anywhere in
 the block after @rhombus(xml.syntax), and it works the same as in
 @rhombus('#,(@rhombus(element))') (see @rhombus(#%quotes)). Conformance
 is checked on the result after substituting escapes.

 An @rhombus(element) corresponds to an @rhombus(xml.Element, ~class).
 It always starts with an identifier, and it is followed by a block
 (possibly empty) for the element's attributes and content. The
 attributes must appear first, where each attribute starts with a keyword
 and is followed by a block for the attribute's value, together
 corresponding to @rhombus(xml.Attribute, ~class).

 Each @rhombus(content) can be one the following:

@itemlist(

 @item{@rhombus(element): Corresponds to a nested @rhombus(xml.Element, ~class).}

 @item{@rhombus(string): Corresponds to @rhombus(xml.Text, ~class).}

 @item{@rhombus(&, ~datum): Corresponds  to @rhombus(xml.Entity, ~class).}

 @item{@rhombus(content ...), @rhombus((content, ...)), or
  @rhombus([content, ...]): Content can be spliced from multiple
  @rhombus(content) forms with a group, within parentheses, or within
  square brackets. Note that this splicing accommodates @litchar("@")
  forms, as in the example below.}

 @item{@rhombus(other): Any value that is satisfies
  @rhombus(Content, ~annot) is allowed. For example, a
  @rhombus(xml.Comment, ~class) value can appear here. Constructing such
  objects will require a @rhombus($) escape with @rhombus(Syntax.inject).}

)

@examples(
  ~hidden:
    import xml
  ~repl:
    xml.write_syntax(
      xml.syntax:
        a: "hello <name>"
    )
    xml.write_syntax(
      xml.syntax:
        a: &something
    )
    xml.write_syntax(
      xml.syntax:
        a: ("b", "c", [["d"], "e"])
    )
    xml.write_syntax(
      xml.syntax:
        a: @{say this: "hello <name>"}
    )
    xml.write_syntax(
      xml.syntax:
        a: $(Syntax.inject(xml.Text(~text: "123", ~write_mode: #'cdata)))
    )
)

}

@doc(
  ~nonterminal:
    element: xml.syntax
  expr.macro 'xml.doc:
                $option
                ...
                $element'

  grammar option:
    ~swap_underscore_dash: $swap_expr
){

 Like @rhombus(xml.syntax), but converts the XML syntax object to a
 @rhombus(xml.Document) using @rhombus(xml.from_syntax). The value of a
 @rhombus(swap_expr) option is used as the
 @rhombus(~swap_underscore_dash) argument to @rhombus(xml.from_syntax).

}

@doc(
  fun xml.from_syntax(
    xml_stx :: Syntax,
    ~swap_underscore_dash: swap :: Any.to_boolean() = #false
  ) :: xml.Document
  fun xml.to_syntax(
    doc :: xml.Document || xml.Element,
    ~swap_underscore_dash: swap :: Any.to_boolean() = #false
  ) :: Syntax
){

 Converts to and from the @tech{XML syntax object} representation.

}

@doc(
  fun xml.read_syntax(
    ~in: inp :: Port.Input = Port.Input.current()
  ) :: Syntax
  fun xml.write_syntax(
    xstx :: Syntax,
    ~swap_underscore_dash: swap = default_swap,
    ~out: outp :: Port.Output = Port.Output.current(),
    ~indentation: indentation :: xml.Indentation = #'none
  ) :: Void  
){

 Reads or prints XML directly from the @tech{XML syntax object}
 representation.

}

@doc(
  fun xml.syntax_to_string(
    xml_stx :: Syntax,
    ~swap_underscore_dash: swap :: Any.to_boolean() = #false
  ) :: String
  fun xml.string_to_syntax(
    xml_str :: String,
    ~swap_underscore_dash: swap :: Any.to_boolean() = #false
  ) :: Syntax
  fun xml.syntax_to_bytes(
    xml_stx :: Syntax,
    ~swap_underscore_dash: swap :: Any.to_boolean() = #false
  ) :: Bytes
  fun xml.bytes_to_syntax(
    xml_bstr :: Bytes,
    ~swap_underscore_dash: swap :: Any.to_boolean() = #false
  ) :: Syntax
){

 Converts directly between XML in a string or byte string and a
 @tech{XML syntax object} representation.

}
