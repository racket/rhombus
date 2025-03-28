#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      xml)

@title(~tag: "class"){XML Document Representation}

@doc(
  class xml.Document(~prolog: prolog :: xml.Prolog = xml.Prolog(),
                     ~element: element :: xml.Element,
                     ~misc: misc :: List.of(xml.Misc) = [])
){

 Represents an XML document, which always contains a single element. A
 document optionally contains a document-type descriptor in the prolog,
 and it may have miscellaneous comments and processing instructions
 after the element.

}

@doc(
  class xml.Element(~name: name :: String,
                    ~attributes: attributes :: List.of(xml.Attribute) = [],
                    ~content: content :: List.of(xml.Content) = [],
                    ~srcloc: srcloc :: maybe(xml.SrclocRange) = #false)

  class xml.Attribute(~name: name :: String,
                      ~value: value :: String || xml.OtherPermitted,
                      ~srcloc: srcloc :: maybe(xml.SrclocRange) = #false)

  annot.macro 'xml.Content'
){

 An @rhombus(xml.Element) represents an XML term of them form
 @litchar{<}@italic{tag}…@litchar{>}…@litchar{</}@italic{tag}@litchar{>},
 where @rhombus(name) field corresponds to @italic{tag},
 @rhombus(attributes) correspond to attributes within
 @litchar{<}@italic{tag}…@litchar{>}, and @rhombus(content) is the
 content between @litchar{<}@italic{tag}…@litchar{>} and
 @litchar{</}@italic{tag}@litchar{>}.

 The @rhombus(xml.Content, ~annot) annotation recognizes allowed content
 values:

@itemlist(

 @item{@rhombus(xml.Text, ~class): Text, potentially written with
  @litchar{<![CDATA[}…@litchar{]]>} or @litchar{&}…@litchar{;}, depending
  on @rhombus(xml.Text.write_mode).}

 @item{@rhombus(xml.Entity, ~class): Entities written with
  @litchar{&}…@litchar{;}. See @rhombus(xml.Entity, ~class) form
  information about when @rhombus(xml.read) produces
  @rhombus(xml.Entity, ~class) objects.}

 @item{@rhombus(xml.Inject, ~class): Text, possibly non-conforming, that
  is written verbatim by @rhombus(xml.write). The @rhombus(xml.read)
  function never produces this form of content.}

 @item{@rhombus(xml.Comment, ~class): A comment written with
  @litchar{<!--}…@litchar{-->}, produced by @rhombus(xml.read) only when
  @rhombus(xml.current_read_comments) is @rhombus(#true).}

 @item{@rhombus(xml.ProcessingInstruction, ~class): A processing
  instruction written with @litchar{<?}…@litchar{?>}, produced by
  @rhombus(xml.read) only when
  @rhombus(xml.current_read_processing_instructions) is @rhombus(#true).}

 @item{@rhombus(xml.OtherPermitted, ~annot): Any other value, but only
  when @rhombus(xml.current_permissive) is @rhombus(#true).}

)

}

@doc(
  class xml.Text(~text: text :: String,
                 ~srcloc: srcloc :: maybe(xml.SrclocRange) = #false,
                 ~write_mode: write_mode :: xml.Text.WriteMode = #'default)
  enum xml.Text.WriteMode:
    default
    cdata
    entity
){

 Represents text for the content of an @rhombus(xml.Element).

 The @rhombus(write_mode) field is used by @rhombus(xml.write) to select
 an encoding of the text:

@itemlist(

 @item{@rhombus(#'default): Text is written using predefined entities as
  necessary to escape special characters, verbatim otherwise.}

 @item{@rhombus(#'cdata): Text is written using
  @litchar{<![CDATA[}…@litchar{]]>}. A @rhombus(xml.Text, ~class) object
  can be created with this mode only with @rhombus(text) that does not
  contain a @litchar{]]>}.}

 @item{@rhombus(#'cdata): Text is written using an integer entity form
  @litchar{&#}…@litchar{;}. A @rhombus(xml.Text, ~class) object can be
  created with this mode only with @rhombus(text) that contains a single
  character.}

)

}

@doc(
  class xml.Entity(~text: text :: String || xml.EntityInt,
                   ~srcloc: srcloc :: maybe(xml.SrclocRange) = #false)

  annot.macro 'xml.EntityInt'
){

 Represents entities written with @litchar{&}…@litchar{;} as the content
 of an @rhombus(xml.Element).

 Reading with normalization (see @rhombus(xml.current_read_normalize))
 produces this form only for symbolic entities, and reading always uses
 @rhombus(xml.Text, ~class) instead for the predefined entities
 @litchar{&amp;}, @litchar{&lt;}, @litchar{&gt;}, @litchar{&apos;}, and
 @litchar{&quot;}.

}

@doc(
  class xml.Inject(~text: text :: String,
                   ~srcloc: srcloc :: maybe(xml.SrclocRange) = #false)

){

 Represents text as the content of an
 @rhombus(xml.Element) that is written verbatim by @rhombus(xml.write).
 The text might not conform to XML syntax. A @rhombus(xml.read) never
 produces this form of content.

}

@doc(
  class xml.Comment(~text: text :: String)

  class xml.ProcessingInstruction(
    ~target_name: target_name :: String,
    ~instruction: instruction :: String,
    ~srcloc: srcloc :: maybe(xml.SrclocRange) = #false
  )

  annot.macro 'xml.Misc'
){

 The @rhombus(xml.Misc, ~annot) annotation recognizes
 @rhombus(xml.Comment, ~class) and
 @rhombus(xml.ProcessingInstruction, ~class) instances. These values can
 appear with an @rhombus(xml.Element) as content, or they can appear
 before or after the element of a @rhombus(xml.Document).

 @rhombus(xml.Comment, ~class) represents comment written with
 @litchar{<!--}…@litchar{-->}. Comments are discarded by
 @rhombus(xml.read) unless @rhombus(xml.current_read_comments) is
 @rhombus(#true).

 @rhombus(xml.ProcessingInstruction, ~class) represents a processing
 instruction written with @litchar{<?}…@litchar{?>}. Comments are discarded by
 @rhombus(xml.read) unless @rhombus(xml.current_read_processing_instructions) is
 @rhombus(#true).

}

@doc(
  annot.macro 'xml.OtherPermitted'

  Parameter.def xml.current_permissive :: Any.to_boolean:
    #false
){

 When @rhombus(xml.current_permissive) is set to @rhombus(#true), then
 @rhombus(xml.Element) content and @rhombus(xml.Attribute) values can be
 anything. Such values can be converted to and from XML syntax objects,
 but not read or written as XML.

}

@doc(
  class xml.Prolog(~pre_misc: post_misc :: List.of(xml.Misc) = [],
                   ~dtd: dtd :: maybe(xml.DTD) = #false,
                   ~post_misc: pst_misc :: List.of(xml.Misc) = [])

  class xml.DTD(~name: name :: String,
                ~system: system :: String,
                ~public: public :: maybe(String) = #false)
){

 Represents metadata for an XML document.

}

@doc(
  class xml.SrclocRange(start :: Srcloc, end :: Srcloc)
){

 Represents the source location of an element, attribute, or content in
 an XML document. The @rhombus(start) source location has a
 @rhombus(Srcloc.span) value to capture the difference between the start
 and end source locations for an XML component, but @rhombus(end)
 provides additional information about the ending line and column.

}
