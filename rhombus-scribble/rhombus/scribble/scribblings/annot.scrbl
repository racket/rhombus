#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open)

@title(~tag: "annot"){Main Datatypes}

@doc(
  veneer Element
  fun Element(
    content  :: Content,
    ~style: style :: maybe(Style || String || Symbol) = #false
  ) :: Element
  property (e :: Element).style :: maybe(Style || String || Symbol)
  property (e :: Element).content :: Content
){

 Elements represent styled components of @tech{content}, typically in a
 @tech{paragraph} or nested in other elements. The styled components can
 be individual words, whole sentences, or longer runs of text, and they
 can have styling such as font or size changes.

 Elements are typically constructed via functions like @rhombus(bold)
 and @rhombus(larger), instead of using @rhombus(Element) directly.

 @veneer_same(Element, rkt_element)

 See also @rhombus(MultiargElement, ~annot),
 @rhombus(TraverseElement, ~annot),
 @rhombus(PartRelativeElement, ~annot), @rhombus(DelayedElement, ~annot),
 @rhombus(CollectElement, ~annot), and @rhombus(RenderElement, ~annot).

}

@doc(
  annot.macro 'Content'
  annot.macro 'PreContent'
  annot.macro 'Content.to_s_exp'
  annot.macro 'PreContent.to_s_exp'
){

 @tech{Content} is either a @rhombus(ReadableString, ~annot),
 @rhombus(Element, ~annot), @rhombus(MultiargElement, ~annot),
 @rhombus(TraverseElement, ~annot),
 @rhombus(PartRelativeElement, ~annot), @rhombus(DelayedElement, ~annot),
 a @tech{convertible} value, or a list of
 @rhombus(Content, ~annot). Convertable values includes images
 constructed via @rhombusmodname(pict).

 Pre-content is the same as content, but with the intent that
 @tech{decoding} is applied to strings in @tech{content mode}. For
 example, @litchar{``} and @litchar{''} are decoded into @litchar{“} and
 @litchar{”}.

 @rhombus(Content.to_s_exp, ~annot) and
 @rhombus(PreContent.to_s_exp, ~annot) are converting annotations that
 produce a value suitable for interoperation with Racket Scribble
 libraries. In particular, they convert Rhombus lists to pair lists.

}

@doc(
  fun Content.to_string(content :: Content) :: String
  fun Content.width(content :: Content) :: Int
){

 The @rhombus(Content.to_string) function converts @tech{content} to a
 plain string, essentially extracting it's ``plain text.''

 The @rhombus(Content.width) function reports an approximate width in
 characters for the given @tech{content}.

}

@doc(
  annot.macro 'PreFlow'
  annot.macro 'PreFlow.to_s_exp'
){

 A pre-flow is subject to @tech{decoding} in @tech{flow mode}. So, it
 includes @rhombus(PreContent, ~annot), but also blocks and
 @rhombus(#void).

 @rhombus(PreFlow.to_s_exp, ~annot) is a converting annotation that
 produces a value suitable for interoperation with Racket Scribble
 libraries. In particular, it converts Rhombus lists to pair lists.

}

@doc(  
  annot.macro 'FlowBlock'
){

 A @tech{flow block} is a generalization of a paragraph, and
 @rhombus(FlowBlock, ~annot) includes @rhombus(Paragraph, ~annot),
 @rhombus(NestedFlow, ~annot), @rhombus(Table, ~annot),
 @rhombus(Itemization, ~annot), @rhombus(CompoundParagraph, ~annot).
 @rhombus(TraverseBlock, ~annot), and @rhombus(DelayedBlock, ~annot),

 @annot_same(FlowBlock, rkt_block)

}

@doc(
  veneer Part
){

 A @tech{part} is a section, perhaps with subsections, or even a whole
 document. A part is normally constructed by @tech{decoding} and not
 constructed or inspected directly, but see @rhombus(Part) for
 information constructing and inspecting them.

 @veneer_same(Parl, rkt_part)

}

@doc(
  annot.macro 'PartDecl'
){

 A part declaration is created by functions such as @rhombus(section),
 and they are used by document decoding to construct a tree of parts.

}

@doc(
  veneer Style
  fun Style(
    ~name: name :: maybe(Symbol || String) = #false,
    ~properties: properties :: Listable = []
  ) :: Style
  def Style.plain :: Style
){

 A style is metadata that is associated with an element or flow block.
 It affects rendering in a backend-specific way. For example,
 @rhombus(bold) creates an element with a style that makes the text bold.

@itemlist(

 @item{@rhombus(name): Symbol names are used to communicate certain
  style adjustments that are applicable to many backends, such as
  @rhombus(#'bold) for bold text. String names tend to be used in a
  backend-specific way: the CSS class name for HTML output, or a Latex
  macro or environment name for Latex/PDF output.}

 @item{@rhombus(properties): A list of values that further configure
  backend-specific choices. Again, symbol properties are often used for
  style adjustments that are applicable to many backends, or a property
  may be a value with more structure.}

)

 The @rhombus(Style.plain) style is used as a default style that applies
 no effects when rendering.

 @veneer_same(Style, rkt_style)

}

@doc(
  annot.macro 'StyleLike'
){

 A @rhombus(StyleLike, ~annot) value can be coerced to a @rhombus(Style, ~annot).
 It can be a @rhombus(Style, ~annot) already, a @rhombus(String, ~annot),
 a @rhombus(Symbol, ~annot), or a @rhombus(List.of(Symbol), ~annot).

}

@doc(
  annot.macro 'Tag'
  annot.macro 'GeneratedTag'
  annot.macro 'TagSuffix'
  annot.macro 'Taglet'
  annot.macro 'TagPrefix'
  annot.macro 'Tag.to_s_exp'
  annot.macro 'Taglet.to_s_exp'
){

 The @rhombus(Tag, ~annot) annotation recognizes values that work as
 @tech{tags}, which includes @rhombus(GeneratedTag, ~annot)s. The
 @rhombus(TagSuffix, ~annot) annotation recognizes values suitable for
 building a tag by putting it in a list with a symbol, such as
 @rhombus(#'part), or a list that can be turned into a tag by adding a
 symbol to the font. The @rhombus(Taglet, ~annot) annotation recognizes
 non-list @rhombus(TagSuffix, ~annot)es.

 The @rhombus(TagPrefix, ~annot) annotation recognizes values that work
 as a @tech{tag prefix}, which is a string or a
 @rhombus(ModulePath, ~annot).

 @rhombus(Tag.to_s_exp, ~annot) and @rhombus(Taglet.to_s_exp, ~annot)
 are converting annotations that produce a value suitable for
 interoperation with Racket Scribble libraries. In particular, they
 convert Rhombus lists to pair lists.

}

@doc(
  annot.macro 'DocCategory'
){

 Recognizes a @rhombus(Symbol, ~annot), @rhombus(Symbol, ~annot), or
 @rhombus(Box.now_of(Symbol), ~annot) as a category name, or a list of
 two items: a category name and a @rhombus(Real, ~annot) to specify an
 order within the category. A category is used to classify documentation
 in a listing of all installed documentation.

 The recognized category symbols are @rhombus(#'#{getting-started}),
 @rhombus(#'tutorial), @rhombus(#'core), @rhombus(#'teaching),
 @rhombus(#'language), @rhombus(#'tool), @rhombus(#'#{gui-library}),
 @rhombus(#'#{net-library}), @rhombus(#'#{parsing-library}),
 @rhombus(#'#{tool-library}), @rhombus(#'foreign), @rhombus(#'interop),
 @rhombus(#'#{drracket-plugin}), @rhombus(#'library),
 @rhombus(#'experimental), and @rhombus(#'legacy).

}
