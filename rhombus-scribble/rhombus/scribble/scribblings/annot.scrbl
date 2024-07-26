#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "annot"){Building Blocks}

@doc(
  annot.macro 'Element'
){

 Elements are components of a paragraph. The can be individual words,
 whole sentences, or londer runs of text, and they can have styling such
 as font or size changes.

 Element constructors include @rhombus(bold) and @rhombus(larger). Most
 elements are constructed implicitly, however, from content or
 pre-content in the form of a plain string.

}

@doc(
  annot.macro 'Content'
  annot.macro 'PreContent'
){

 Content is either a @rhombus(ReadableString, ~annot),
 @rhombus(Element, ~annot), a @defterm{convertable} value, or a list of
 @rhombus(Content, ~annot). @defterm{Convertable} values includes images constructed
 via @rhombusmodname(pict).

 Pre-content is the same as content, but with the intent that strings
 will be decoded to change, for example, @litchar{``} and @litchar{''} into
 @litchar{“} and @litchar{”}.

}

@doc(
  annot.macro 'PreFlow'
){

 A pre-flow is like pre-content, but can also include blocks and
 @rhombus(#void).

}

@doc(  
  annot.macro 'FlowBlock'
){

 A flow block is a generalization of a paragraph.

}

@doc(
  annot.macro 'Part'
){

 A part is a section, perhaps with subsection, or even a whole document.

}

@doc(
  annot.macro 'PartDecl'
){

 A part declaration is created by functions such as @rhombus(section),
 and they are used by document decoding to construct a tree of parts.

}

@doc(
  annot.macro 'Style'
){

 A style is metadata that is associated with an element or flow block.
 It affects rendering in a backend-specific way.

 For example, @rhombus(bold) creates an element with a style that makes
 the text bold.

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
  annot.macro 'Taglet'
){

}

@doc(
  annot.macro 'Content.to_s_exp'
  annot.macro 'PreContent.to_s_exp'
  annot.macro 'PreFlow.to_s_exp'
  annot.macro 'Tag.to_s_exp'
  annot.macro 'Taglet.to_s_exp'
){

 Converting annotations that produce a value suitable for interoperation
 with Racket Scribble libraries.

}
