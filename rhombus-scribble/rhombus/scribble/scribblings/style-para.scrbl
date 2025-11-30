#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open
    "style.rhm" open)

@title(~tag: "style-para"){Paragraph Styles}

@doc(
  ~nonterminal_key: Paragraph
  ~nonterminal: compound_paragraph_style: CompoundParagraph
  grammar paragraph_style
){

@name_itemlist(
 @elem{a @tech{paragraph}}

 @item{A @rhombus(String, ~annot): @html{Used as a CSS class name.}
  @latex{Used as the name of a command called with the element content as
   a single argument.}}

 @item{@symkey(#'author): @all{Typeset as the author of a document. Such
   paragraphs normally should appear only in the initial flow of a
   @tech{part} for a document.} @latex{Author information is moved
   information to the title.}}

 @item{@symkey(#'pretitle): @all{Typeset before the title of the
   enclosing part.}}

 @item{@symkey(#'wraps): @latex{Not boxable in the sense of
   @rhombus(Style.BoxMode, ~annot).}}

)

@property_itemlist(
 @elem{a @tech{paragraph}}

 @item{@symkey(#'omitable): @html{When a table cell contains a single
   @tech{paragraph} with this property, no @tt{<p>} tag wraps the cell
   content.}}

 @item{@symkey(#'div): @html{Generates @tt{<div>} output instead of
   @tt{<p>}, unless a @rhombus(Style.HTML.AltTag, ~annot) property is also
   present.}}

 @item{A @rhombus(Style.HTML.AltTag, ~annot): @html{Generates the
   indicated tag instead of @tt{<p>} or @tt{<div>}.}}

 @item{A @rhombus(Style.HTML.Attributes, ~annot): @html{Provides
   additional attributes for the @tt{<p>}, @tt{<div>}, or alternate tag.}}

 @item{@symkey(#'#{never-indents}): @latex{Adjusts the pargraph when in
   a @tech{compound paragraphs}. See @rhombus(compound_paragraph_style).}}

 @item{A @rhombus(Style.BoxMode, ~annot): @latex{Uses an alternate
   rendering form for boxing contexts (such as a table cell). See
   @rhombus(Style.BoxMode, ~annot).}}

)

}
