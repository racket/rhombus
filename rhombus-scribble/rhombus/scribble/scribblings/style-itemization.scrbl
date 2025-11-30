#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open
    "style.rhm" open)

@title(~tag: "style-itemization"){Itemization Styles}

@doc(
  ~nonterminal_key: Itemization
  ~nonterminal: compound_paragraph_style: CompoundParagraph
  grammar itemization_style
){

@name_itemlist(
 @elem{an @tech{itemization}}

 @item{A @rhombus(String, ~annot): @html{Used as a CSS class name.}
  @latex{Used as the name of a command called with the itemization content
   as a single argument.}}

 @item{@symkey(#'compact): @all{Reduces space between items.}}

 @item{@symkey(#'ordered): @html{Generates @tt{<ol>} output instead of
   @tt{<ul>}. @latex{Generates an enumeration instead of an itemization.}}}

)

@property_itemlist(
 @elem{an @tech{itemization}}

 @item{A @rhombus(Style.HTML.Attributes, ~annot): @html{Provides
    additional attributes for the @tt{<ul>} or @tt{<ol>} tag.}}

 @item{@symkey(#'#{never-indents}): @latex{Adjusts the pargraph when in
    a @tech{compound paragraphs}. See @rhombus(compound_paragraph_style).}}

)

}
