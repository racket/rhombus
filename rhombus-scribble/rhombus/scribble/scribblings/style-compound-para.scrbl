#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open
    "style.rhm" open)

@title(~tag: "style-compound-para"){Compound Paragraph Styles}

@doc(
  ~nonterminal_key: CompoundParagraph
  grammar compound_paragraph_style
){

 @html{A @tech{paragraph} in a compound paragraph's flow is rendered
  without a @tt{<p>} tag, unless the paragraph has a style with a
  non-@rhombus(#false) name.} @latex{For Latex, each @tech{flow block} in
  the compound paragraph's flow is rendered with a preceding
  @ltx{noindent}, unless the block has the @rhombus(#'#{never-indents})
  property---checking recursively in a @tech{nested flow} or
  @tech{compound paragraph} if the nested flow or compound-paragraph
  itself has no @rhombus(#'#{never-indents}) property).}

@name_itemlist(
 @elem{a @tech{compound paragraph}}

 @item{A @rhombus(String, ~annot): @html{Used as a CSS class name.}
  @latex{Used as the name of an environment used around the compound
   paragraph content.}}

)

@property_itemlist(
 @elem{a @tech{compound paragraph}}

 @item{@symkey(#'command): @latex{A @rhombus(String, ~annot) style name
   is used as a command name instead of an environment name.}}

 @item{A @rhombus(Style.HTML.AltTag, ~annot): @html{Generates the
   indicated tag instead of @tt{<blockquote>}.}}

 @item{A @rhombus(Style.HTML.Attributes, ~annot): @html{Provides
    additional attributes for the @tt{<ul>} or @tt{<ol>} tag.}}

 @item{@symkey(#'#{never-indents}): @latex{Adjusts pargraphs with the
   nested flow when in another @tech{compound paragraphs}.}}

)

}
