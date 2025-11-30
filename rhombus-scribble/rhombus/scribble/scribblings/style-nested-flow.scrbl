#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open
    "style.rhm" open)

@title(~tag: "style-nested-flow"){Nested Flow Styles}

@doc(
  ~nonterminal_key: NestedFlow
  ~nonterminal: compound_paragraph_style: CompoundParagraph
  grammar nested_flow_style
){

@name_itemlist(
 @elem{a @tech{nested flow}}

 @item{A @rhombus(String, ~annot): @html{Used as a CSS class name.}
  @latex{Used as the name of an environment used around the nested flow
   content.}}

 @item{@symkey(#'inset): @all{Insets the nested flow relative to
   surrounding text.}}

 @item{@symkey(#'#{code-inset}): @all{Insets the nested flow relative to
   surrounding text in a way suitable for code.} @latex{If the nested flow
   has a single @tech{flow block}, then it is boxable in the sense of
   @rhombus(Style.BoxMode, ~annot).}}

 @item{@symkey(#'#{vertical-inset}): @all{Insets the nested flow
   vertically relative to surrounding text, but not horizontally.}
  @latex{If the nested flow has a single @tech{flow block}, then it is
   boxable in the sense of @rhombus(Style.BoxMode, ~annot).}}

)

@property_itemlist(
 @elem{a @tech{nested flow}}

 @item{@symkey(#'command): @latex{A @rhombus(String, ~annot) style name
   is used as a command name instead of an environment name.}}

 @item{@symkey(#'multicommand):@latex{A @rhombus(String, ~annot) style
   name is used as a command name, instead of an environment name, with a
   separate argument for each @tech{flow block} in in the nested flow.}}

 @item{A @rhombus(Style.HTML.AltTag, ~annot): @html{Generates the
   indicated tag instead of @tt{<blockquote>}.}}

 @item{A @rhombus(Style.HTML.Attributes, ~annot): @html{Provides
    additional attributes for the @tt{<ul>} or @tt{<ol>} tag.}}

 @item{@symkey(#'#{never-indents}): @latex{Adjusts the pargraph when in
   a @tech{compound paragraphs}. See @rhombus(compound_paragraph_style).}}

 @item{A @rhombus(Style.BoxMode, ~annot): @latex{Uses an alternate
   rendering form for boxing contexts (such as a table cell). See
   @rhombus(Style.BoxMode, ~annot).}}

 @item{@symkey(#'decorative): @all{The content of the nested flow is
   intended for decoration.} @text_renderer{Skips a @rhombus(#'decorative)
   nested flow.}}

 @item{@symkey(#'pretitle): @latex{Typeset before the title of the
   enclosing part.}}

)

}
