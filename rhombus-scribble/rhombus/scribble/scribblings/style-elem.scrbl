#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open
    "style.rhm" open)

@title(~tag: "style-elem"){Element Styles}

@doc(
  ~nonterminal_key: Element
  grammar element_style
){

@name_itemlist(
 @elem{an @rhombus(Element, ~annot)}

 @item{A @rhombus(String, ~annot): @html{Used as a CSS class name.}
  @latex{Used as the name of a command called with the element content as
   a single argument.}}

 @item{@symkey(#'tt), @rhombus(#'italic), @rhombus(#'bold),
  @rhombus(#'roman), @rhombus(#'sf), @rhombus(#'url),
  @rhombus(#'subscript), @rhombus(#'superscript), @rhombus(#'smaller),
  @rhombus(#'larger): @all{Basic styles recognized by all
   @tech{renderers}.}}

 @item{@symkey(#'hspace): @all{Renders the element's content as
   monospace blanks.}}

 @item{@symkey(#'newline): @all{Renders a line break, independent of the
   element's content.}}

 @item{@symkey(#'#{no-break}): @all{Prevents line breaks when rendering
   the elements content.}}

)

@property_itemlist(
 @elem{an @rhombus(Element, ~annot)}

 @item{A @rhombus(Style.TargetURL, ~annot): @all{Generates a hyperlink.}}

 @item{A @rhombus(Style.HTML.URLAnchor, ~annot): @html{Inserts a hyperlink
   target at the start of the element's content.}}

 @item{A @rhombus(Style.Color, ~annot)): @all{Applies a color to the
   content text.}}

 @item{A @rhombus(Style.BackgroundColor, ~annot): @all{Applies a color
   to the background of the content.}}

 @item{A @rhombus(Style.HTML.AltTag, ~annot): @html{Generates the given
   HTML tag instead of the default one (@tt{<span>}, @tt{<b>}, etc.}}

 @item{A @rhombus(Style.HTML.Attributes, ~annot): @html{Provides
   additional HTML attributes for a tag.}}

 @item{A @rhombus(Style.HTML.Hover, ~annot)): @html{Adds a text label to
   the content to be shown when the mouse hovers over it.}}

 @item{A @rhombus(Style.HTML.Script, ~annot): @html{Supplies a script
   alternative to the element's content.}}

 @item{A @rhombus(Style.HTML.Xexpr, ~annot): @html{Supplies literal HTML
   to render before and after the element's content.}}

 @item{A @rhombus(Style.HTML.LinkResource, ~annot): @html{Copies a
   referenced file to the location of the rendered document and renders a
   hyperlink to the copy.}}

 @item{A @rhombus(Style.HTML.InstallResource, ~annot): @html{Copies a
   referenced file to the location of the rendered document (without a
   hyperlink).}}

 @item{@symkey(#'aux): @all{Intended for use in titles, where the
  auxiliary part of the title can be omitted in hyperlinks. See, for
  example, @rhombus(secref).}}

 @item{@symkey(#'#{tt-chars}): @latex{When the style name is a string,
   render the element's content with escapes suitable for Latex @tt{tt}
   mode.}}

 @item{@symkey(#'#{exact-chars}): @latex{When the style name is a string
   or @rhombus(#false), render the elements content exactly (without
   escapes).}}

 @item{A @rhombus(Style.Latex.CommandExtras, ~annot): @latex{Adds
   strings as arguments to the Latex command.}}

)

}
