#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open
    "style.rhm" open)

@(nonterminal:
    part_style: Part
    element_style: Element
    paragraph_style: Paragraph
    nested_flow_style: NestedFlow
    table_style: Table
    itemization_style: Itemization
    compound_paragraph_style: CompoundParagraph)

@(macro 'veneer_alias($rhm, $rkt, $content)':
    '@doc(
       veneer $rhm
     ){
      @($content)
      @veneer_same($rhm, $rkt)
     }')

@title(~tag: "style-property-annot"){Style Property Datatypes}

@veneer_alias(Style.Numberer, rkt_numberer){

 Used as a @nontermref(part_style) to implement a section number that
 increment separately from the default numbering style and that can be
 rendered differently than as Arabic numerals.

}

@veneer_alias(Style.DocumentVersion, rkt_document_version){

 Used as a @tech{style property} for a @tech{part} to indicate a version
 number.

}

@veneer_alias(Style.DocumentDate, rkt_document_date){

 @latex{Used as a @tech{style property} for a @tech{part} to indicate a
  date.}

}

@veneer_alias(Style.DocumentSource, rkt_document_source){

 Used as a @tech{style property} to associate a module path with a
 @tech{part}.

}

@doc(
  veneer Style.Color
  fun Style.Color(color :: String) :: Style.Color
  fun Style.Color(red :: Byte, green :: Byte, blue :: Byte)
    :: Style.Color
){

 Used as a @tech{style property} in an @nontermref(element_style) to set
 the content's color. Recognized string names for color depend on the
 renderer, but at the recognized set includes at least @rhombus("white"),
 @rhombus("black"), @rhombus("red"), @rhombus("green"), @rhombus("blue"),
 @rhombus("cyan"), @rhombus("magenta"), and @rhombus("yellow").

 @html{A @rhombus(Style.Color, ~annot) is also recognized for a
  @tech{flow block}, @tech{part} (and used for the title in the latter case) or
  cell in a @tech{table}}

 @veneer_same(Style.Color, rkt_color)

}

@doc(
  veneer Style.BackgroundColor
  fun Style.BackgroundColor(color :: String)
    :: Style.BackgroundColor
  fun Style.BackgroundColor(red :: Byte, green :: Byte, blue :: Byte)
    :: Style.BackgroundColor
){

 Like @rhombus(Style.Color, ~annot), but for a background color.

 @veneer_same(Style.BackgroundColor, rkt_background_color)

}

@doc(
  veneer Style.TargetURL
  fun Style.TargetURL(~path: path :: PathString) :: Style.TargetURL
  fun Style.TargetURL(~url: url :: String)  :: Style.TargetURL
){

 Used as a @nontermref(element_style) to link content to a specific URL.

 @veneer_same(Style.TargetURL, rkt_target_url)

}

@veneer_alias(Style.BoxMode, rkt_box_mode){

 @latex{Used as a @nontermref(paragraph_style),
  @rhombus(nested_flow_style), @rhombus(table_style),
  @rhombus(itemization_style), @rhombus(compound_paragraph_style).}

}

@veneer_alias(Style.LinkRenderStyle, rkt_link_render_style){

 Used as a @tech{style property} in a @nontermref(part_style).

}

@veneer_alias(Style.RenderConvertibleAs, rkt_render_convertible_as){

 @html{Used as a @tech{style property} in a @nontermref(part_style).}

}

@doc(
  veneer Style.HTML.BodyId
  fun Style.HTML.BodyId(str :: String):: Style.HTML.BodyId
){

 @html{Used as a @tech{style property} in a @nontermref(part_style).}

 @veneer_same(Style.HTML.BodyId, rkt_body_id)

}

@doc(
  veneer Style.HTML.AltTag
  fun Style.HTML.AltTag(
    str :: String && matching(rx'[alpha digit]+')
  ):: Style.HTML.AltTag
){

 @html{Used as a @tech{style property} in a @nontermref(element_style),
  @nontermref(paragraph_style), @nontermref(nested_flow_style),
  @nontermref(compound_paragraph_style).}

 @veneer_same(Style.HTML.AltTag, rkt_alt_tag)

}

@doc(
  veneer Style.HTML.Attributes
  fun Style.HTML.Attributes(
    attribs :: Map.of(Symbol, String) || List.of([Symbol, String])
  ) :: Style.HTML.Attributes
){

 @html{Used as a @tech{style property} in a style to add attibutes to an
  HTML tag.}

 @veneer_same(Style.HTML.Attributes, rkt_attributes_id)

}

@veneer_alias(Style.HTML.HeadExtra, rkt_head_extra){

 @html{Used as a @tech{style property} in a @nontermref(part_style).}

}

@veneer_alias(Style.HTML.HeadAddition, rkt_head_addition){

 @html{Used as a @tech{style property} in a @nontermref(part_style).}

}

@veneer_alias(Style.HTML.Hover, rkt_hover_property){


 @html{Used as a @tech{style property} in a @nontermref(part_style) or
  @nontermref(element_style).}

}

@doc(
  veneer Style.HTML.URLAnchor
  fun Style.HTML.URLAnchor(str :: String):: Style.HTML.URLAnchor
){

 @html{Used as a @tech{style property} in an @nontermref(element_style).}

 @veneer_same(Style.HTML.URLAnchor, rkt_url_anchor)

}

@veneer_alias(Style.HTML.PartTitleAndContentWrapper, rkt_part_title_and_content_wrapper){

 @html{Used as a @tech{style property} in a @nontermref(part_style).}

}

@veneer_alias(Style.HTML.PartLinkRedirect, rkt_part_link_redirect){

 @html{Used as a @tech{style property} in a @nontermref(part_style).}

}

@doc(
  veneer Style.HTML.Script
  fun Style.HTML.Script(
    script :: PathString || List.of(String)
  ):: Style.HTML.Script
){

 @html{Used as a @tech{style property} in an @nontermref(element_style)
  to supply a script alternative to the element content.}

 @veneer_same(Style.HTML.Script, rkt_script_property)

}

@veneer_alias(Style.HTML.Xexpr, rkt_xexpr_property){

 @html{Used as a @tech{style property} in an @nontermref(element_style)
  to supply literal HTML that is rendered before and after element
  content.}

}


@doc(
  veneer Style.Latex.CommandExtras
  fun Style.Latex.Script(extras :: List.of(String))
    :: Style.Latex.CommandExtras
){

 @latex{Used as a @tech{style property} in an @nontermref(element_style)
  to to add extra arguments to the element's command in Latex output.}

 @veneer_same(Style.Latex.CommandExtras, rkt_command_extras)

}
