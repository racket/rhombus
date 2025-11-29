#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open)

@title(~tag: "more_annot"){Additional Datatypes}

@doc(
  fun Part(
    ~tag_prefix: tag_prefix :: maybe(String || Map) = #false,
    ~tags: tags :: Listable.to_list && List.of(Tag) = [],
    ~title_content: title_content :: maybe(Content) = #false,
    ~style: style :: Style = Style.plain,
    ~to_collect: to_collect :: Content = [],
    ~blocks: blocks :: Listable.to_list && List.of(FlowBlock) = [],
    ~parts: parts :: Listable.to_list && List.of(Part) = []
  ) :: Part
  property (p :: Part).tag_prefix :: maybe(String || Map)
  property (p :: Part).tags :: PairList.of(Tag)
  property (p :: Part).title_content :: maybe(Content)
  property (p :: Part).style :: Style
  property (p :: Part).to_collect :: Content
  property (p :: Part).blocks :: PairList.of(FlowBlock)
  property (p :: Part).parts :: PairList.of(Part)
  fun Part.context_accumulation(key :: Any) :: Any
){

 See @rhombus(Part, ~annot) for general information about parts.

 @veneer_same(Part, rkt_part) The @rhombus(Part) function constructs
 such a value given its components:

@itemlist(

 @item{@rhombus(tag_prefix): a @tech{tag prefix} and/or @tech{part
   context} accumulation.}

 @item{@rhombus(tags): A list of @tech{tags} that each link to the section
  represented by the part. Normally, @rhombus(tags) should be a non-empty
  list, so that hyperlinks can target the section.}

 @item{@rhombus(title_content): The part's title, if any.}

 @item{@rhombus(style): See part styles for more information about recognized styles.}

 @item{@rhombus(to_collect): @tech{Content} that is inspected during the
  @tech{collect pass}, but ignored in later passes, so it doesn't directly
  contribute to the part's rendered form.}

 @item{@rhombus(blocks): The part's content, rendered before any subparts.}

 @item{@rhombus(parts): Subparts, which represent subsections of the part.}

)

 See @secref("part-context") for information about
 @rhombus(Part.context_accumulation).

}


@doc(
  veneer Paragraph
  fun Paragraph(
    ~style: style :: Style = Style.plain,
    content :: Content = []
  ) :: Paragraph
  property (p :: Paragraph).style :: Style
  property (p :: Paragraph).content :: Content
){

 Represents a @tech{paragraph}. Normally, a paragraph is created with
 @rhombus(para) or by @tech{decoding}.

 @veneer_same(Paragraph, rkt_paragraph)

}

@doc(
  veneer Table
  fun Table(
    ~style: style :: Style = Style.plain,
    blockss :: (Listable.to_list
                  && List.of(Listable.to_list)
                  && List.of(List.of(Cell))) = []
  ) :: Table
  property (t :: Table).style :: Style
  property (t :: Table).blockss :~ PairList.of(PairList.of(FlowBlock))
){

 Represents a @tech{table}. Normally, a table is created with
 @rhombus(tabular).

 @veneer_same(Table, rkt_table)

}

@doc(
  veneer Itemization
  fun Itemization(
    ~style: style :: Style = Style.plain,
    blockss :: (Listable.to_list
                  && List.of(Listable.to_list)
                  && List.of(List.of(FlowBlock))) = []
  ) :: Itemization
  property (i :: Itemization).style :: Style
  property (i :: Itemization).blockss
    :: PairList.of(PairList.of(FlowBlock))
){

 Represents an @tech{itemization}. Normally, an itemization is created
 with @rhombus(itemlist).

 @veneer_same(Itemization, rkt_itemization)

}

@doc(
  veneer NestedFlow
  fun NestedFlow(
    ~style: style :: Style = Style.plain,
    blocks :: Listable.to_list && List.of(List.of(FlowBlock)) = []
  ) :: NestedFlow
  property (nf :: NestedFlow).style :: Style
  property (nf :: NestedFlow).blocks :: PairList.of(FlowBlock)
){

 Represents a @tech{nested flow}. Normally, an itemization is created
 with @rhombus(nested).

 @veneer_same(NestedFlow, rkt_nested_flow)

}

@doc(
  veneer CompoundParagraph
  fun CompoundParagraph(
    ~style: style :: Style = Style.plain,
    blocks :: Listable.to_list && List.of(List.of(FlowBlock)) = []
  ) :: CompoundParagraph
  property (cp :: CompoundParagraph).style :: Style
  property (cp :: CompoundParagraph).blocks :: PairList.of(FlowBlock)
){

 Represents a @tech{compound paragraph}. Normally, a compound paragraph
 is created by @tech{decoding}.

 @veneer_same(CompoundParagraph, rkt_compound_paragraph)

}

@doc(
  veneer TraverseBlock
  fun TraverseBlock(
    ~traverse: traverse :: TraverseBlockFunction
  ) :: TraverseBlock
  property (tb :: TraverseBlock).traverse :: TraverseBlockFunction
  annot.macro 'TraverseBlockFunction'
){

 Represents a @tech{flow block} that participates directly in the
 @tech{traverse pass}.

 A @rhombus(TraverseBlockFunction, ~annot) for @rhombus(traverse)
 satisifes

@rhombusblock(#,(@rhombus((Any -> Any, (Any, Any) -> Void) -> FlowBlock || TraverseBlockFunction, ~annot)))

 where the first argument to a @rhombus(TraverseBlockFunction, ~annot)
 is a @rhombus(get, ~var) function that takes a key and results a value,
 and the second argument is a @rhombus(set, ~var) function that takes a
 key and a new value for the key. The result of a
 @rhombus(TraverseBlockFunction, ~annot) must be either a replacement
 @rhombus(FlowBlock, ~annot) or another
 @rhombus(TraverseBlockFunction, ~annot) function to participate in the
 next iteration of the @tech{traverse pass}.

 @veneer_same(TraverseBlock, rkt_traverse_block)

}

@doc(
  veneer DelayedBlock
  fun DelayedBlock(
    ~resolve: resolve :: (Any, Part, ResolveInfo) -> FlowBlock
  ) :: DelayedBlock
  property (db :: DelayedBlock).resolve
    :: (Any, Part, ResolveInfo) -> FlowBlock
){

 Represents a @tech{flow block} that participates directly in the
 @tech{resolve pass}. The @rhombus(resolve) function produces a block
 that serves as replacement for the @rhombus(DelayedBlock, ~annot) after
 it is resolved.

 @veneer_same(DelayedBlock, rkt_delayed_block)

}

@doc(
  veneer MultiargElement
  fun MultiargElement(
    contents  :: Listable.to_list && List.of(Content),
    ~style: style :: maybe(Style || String || Symbol) = #false
  ) :: MultiargElement
  property (e :: MultiargElement).style
    :: maybe(Style || String || Symbol)
  property (e :: MultiargElement).contents :: PairList.of(Content)
){

 Like @rhombus(Element, ~annot), but for Latex-backed @tech{renderers}
 that have multiple-argument styles. Specifically, the name of
 @rhombus(style) is used as the name of a Latex macro that accepts as
 many arguments as elements in @rhombus(contents).

 @veneer_same(MultiargElement, rkt_multarg_element)

}

@doc(
  veneer TraverseElement
  fun TraverseElement(
    ~traverse: traverse :: TraverseElementFunction
  ) :: TraverseElement
  property (e :: TraverseElement).traverse
    :: TraverseElementFunction
  annot.macro 'TraverseElementFunction'
){

 Represents @tech{content} that participates directly in the
 @tech{traverse pass}.

 A @rhombus(TraverseElementFunction, ~annot) for @rhombus(traverse)
 satisifes

@rhombusblock(#,(@rhombus((Any -> Any, (Any, Any) -> Void) -> Content || TraverseElementFunction, ~annot)))

 analogous to @rhombus(TraverseBlockFunction, ~annot).

 @veneer_same(TraverseElement, rkt_traverse_element)

}

@doc(
  veneer PartRelativeElement
  fun PartRelativeElement(
    ~collect: collect :: CollectInfo -> Content,
    ~sizer: sizer :: () -> Any,
    ~plain: plain :: () -> Any
  ) :: PartRelativeElement
  property (e :: PartRelativeElement).collect
    :: CollectInfo -> Content
  property (e :: PartRelativeElement).sizer :: () -> Any
  property (e :: PartRelativeElement).plain :: () -> Any
){

 Represents @tech{content} that participates directly in the
 @tech{collect pass}.

 @veneer_same(PartRelativeElement, rkt_part_relative_element)

}

@doc(
  veneer CollectElement:
    extends Element
  fun CollectElement(
    ~style: style :: maybe(Style || String || Symbol) = #false,
    content :: Content = [],
    ~collect: collect :: CollectInfo -> Any
  ) :: CollectElement
  property (e :: CollectElement).style
    :: maybe(Style || String || Symbol)
  property (e :: CollectElement).content :: COntent
  property (e :: CollectElement).collect
    :: CollectInfo -> Any
){

 Represents @tech{content} that participates directly in the
 @tech{collect pass}---similar to @rhombus(PartRelativeElement, ~annot),
 but as a value that also satisifies @rhombus(Element, ~annot) and that
 does not produce a replacement for itself.

 @veneer_same(CollectElement, rkt_collect_element)

}

@doc(
  veneer DelayedElement
  fun DelayedElement(
    ~resolve: resolve :: (Any, Part, ResolveInfo) -> Content,
    ~sizer: sizer :: () -> Any,
    ~plain: plain :: () -> Any
  ) :: DelayedElement
  property (e :: DelayedElement).resolve
    :: (Any, Part, ResolveInfo) -> Content
  property (e :: DelayedElement).sizer :: () -> Any
  property (e :: DelayedElement).plain :: () -> Any
){

 Represents @tech{content} that participates directly in the
 @tech{resolve pass}.

 @veneer_same(DelayedElement, rkt_delayed_element)

}


@doc(
  veneer RenderElement:
    extends Element
  fun RenderElement(
    ~resolve: resolve :: (Any, Part, ResolveInfo) -> Content
  ) :: RenderElement
  property (e :: RenderElement).resolve
    :: (Any, Part, ResolveInfo) -> Content
){

 Represents @tech{content} that participates directly in the
 @tech{render pass}.

 @veneer_same(RenderElement, rkt_render_element)

}


@doc(
  annot.macro 'CollectInfo'
  annot.macro 'ResolveInfo'
){

 A @rhombus(CollectInfo, ~annot) object contains accumulated, part-local
 information gathered in the @tech{collect pass} to set up
 cross-reference targets. @annot_same(CollectInfo, rkt_collect_info).

 A @rhombus(ResolveInfo, ~annot) object contains information gathered in
 the @tech{resolve pass}. @annot_same(ResolveInfo, rkt_resolve_info).

}
