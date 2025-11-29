#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "rhombus"){Typesetting Code}

@(def esc: @rhombus(#,))

@doc(
  expr.macro 'rhombus($group)'
  expr.macro 'rhombus($group, $builtin_space)'
  expr.macro 'rhombus($group, ~at $space_name)'
  expr.macro 'rhombus($group, ~at: $space_name)'
  grammar builtin_space:
    ~var
    ~datum
    ~value
    ~result
    ~expr
    ~defn
    ~decl
    ~bind
    ~impo
    ~expo
    ~modpath
    ~annot
    ~repet
    ~stxclass
    ~reducer
    ~class
    ~space
    ~for_clause
    ~class_clause
    ~interface_clause
    ~veneer_clause
    ~entry_point
    ~unquote_bind
    ~syntax_class_clause
    ~pattern_clause
    ~space_clause
    ~space_meta_clause
    ~key_comp
    ~immediate_callee
    ~operator_order
    ~doc
  grammar space_name:
    $id_or_op ...
){

 Typesets @rhombus(group) literally, except as adjusted by spacers and
 typesetting transformers. The result is an @rhombus(Element, ~annot) to
 appear inline in a paragraph. Spacing in @rhombus(group) is normalized,
 instead of preserved exactly as in the source.

 Use @rhombus(#,(esc)(#,(@rhombus(expr, ~var)))) within @rhombus(group)
 to escape from literal mode and substitute the element produced by
 @rhombus(expr, ~var).

 A @rhombus(builtin_space) or space name after @rhombus(~at) supplies
 the initial space to use for creating hyperlinks in @rhombus(group)
 based on @rhombus(meta_label, ~impo) imports.

 Typesetting and hyperlinking can be adjusted via @tech{spacers}, which
 can select an alternative space for components of @rhombus(group) for
 hyperlinking. That is, a supplied @rhombus(builtin_space) or
 @rhombus(~at space_name) specifies the initial space of @rhombus(group),
 but a spacer binding can determine the space of subsequent elements of
 @rhombus(group).

 Even before applying spacers, identifiers in @rhombus(group) bound as
 typesetting transformers are replaced with their transformations. That's
 how metavariables get typeset as italic, for example: each metavariable
 is bound as a transformer that adds the @rhombus(~var) space to the
 identifier.

}

@doc(
  expr.macro 'rhombuslink($name, $content, ... ~nonempty)'
  expr.macro 'rhombuslink($name, $builtin_space, $content, ... ~nonempty)'
  expr.macro 'rhombuslink($name, ~at $space_name, $content, ... ~nonempty)'
){

 Link @rhombus(rhombus), but uses the text of
 @rhombus(elem([content, ...])) preserving the hyperlink (if any) of
 @rhombus(name).

}

@doc(
  expr.macro 'rhombusblock($group, ...)'
){

 Similar to @rhombus(rhombus), but producing a
 @rhombus(FlowBlock, ~annot) for multiple groups and with whitespace
 preserved exactly as in the source.

 Note that when you use @litchar("@") notation to call
 @rhombus(rhombusblock), then no comma is required between
 @rhombus(group)s that are on separate lines. So, for example,

@rhombusblock(
  @rhombusblock(
    fun add(x, y):
      x + y
    add(1, 2)
  )
)

 renders as

@rhombusblock(
  fun add(x, y):
    x + y
  add(1, 2)
)

 without adding a comma after the @rhombus(add) definition. If the
 source does contain a comma, then is preserved in the output.

}


@doc(
  expr.macro 'rhombusblock_etc:
                $group
                ...'
  expr.macro 'rhombusblock_etc ($option, ...):
                $group
                ...'
  grammar option:
    ~escape: $op
    ~inline: $body; ...
    ~inset: $body; ...
    ~indent: $body: ...
    ~prompt: $body: ....
    ~text: $body; ...
    ~indent_from_block: $body; ...
    ~spacer_info_box: $body; ...
    ~number_from: $body; ...
){

 Like @rhombus(rhombusblock), but supports @rhombus(option)s:

@itemlist(

 @item{@rhombus(~escape): Replaces the escape operator @litchar{#,} with
  @rhombus(op) so that @litchar{#,} is literal (assuming that
  @rhombus(op) is different from @litchar{#,}).}

 @item{@rhombus(~inline): Produces @tech{content} instead of a
  @tech{flow block}. The @rhombus(group) content must be a single line.}

 @item{@rhombus(~inset): Insets the block relative to surrounding text
  if the @rhombus(body) sequence after @rhombus(~inset) produces
  a true value. The default is to inset.}

 @item{@rhombus(~indent): Adds space before each line of the block to
  indent by the amount produced by the @rhombus(body) sequence after
  @rhombus(~indent). The indentation amount must be a nonnegative integer
  that is treated as a character count. The default is @rhombus(0)
  indentation.}

 @item{@rhombus(~prompt): Uses the string produced by the @rhombus(body)
  sequence after @rhombus(~prompt) as a prompt that prefixes the first
  line of the content, and lines afterward get the same number of spaces
  as characters in the prompt. The default is to show no prompt, which is
  equivalent to an empty string.}

 @item{@rhombus(~text): Expects the body @rhombus(group) to be
  @rhombus(([group, ...])) and ignores metadata of the parentheses and
  brackets with the expectation that the body @rhombus(group) was
  originally written between @litchar("@{") and @litchar("}").}

 @item{@rhombus(~indent_from_block): Uses the indentation of the block
  overall, instead of the first item in the block, to infer relative
  indentation for rendered content if the @rhombus(body) sequence after
  @rhombus(~indent_from_block) produces a true value. The default is
  true.}

 @item{@rhombus(~spacer_info_box): If the @rhombus(body) sequence after
  @rhombus(~spacer_info_box) produces a box, and if the same box is used
  for multiple @rhombus(rhombusblock_etc) forms, then spacer information
  is shared across the forms. For example, when forms a is treated as
  bindings that associates an annotation with an identifier, that
  association can persist for later blocks. The default is for each block
  to collect independent spacer information.}

 @item{@rhombus(~number_from): If the @rhombus(body) sequence after
  @rhombus(~number_from) produces a value other than @rhombus(#false), it
  must be a number to write to the left of the first line of rendered
  output, and subsequent lines are numbered accordingly.}

)

}


@doc(
  defn.macro '«rhombus_typeset '$id':
                 $body
                 ...»'
){

 Binds @rhombus(id) so that when it is used in @rhombus(rhombus) or
 @rhombus(rhombusblock), the use of @rhombus(id) is replaced with the
 result of the @rhombus(body) sequence.

}

@doc(
  expr.macro 'rhombusmodname ($module_path)'
  expr.macro 'rhombuslangname ($module_path)'
  expr.macro 'racketmodname ($module_path)'
){

 Form for referencing Rhombus modules, languages, and Racket modules.

}
