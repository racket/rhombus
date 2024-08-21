#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "rhombus"){Typesetting Code}

@(def esc: @rhombus(#,))

@doc(
  expr.macro 'rhombus($group)'
  expr.macro 'rhombus($group, $builtin_space)'
  expr.macro 'rhombus($group, ~at $space_name)'
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
    ~doc
){

 Typesets @rhombus(group) literally as an @rhombus(Element, ~annot) to
 appear inline in a paragraph. Spacing in @rhombus(group) is normalized,
 instead of preserved exactly as in the source.

 Use @rhombus(#,(esc)(#,(@rhombus(expr, ~var)))) within @rhombus(group)
 to escape from literal mode and substitute the element produced by
 @rhombus(expr, ~var).

 A @rhombus(builtin_space) or space name after @rhombus(~at) supplies
 the initial space to use for creating hyperlinks in @rhombus(group)
 based on @rhombus(meta_label, ~impo) imports.

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
  expr.macro 'rhombusblock_etc($option, ...):
                $group
                ...'
  grammar option:
    ~inset
    ~indent
    ~prompt
    ~indent_from_block
){

 Like @rhombus(rhombusblock), but supports @rhombus(option)s.

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
