#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open
    "style.rhm" open)

@title(~tag: "style-table"){Table, Column, and Cell Styles}

@doc(
  ~nonterminal_key: Table
  ~nonterminal: compound_paragraph_style: CompoundParagraph
  grammar table_style
){

@name_itemlist(
 @elem{a @tech{table}}

 @item{A @rhombus(String, ~annot): @html{Used as a CSS class name.}
  @latex{Used as the name of an environment used around the table
   content.}}

 @item{@symkey(#'boxed): @all{Renders as a definition. This style name
   is not intended for use on a table that is nested within another
   @rhombus(#'boxed) table; nested uses may look right for some renderers
   but not others.}}

 @item{@symkey(#'centered): @html{Centers the table horizontally with
   respect to its enclosing flow.}}

 @item{@symkey(#'block): @latex{Prevents pages breaks between the
   table's rows.}}

)

@property_itemlist(
 @elem{a @tech{table}}

 @item{A @rhombus(Style.TableColumns, ~annot): @all{Provides
   column-specific styles, but only
   @rhombus(Style.HTML.ColumnAttributes, ~annot) properties (if any) within the
   styles are used if a @rhombus(Style.TableCells, ~annot) structure is also
   included as a style property. See @rhombus(Style.TableCells, ~annot) for
   information about how a column style is used for each cell.}}

 @item{A @rhombus(Style.TableCells, ~annot): @all{Provides cell-specific
   styles. See @rhombus(Style.TableCells, ~annot) for information about how
   the styles are used.}}

 @item{A @rhombus(Style.HTML.Attributes, ~annot): @html{Provides
   additional attributes for the @tt{<table>} tag.}}

 @item{@symkey(#'aux): @html{Include the table in the table-of-contents
   display for the enclosing part.}}

 @item{@symkey(#'#{never-indents}): @latex{Adjusts the pargraph when in
   a @tech{compound paragraphs}. See @rhombus(compound_paragraph_style).}}

)

 @latex{A @tech{paragraph} as a cell value is not automatically
  line-wrapped, unless a vertical alignment is specified for the cell
  through a @rhombus(Style.TableCells, ~annot) or
  @rhombus(Style.TableColumns, ~annot) style property. To get a
  line-wrapped paragraph, use a @tech{compound paragraph} or use an
  element with a string style and define a corresponding Latex macro in
  terms of @tt{\parbox}. For Latex output of blocks in the flow that are
  @tech{nested flows}, @tech{itemizations}, @tech{compound paragraphs}, or
  @rhombus(DelayedBlock, ~annot), the block is wrapped with minipage using
  @tt{\linewidth} divided by the column count as the width.}

}

@doc(
  veneer Style.TableCells
  fun Style.TableCells(styles :: List.of(List.of(Style)))
){

 A @rhombus(Style.TableCells, ~annot) object is intended for use as a
 @tech{style property} in a @tech{table}'s style to provide styles to
 individual cells.

@name_itemlist(
 @elem{a table cell}

 @item{A @rhombus(String, ~annot): @html{Used as a CSS class name for a
   @tt{<td>} tag.} @latex{Used as the name of a command to wrap the cell content.}}

)

@property_itemlist(
 @elem{a table cell}

 @item{@symkey(#'left): @all{Left-align the cell content.}}

 @item{@symkey(#'right): @all{Right-align the cell content top baselines.}}

 @item{@symkey(#'center): @all{Center the cell content horizontally.}}

 @item{@symkey(#'top): @all{Top-align the cell content.}}

 @item{@symkey(#'baseline): @all{Align the cell content top baselines.}}

 @item{@symkey(#'bottom): @all{bottom-align the cell content.}}

 @item{@symkey(#'vcenter): @all{Center the cell content vertically.}}

 @item{@symkey(#'border): @all{Draw a line around all sides of the cell.
   Borders along a shared edge of adjacent cells are collapsed into a
   single line.}}

 @item{@symkey(#'#{left-border}), @symkey(#'#{right-border}),
  @symkey(#'#{top-border}), or @symkey(#'#{bottom-border}): @all{Draw a
   line along the corresponding side of the cell (with the same border
   collapsing as for @rhombus(#'border).}}

 @item{A @rhombus(Style.Color, ~annot): @html{Applies a color to the
   cell content.}}

 @item{A @rhombus(Style.BackgroundColor, ~annot): @html{Applies a color
   to the cell background.}}

 @item{A @rhombus(Style.Padding, ~annot): @html{Applies padding around
   the cell content. Supplying @rhombus(~pad) to @rhombus(tabular) adds
   this property to each cell's style if it is not already present.}}

 @item{A @rhombus(Style.HTML.Attributes, ~annot): @html{Provides additional
   attributes for the cell's @tt{<td>} tag.}}

)

  @veneer_same(Style.TableCells, rkt_table_cells)

}

@doc(
  veneer Style.TableColumns
  fun Style.TableColumns(styles :: List.of(Style))
){

 Like @rhombus(Style.TableCells, ~annot), but but with support for a
 @rhombus(Style.HTML.ColumnAttributes, ~annot) property in each style,
 and the styles list is otherwise duplicated for each row in the table.
 The non-@rhombus(Style.HTML.ColumnAttributes, ~annot) parts of a
 @rhombus(Style.TableColumns, ~annot) are used only when a
 @rhombus(Style.TableCells, ~annot) property is not present along with
 the @rhombus(Style.TableColumns, ~annot) property in a @tech{table}.

 @html{For each column that has a
  @rhombus(Style.HTML.ColumnAttributes, ~annot) property in the
  corresponding element of @rhombus(styles), the attributes are put into
  an HTML @tt{<col>} tag within the table.}

 @veneer_same(Style.TableColumns, rkt_table_columns)

}


@doc(
  veneer Style.HTML.ColumnAttributes
  fun Style.HTML.ColumnAttributes(
    attribs :: Map.of(Symbol, String) || List.of([Symbol, String])
  ) :: Style.HTML.ColumnAttributes
){

 Like @rhombus(Style.HTML.Attributes), but specifically for columns in a
 @tech{table}. A @rhombus(Style.HTML.ColumnAttributes) object is normally
 useful as a @tech{style property} within a @tech{style} within
 @rhombus(Style.TableColumns).

 @veneer_same(Style.HTML.ColumnAttributes, rkt_column_attributes_id)

}

@doc(
  veneer Style.Padding
  fun Style.Padding(
    around :: Real = 0,
    ~horiz: horiz :: Real = around,
    ~vert: vert :: Real = around,
    ~left: left :: Real = horiz,
    ~top: top :: Real = vert,
    ~right: right :: Real = horiz,
    ~bottom: bottom :: Real = vert
  ) :: Style.Padding
){

 For cells in a @tech{table}, specifies padding to add around each edge
 of the cell content.

 @veneer_same(Style.Padding, rkt_cell_padding_property)

}
