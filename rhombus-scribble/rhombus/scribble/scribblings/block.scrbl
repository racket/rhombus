#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open)

@title(~tag: "block"){Flow Blocks}

@doc(~nonterminal: paragraph_style: Paragraph
     ~include rhombus/scribble/private/block: para){

 Creates a @tech{paragraph}. Most paragraphs are created implicitly by
 @tech{decoding}, but @rhombus(para) is useful for constructing a
 @tech{flow block} explicitly or giving a @tech{style} to the paragraph.

 See @rhombus(paragraph_style) for information about @tech{styles} for
 paragraphs.

}

@doc(~nonterminal: nested_flow_style: NestedFlow
     ~include rhombus/scribble/private/block: nested){

 Creates a @tech{nested flow} as a single @tech{flow block}.

 See @rhombus(nested_flow_style) for information about @tech{styles} for
 nested flows.

}

@doc(~include rhombus/scribble/private/block: centered){

 Creates a @tech{nested flow} whose contented is centered with respect
 to the enclosing flow, if centering is supported by the @tech{renderer}
 that is used to render the document.

 Calling @rhombus(centered) is equivalent to calling @rhombus(nested)
 with a centering style.

}

@doc(~include rhombus/scribble/private/block:
       margin_note
       margin_note_block
       MarginSide){

 Creates @tech{content} or a @tech{flow block} that is typeset in the
 left or right margin, if maring notes are supported by the
 @tech{renderer} that is used to render the document.

}

@doc(~nonterminal: itemization_style: Itemization
     ~include rhombus/scribble/private/block:
       itemlist
       item
     annot.macro 'Item'){

 The @rhombus(itemlist) function creates an @tech{itemization}.

 The @rhombus(item) function provides an @rhombus(Item, ~annot) that is
 ultimately only useful with @rhombus(itemlist).
 @annot_same(Item, rkt_item)

 See @rhombus(itemization_style) for information about @tech{styles} for
 itemizations.

}

@doc(~include rhombus/scribble/private/block: verbatim){

 Renders the strings of @rhombus(content) literally and in a fixed-width
 font. No @tech{decoding} is used for strings among the @rhombus(content).

 The given @rhombus(content) can include non-string element, and those
 are rendered as they normally would be, which is not ``verbatim.'' Only
 string elements of @rhombus(content) (possibly nested in lists) are
 rendered verbatim.

}

@doc(~nonterminal: table_style: Table
     ~include rhombus/scribble/private/block:
       tabular Cell){

 Creates a @tech{table} for a two-dimentional layout of @tech{flow
  blocks}. Each list in @rhombus(cells) is a row in the table.

 An element in @rhombus(cells) can be a @tech{flow block},
 @tech{content} that is coerced to a @tech{flow block} by wrapping it as
 a @tech{paragraph}, or @rhombus(#'cont). A @rhombus(#'cont) cell makes
 sense only after the first cell in a row, and it causes the content of
 the previous column in the row to continue into the cell's row. Multiple
 adjacent @rhombus(#'cont) values allow content to span more than two
 columns.

 The @rhombus(col_props), @rhombus(row_props), and @rhombus(cell_props)
 arguments all supply properties to be used for indvidual cells:

@itemlist(

 @item{The elements of @rhombus(col_props) are given to every column and
  every cell in the corresponding column, and if there are more columns
  than elements in @rhombus(col_props), the last element is repeated for
  all remaining columns. Each element is either a single @tech{style
   property} value or a list of @tech{style property} values to associate
  with a cell, where a single @tech{style property} value is equivaent to
  a list containing that value.}

 @item{The elements of @rhombus(row_props) are similarly given to every
  cell in the corresponding row, repeating the last element as needed.}

 @item{The lists and elements of @rhombus(cell_props) are similarly
  repeated as needed to match the number of columns and rows in
  @rhombus(cells).}

)

 An empty list for any of @rhombus(col_props), @rhombus(row_props), and
 @rhombus(cell_props) is the same as provding @rhombus([[]]). When
 multiple property lists are provided for a cell by @rhombus(col_props),
 @rhombus(row_props), and @rhombus(cell_props), the lists are all
 appended for the cell. Note that @rhombus(col_props) provides properties
 both for @emph{columns} (for which certiain properties are recognized)
 and for individual @emph{cells} (for which other properties are
 recognized), but @rhombus(row_props) and @rhombus(cell_props) provide
 only properties for indvidual cells.

 See @rhombus(table_style) for information about @tech{styles} for
 tables. A @rhombus(table_style) can have column and cell styles via
 @rhombus(Style.TableColumns, ~annot) and
 @rhombus(Style.TableCells, ~annot) properties. Any styles from
 @rhombus(col_props) are merged with styles in a
 @rhombus(Style.TableColumns, ~annot) property, and any styles from
 @rhombus(row_props) and @rhombus(cell_props) are merged with a
 @rhombus(Style.TableCells, ~annot) property.

 If @rhombus(sep) is not @rhombus(#false), then it is used for a column
 added between every column in a row (except for columns that continue
 via @rhombus(#'cont). For example, @rhombus(sep) could be
 @rhombus(@hspace(1)) to ensure space between columns. These extra
 columns do not count for distributing properties from
 @rhombus(col_props), @rhombus(row_props), and @rhombus(cell_props);
 instead, each inserted column gets the same properties as te preceding
 cell within the row, unless @rhombus(sep_props) is a list to provide
 properties for the added columns.

}
