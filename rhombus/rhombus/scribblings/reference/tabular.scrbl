#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/tabular open)

@(def tab_eval = make_rhombus_eval())
@examples(
  ~eval: tab_eval
  ~hidden:
    import rhombus/tabular open
)

@title{Table Formatting}

@docmodule(rhombus/tabular)

The @rhombusmodname(rhombus/tabular) module extends the
@rhombus(str) namespace with @rhombus(str.tabular) for
formatting a two-dimensional table of strings.

@doc(
  fun str.tabular(
    [[cell :: str.Cell, ...], ...],
    ~pad: pad :: str.CellPadding || Nat || [Nat, Nat] || [Nat, Nat, Nat, Nat]
            = 0,
    ~column_properties: colum_properties :: List.of(str.CellProperties)
                          = [],
    ~row_properties: row_properties :: List.of(str.CellProperties)
                       = [],
    ~cell_properties: cell_properties :: List.of(List.of(str.CellProperties))
                        = []
  ) :: String

  enum str.Cell:
    ~is_a String
    cont
){

 Returns a string that formats the given @rhombus(cell)s into a table.
 Each inner list of @rhombus(cell)s represents a row, so the argument to
 @rhombus(str.tabular) is a list of rows. The width and height (in
 characters) of each row and column is inferred, while @rhombus(pad),
 @rhombus(cell_properties), @rhombus(column_properties), and
 @rhombus(row_properties) are merged to determine padding space around a
 cell, horizontal alignment, vertical alignment, and where borders are
 drawn around or between cells.

@examples(
  ~eval: tab_eval
  println(str.tabular([["apple", "banana"],
                       ["cherry", "durian"]]))
  println(str.tabular([["apple", "banana"],
                       ["cherry", "durian"]],
                      ~pad: [1, 0],
                      ~cell_properties: [[#'border]]))
)

 The resulting string has newlines for a table that has multiple rows,
 but it does not end in a newline after the last row. Each @rhombus(cell)
 string can itself contain newlines, in which case the corresponding cell
 will span multiple lines, and the resulting string will have newlines
 even if it formats just one row.

@examples(
  ~eval: tab_eval
  println(str.tabular(
            [["fruits", str.tabular([["apple", "banana"],
                                     ["cherry", "durian"]],
                                    ~pad: [1, 0],
                                    ~cell_properties: [[#'border]])],
             ["veggies", str.tabular([["eggplant"]],
                                     ~cell_properties: [[#'border]])]],
            ~pad: [1, 0],
            ~cell_properties: [[#'vcenter]]
          ))
)

 A @rhombus(cell) that is not in the first column of a row can be
 @rhombus(#'cont) to indicate that the previous cell's content continues
 into the next column. Table formatting ensures that spanned columns via
 @rhombus(#'cont) are together wide enough for the cell content, instead
 of requiring just the content's first cell to fit the entire content.

@examples(
  ~eval: tab_eval
  println(str.tabular([["apple", "banana"],
                       ["cantaloupe", #'cont]],
                      ~pad: [1, 0],
                      ~cell_properties: [[#'border]]))
)

 If @rhombus(pad) is not a @rhombus(str.CellPadding), it is converted to
 one by treating a single as a padding for all sizes, a list of two
 @rhombus(Nat, ~annot)s and horizontal and vertical padding, and a list
 of four @rhombus(Nat, ~annot) as left, top, right, and bottom padding.
 The normalized @rhombus(pad) becomes the default property for all cells.

 Each property (or list of propertie) in @rhombus(colum_properties) is
 then combined with the properties accumulated so far for each cell in
 the corresponding column, where the last elemen of
 @rhombus(colum_properties) (is any) is duplicated when thete are more
 column @rhombus(cell)s than elements of @rhombus(colum_properties).
 Combining means that properties in @rhombus(colum_properties) can
 complement or supercede the padding specified as @rhombus(pad).

 Along similar lines, each property (or list of properties) in
 @rhombus(row_properties) is combined, dupliacting the last element of
 @rhombus(row_properties) as needed to match the number of @rhombus(cell)
 rows, complementing and superceding any properties from
 @rhombus(colum_properties).

 Finally, @rhombus(cell_properties) provides cell-specific properties,
 where the last lists of @rhombus(cell_properties) is duplicated as need
 to match the number of @rhombus(cell) rows, and the last element of each
 list in @rhombus(cell_properties) is duplicated as need to match the
 @rhombus(cell) columns. These cell-specific properties complement or
 supercede properties from @rhombus(row_properties),
 @rhombus(colum_properties), and @rhombus(pad).

@examples(
  ~eval: tab_eval
  println(str.tabular(
            [["apple", "banana"],
             ["cherry", "durian"]],
            ~pad: [1, 0],
            ~row_properties: [#'top_border, #'bottom_border],
            ~column_properties: [[#'left_border, #'right_border], #'border]
          ))
)

 The default alignment for a cell is @rhombus(#'left) and
 @rhombus(#'top), and a cell has no broders by default.

}

@doc(
  enum str.CellProperty:
    ~is_a str.Align
    ~is_a str.VerticalAlign
    ~is_a str.Border
    ~is_a str.CellPadding

  annot.macro 'str.CellProperties'
){

 A @rhombus(str.CellProperty, ~annot) describes formatting options for an
 individual cell in a table formatted with @rhombus(str.tabular).

 The @rhombus(str.CellProperties, ~annot) annotation is equivalent to
 @rhombus(str.CellProperty || List.of(str.CellProperty), ~annot). The
 individual elements in a list of cell properties provided to
 @rhombus(str.tabular) satisfy @rhombus(str.CellProperties, ~annot),
 which means that each can be a single property or a list of properties
 to combine for a cell.

}


@doc(
  enum str.VerticalAlign:
    top
    vcenter
    bottom
){

 Vertical alignment options for a cell formatted by
 @rhombus(str.tabular). Horizontal alignment uses
 @rhombus(str.Align, ~annot) as provided directly by
 @rhombuslangname(rhombus).

}

@doc(
  enum str.Border:
    border
    top_border
    left_border
    right_border
    bottom_border
){

 Border options for or a cell formatted by @rhombus(str.tabular). The
 @rhombus(#'border) option is equivalent to specifying all of
 @rhombus(#'top_border), @rhombus(#'left_border),
 @rhombus(#'right_border), and @rhombus(#'bottom_border).

 Merging for border options is cumulative, combining
 @rhombus(#'left_border) with an existing @rhombus(#'border) option is
 the same as @rhombus(#'border), not a reduction in borders to just
 @rhombus(#'left_border).

 Borders overlaop on the shared edge of adjacent cells. For example, if
 one cell has @rhombus(#'right_border) and the cell just to its right has
 @rhombus(#'left_border), then only one border is drawn, and it's the
 same if either @rhombus(#'right_border) or @rhombus(#'left_border) (but
 not both) is removed.

}


@doc(
  class str.CellPadding(left :: Nat,
                        top :: Nat,
                        right :: Nat,
                        bottom :: Nat):
    constructor (around :: Real = 0,
                 ~horiz: horiz :: Real = around,
                 ~vert: vert :: Real = around,
                 ~left: left :: Real = horiz,
                 ~top: top :: Real = vert,
                 ~right: right :: Real = horiz,
                 ~bottom: bottom :: Real = vert)
){

 Describes padding for a cell as formatted by @rhombus(str.tabular). For
 each direction, a padding of @rhombus(amt, ~var) adds
 @rhombus(amt, ~var) space characters (before a border, if any).

}

@close_eval(tab_eval)
