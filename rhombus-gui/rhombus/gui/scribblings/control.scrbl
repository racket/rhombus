#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "control", ~style: #'toc){Controls}

@local_table_of_contents()

@// ------------------------------------------------------------
@section(~tag: "button", ~style: [#'hidden]){@rhombus(Button)}

@centered{@gallery("button")}

@doc(
  ~page
  class gui.Button():
    implements WindowChildView
    constructor (
      label :: ObsOrValue.of(View.LabelString
                               || draw.Bitmap
                               || [draw.Bitmap,
                                   View.LabelString,
                                   Button.LabelPosition]),
      ~action: action :: () -> ~any = fun (): #void,
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~styles: styles :: List.of(Button.Style) = [],
      ~font : font :: draw.Font = View.normal_control_font,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#false, #false],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )
){

 Creates a button. When rendered, the function call @rhombus(action())
 is performed when the button is clicked.

}

@doc(
  enum gui.Button.Style
  | default
  | multi_line
){

 A button style option:

@itemlist(

 @item{@rhombus(#'default): The button is the default action,
  especially in a @rhombus(Dialog, ~class) where the Return key
  automatically has the effect of clicking a default button.}

 @item{@rhombus(#'multi_line): Newline characters in the button's label
  can create a line break in the button's label.}
)

}

@doc(
  enum gui.Button.LabelPosition
  | left
  | top
  | right
  | bottom
){

 A button label-position option for text combined with an image.

}

@// ------------------------------------------------------------
@section(~tag: "checkbox", ~style: [#'hidden]){@rhombus(Checkbox)}

@centered{@gallery("checkbox")}

@doc(
  ~page
  class gui.Checkbox():
    implements WindowChildView
    constructor (
      label :: ObsOrValue.of(View.LabelString),
      ~is_checked: is_checked :: ObsOrValue.of(Boolean) = #false,
      ~action: action :: Boolean -> ~any = values,
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~styles: styles :: List.of(Checkbox.Style) = [],
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#false, #false],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )

  property (cb :: gui.Checkbox).at_is_checked :: Obs.of(Boolean)
){

 Creates a checkbox. When rendered, the function call
 @rhombus(action(#,(@rhombus(now_checked, ~var)))) is performed when the
 checkbox is clicked, where @rhombus(now_checked, ~var) indicates the
 state of the checkbox.

 The @rhombus(Checkbox.at_is_checked) property returns an observable that
 is updated whenever the checkbox's state changes through an action (as
 also reported via @rhombus(action)) or via @rhombus(is_checked) as an
 observable.

}

@doc(
  enum gui.Checkbox.Style
){

 A checkbox style option. No options are supported, currently.

}

@// ------------------------------------------------------------
@section(~tag: "choice", ~style: [#'hidden]){@rhombus(Choice)}

@centered{@gallery("choice")}

@doc(
  ~page
  class gui.Choice():
    implements WindowChildView
    constructor (
      choices :: ObsOrValue.of(List),
      ~choice_to_label: choice_to_label :: Any -> Any = values,
      ~choice_equal: choice_equal :: Function.of_arity(2) = (_ == _),
      ~selection: selection :: ObsOrValue.of(Any) = #false,
      ~action: action :: Any -> ~any = values,
      ~label: label :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      ~styles: styles :: List.of(Choice.Style) = [],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(Stretch) = [#true, #true],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )

  property (chc :: gui.Choice).at_selection :: Obs
){

 Creates a popup choice selector where @rhombus(choices) provides the
 number and identity of choices, and @rhombus(selection) determines which
 of the tabs is selected. When rendered, the function call
 @rhombus(action(#,(@rhombus(now_selected, ~var)))) is performed when the
 selection is changed, where @rhombus(now_selected, ~var) indicates the
 newly selected choice.

 The @rhombus(Choice.at_selection) property returns an observable that
 is updated whenever the choice popup's state changes through an action
 (as also reported via @rhombus(action)) or via @rhombus(selection) as an
 observable.

 The @rhombus(choice_to_label) function converts an item in
 @rhombus(choices) to a label to be shown for the control, and
 @rhombus(choice_equal) defines equality for choice identities. By
 default, @rhombus(choices) is expected to be a list of
 @rhombus(View.LabelString, ~annot), since @rhombus(choice_to_label) is the identity
 function.

}

@doc(
  enum gui.Choice.Style
  | horizontal_label
  | vertical_label
){

 A choice control style option:

@itemlist(

 @item{@rhombus(#'horizontal_label): When the choice control has a
  label, show it to the left of the control. Horizontal label placement is
  the default if @rhombus(#'vertical_label) is not specified, and
  @rhombus(#'horizontal_label) and @rhombus(#'vertical_label) are mutually
  exclusive.}

 @item{@rhombus(#'vertical_label): When the choice control has a
  label, show it above the control.}

)

}

@// ------------------------------------------------------------
@section(~tag: "radio-choice", ~style: [#'hidden]){@rhombus(RadioChoice)}

@centered{@gallery("radio-choice")}

@doc(
  ~page
  class gui.RadioChoice():
    implements WindowChildView
    constructor (
      choices :: List.of(String),
      ~choice_to_label: choice_to_label :: Any -> Any = values,
      ~choice_equal: choice_equal :: Function.of_arity(2) = (_ == _),
      ~selection: selection :: ObsOrValue.of(Any) = #false,
      ~action: action :: Any -> ~any = values,
      ~label: label :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      ~styles: styles :: List.of(RadioChoice.Style) = [#'vertical],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(Stretch) = [#true, #true],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )

  property (chc :: gui.RadioChoice).at_selection :: Obs
){

 Like @rhombus(gui.Choice), but presented as radio buttons instead of a
 popup menu. Unlike @rhombus(gui.Choice), the @rhombus(choices) list
 cannot be changed.

 The @rhombus(styles) list must include either @rhombus(#'vertical) or
 @rhombus(#'horizontal).

}

@doc(
  enum gui.RadioChoice.Style
  | vertical
  | horizontal
  | horizontal_label
  | vertical_label
){

 A radio choice style option:

@itemlist(

 @item{@rhombus(#'vertical): Arrange radio-button choices vertically.
  Vertical arrangement is the default if @rhombus(#'horzontal) is not
  specified, and @rhombus(#'vertical) and
  @rhombus(#'horizontal) are mutually exclusive.}

 @item{@rhombus(#'horizontal): Arrange radio-button choices
  horizontally.}

 @item{@rhombus(#'horizontal_label): When the radio-choice control has a
  label, show it to the left of the control. Horizontal label placement is
  the default if @rhombus(#'vertical_label) is not specified, and
  @rhombus(#'horizontal_label) and @rhombus(#'vertical_label) are mutually
  exclusive.}

 @item{@rhombus(#'vertical_label): When the radio-choice control has a
  label, show it above the control.}

)

}

@// ------------------------------------------------------------
@section(~tag: "list-choice", ~style: [#'hidden]){@rhombus(ListChoice)}

@centered{@gallery("list-choice")}

@doc(
  ~page
  class gui.ListChoice():
    implements WindowChildView
    constructor (
      choices :: ObsOrValue.of(List),
      ~choice_to_label: choice_to_label :: Any -> Any = values,
      ~choice_equal: choice_equal :: Function.of_arity(2) = (_ == _),
      ~selection: selection :: ObsOrValue.of(Any) = #false,
      ~action: action :: Any -> ~any = values,
      ~label: label :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      ~styles: styles :: List.of(ListChoice.StyleSymbol) = [],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~font : font :: draw.Font = View.normal_control_font,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(Stretch) = [#true, #true],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )

  property (chc :: gui.ListChoice).at_selection :: Obs
){

 Like @rhombus(gui.Choice), but presented as a list box instead of a
 popup menu. The @rhombus(ListChoice) view is a simplified version of
 the @rhombus(Table, ~class) view.

}

@doc(
  enum gui.ListChoice.StyleSymbol
  | horizontal_label
  | vertical_label
){

 A list choice style option:

@itemlist(

  @item{@rhombus(#'horizontal_label): When the list-choice control has a
  label, show it to the left of the control. Horizontal label placement is
  the default if @rhombus(#'vertical_label) is not specified, and
  @rhombus(#'horizontal_label) and @rhombus(#'vertical_label) are mutually
  exclusive.}

  @item{@rhombus(#'vertical_label): When the list-choice control has a
  label, show it above the control.}

)


}

@// ------------------------------------------------------------
@section(~tag: "table", ~style: [#'hidden]){@rhombus(Table)}

@centered{@gallery("table")}

@doc(
  ~page
  class gui.Table():
    implements WindowChildView
    constructor (
      columns :: maybe(List.of(View.LabelString)),
      choices :: ObsOrValue.of(Array),
      ~action: action :: (Table.Event, Array, Table.Selection) -> ~any
                 = values,
      ~choice_to_row: choice_to_row :: Any -> Array.now_of(View.LabelString)
                        = values,
      ~selection: selection :: ObsOrValue.of(Table.Selection) = #false,
      ~label: label :: ObsOrValue.of(View.LabelString) = "",
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~style: style :: List.of(Table.StyleSymbol)
                = [#'single, #'column_headers,
                   #'clickable_headers, #'reorderable_headers],
      ~font : font :: draw.Font = View.normal_control_font,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~column_widths: column_widths :: ObsOrValue.of(List.of(Table.CellWidth))
                        = [],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )

  property (chc :: gui.Table).at_selection :: Obs
){

 Creates a list-row selector where @rhombus(choices) provides column
 labels (and, implicitly, the column column), @rhombus(choices) provides
 the number and identity of rows, and @rhombus(selection) determines
 which of the lines are selected. When rendered, the function call
 @rhombus(action(#,(@rhombus(event, ~var)), #,(@rhombus(choices, ~var)), #,(@rhombus(selection, ~var))))
 is performed when the table is changed, where @rhombus(event, ~var)
 indicates the kind of action, @rhombus(choices, ~var) indicates the
 choices at the time of the event, and choice @rhombus(selection, ~var)
 identifies the selected rows.

 The @rhombus(Table.at_selection) property returns an observable that
 is updated whenever the table's selection state changes through an action
 (as also reported via @rhombus(action)) or via @rhombus(selection) as an
 observable.

 The @rhombus(choice_to_row) function converts an element of
 @rhombus(choices) to an array of label strings to show in the table.

 The @rhombus(style) list must contain exactly one of @rhombus(single),
 @rhombus(#'multiple), and @rhombus(#'extended).

}

@doc(
  enum gui.Table.StyleSymbol
  | single
  | multiple
  | extended
  | column_headers
  | clickable_headers
  | reorderable_headers
  | variable_columns
  | horizontal_label
  | vertical_label
){

 A table style option.

@itemlist(

  @item{@rhombus(#'single): Creates a table where at most one row can be
  selected at a time. On some platforms, the user can deselect the (sole)
  selected item. The @rhombus(#'single), @rhombus(#'multiple), and
  @rhombus(#'extended) styles are mutually exclusive, and one of them must
  be present.}

  @item{@rhombus(#'multiple): Creates a multiple-selection table where a
  single click deselects other rows and selects a new row. Use this
  style for a table when single-selection is common, but multiple
  selections are allowed.}

  @item{@rhombus(#'extended): Creates a multiple-selection table where a
  single click extends or contracts the selection by toggling the clicked
  row. Use this style for a table when multiple selections are the rule
  rather than the exception.

  The @rhombus(#'multiple) and @rhombus(#'extended) styles determine a
  platform-independent interpretation of unmodified mouse clicks, but
  dragging, shift-clicking, control-clicking, etc. have platform-standard
  interpretations. Whatever the platform-specific interface, the user can
  always select disjoint sets of rows or deselect rows (and leave no items
  selected).}

  @item{@rhombus(#'column_headers): Shows the table column names.}

  @item{@rhombus(#'clickable_headers): Allows the user to click a table
  column, where clicks are handled by the table's @rhombus(~action)
  callback.}

  @item{@rhombus(#'reorderable_headers): Allows the user to reorder
  table columns, where the table's @rhombus(~action) callback receives a
  notification of reordering.}

  @item{@rhombus(#'variable_columns): Allows new columns to be added
  dynamically.}

  @item{@rhombus(#'horizontal_label): When the table has a label, show
  it to the left of the control. Horizontal label placement is the default
  if @rhombus(#'vertical_label) is not specified, and
  @rhombus(#'horizontal_label) and @rhombus(#'vertical_label) are mutually
  exclusive.}

  @item{@rhombus(#'vertical_label): When the table has a label, show it
  above the control.}

)

}

@doc(
  enum gui.Table.Event
  | select
  | double_click
  | column
){

 An event provided to the @rhombus(~action) callback function of an
 @rhombus(Table, ~class). A @rhombus(#'select) event reports a change in
 the selection, @rhombus(#'double_click) reports a double click, and
 @rhombus(#'column) indicates a change in column order.

}

@doc(
  annot.macro 'gui.Table.Selection'
){

 Equivalent to @rhombus(maybe(Int || List.of(Int)), ~annot) to represent
 a @rhombus(Table, ~class) selection.

}

@// ------------------------------------------------------------
@section(~tag: "slider", ~style: [#'hidden]){@rhombus(Slider)}

@centered{@gallery("slider")}

@doc(
  ~page
  class gui.Slider():
    implements WindowChildView
    constructor (
      ~label: label :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      ~value: value :: ObsOrValue.of(View.PositionInt) = 0,
      ~min_value: min_value :: ObsOrValue.of(View.PositionInt) = 0,
      ~max_value: max_value :: ObsOrValue.of(View.PositionInt) = 100,
      ~action: action :: View.PositionInt -> ~any = values,
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~styles: styles :: List.of(Slider.Style) = [#'horizontal],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )

  property (sldr :: gui.Slider).at_value :: Obs.of(PositionInt)
){

 Creates a slider. When rendered, the function call
 @rhombus(action(#,(@rhombus(now_val, ~var)))) is performed when the
 slider is changed, where @rhombus(now_val, ~var) indicates the value of
 the slider.

 The @rhombus(Slider.at_value) property returns an observable that
 is updated whenever the slider's value changes through an action
 (as also reported via @rhombus(action)) or via @rhombus(value) as an
 observable.

 The @rhombus(styles) list must include one of @rhombus(#'horiziontal)
 and @rhombus(#'vertical).

}

@doc(
  enum gui.Slider.Style
  | horizontal
  | vertical
  | plain
  | horizontal_label
  | vertical_label
){

 A slider style option:

@itemlist(

 @item{@rhombus(#'horizontal): Creates a horizontal slider, where
  @rhombus(#'vertical) and @rhombus(#'horizontal) are mutually exclusive.}

 @item{@rhombus(#'vertical): Creates a vertical slider.}

 @item{@rhombus(#'plain): Creates an unadorned slider where the slider's
  numerical value is not reported to the user.}

 @item{@rhombus(#'horizontal_label): When the slider has a label, show
  it to the left of the slider. Horizontal label placement is the default
  if @rhombus(#'vertical_label) is not specified, and
  @rhombus(#'horizontal_label) and @rhombus(#'vertical_label) are mutually
  exclusive.}

 @item{@rhombus(#'vertical_label): When the slider has a label, show it
  above the control.}

)

}

@// ------------------------------------------------------------
@section(~tag: "progress", ~style: [#'hidden]){@rhombus(Progress)}

@centered{@gallery("progress")}

@doc(
  ~page
  class gui.Progress():
    implements WindowChildView
    constructor (
      value :: ObsOrValue.of(View.SizeInt) = 0,
      ~label: label :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      ~max_value: max_value :: ObsOrValue.of(View.PosSizeInt) = 100,
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~styles: styles :: List.of(Progress.Style) = [#'horizontal],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )

  property (prog :: gui.Progress).at_value :: Obs.of(SizeInt)
){

 Creates a read-only (from the user's perspective) progress gauge that
 show @rhombus(value) out of @rhombus(max_value) progress.

 The @rhombus(styles) list must include one of @rhombus(#'horiziontal)
 and @rhombus(#'vertical).

 The @rhombus(Progress.at_value) property returns an observable that is
 updated whenever the progress gauge's state changes through
 @rhombus(value) as an observable.

}

@doc(
  enum gui.Progress.Style
  | horizontal
  | vertical
  | horizontal_label
  | vertical_label
){

 A progress-gauge style option:

@itemlist(

 @item{@rhombus(#'horizontal): Creates a horizontal progress gauge,
  where @rhombus(#'vertical) and @rhombus(#'horizontal) are mutually
  exclusive.}

 @item{@rhombus(#'vertical): Creates a vertical progress gauge.}

 @item{@rhombus(#'horizontal_label): When the progress gauge has a label, show
  it to the left of the gauge. Horizontal label placement is the default
  if @rhombus(#'vertical_label) is not specified, and
  @rhombus(#'horizontal_label) and @rhombus(#'vertical_label) are mutually
  exclusive.}

 @item{@rhombus(#'vertical_label): When the progress gauge has a label,
  show it above the gauge.}

)

}

@// ------------------------------------------------------------
@section(~tag: "input", ~style: [#'hidden]){@rhombus(Input)}

@centered{@(gallery("input").pad(~top: -1).clip())
          @hspace(1)
          @(gallery("choice-input"))}

@doc(
  ~page
  class gui.Input():
    implements WindowChildView
    constructor (
      content :: ObsOrValue.of(Any),
      ~action: action :: maybe((Input.Event, String) -> ~any) = #false,
      ~label: label :: ObsOrValue.of(View.LabelString) = "",
      ~choices: choices :: maybe(ObsOrValue.of(List.of(View.LabelString)))
                  = #false,
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~background_color: bg_color :: ObsOrValue.of(maybe(Color)) = #false,
      ~styles: styles :: List.of(Input.StyleSymbol) = [#'single],
      ~font : font :: draw.Font = View.normal_control_font,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch)
                  = [#true, #'multiple in styles],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false,
      ~is_equal_value: is_equal :: Function.of_arity(2) = (_ == _),
      ~value_to_text: val_to_txt :: Function = values
    )

  property (inp :: gui.Input).at_content :: Obs.of(Any)
){

  Returns a representation of a text field that calls @rhombus(action) on change.
  The first argument to the @rhombus(action) is the type of event that caused
  the input to change and the second is the contents of the text field.

  If the @rhombus(~choices) argument is not @rhombus(#false), it provides
  a list of suggestions via a popup menu. When a user selects an item in
  the popup menu, it is copied into the text field.

  The @rhombus(~is_equal_value) argument controls when changes to the input data
  are reflected in the contents of the field. The contents of the input field only
  change when the new value of the underlying observable is not @rhombus(==) to the
  previous one. The only exception to this is when the textual value
  (via @rhombus(~value_to_text)) of the observable is the empty string, in which case
  the input is cleared regardless of the value of the underlying observable.

  The @rhombus(~value_to_text) argument controls how the input values are rendered
  to strings. If not provided, value must be either a string? or an observable
  of strings.

  The @rhombus(Input.at_content) property returns an observable that
  is updated whenever the input's value changes through an action
  (as also reported via @rhombus(action)) or via @rhombus(content) as an
  observable.

  The @rhombus(styles) list must contain exactly one of
  @rhombus(#'single) and @rhombus(#'multiple).

}

@doc(
  enum gui.Input.StyleSymbol
  | single
  | multiple
  | password
  | horizontal_label
  | vertical_label
  | hscroll
){

 An input style option:

@itemlist(

  @item{@rhombus(#'single): Shows a single line of text input, and
  treats the Enter or Return key as a special event. The input content is
  not automatically prevented from containing a newline character in
  pasted text, however.}

  @item{@rhombus(#'single): Shows multiple lines of text input, and
  creates a new line of text in response to the Enter or Return key.}

  @item{@rhombus(#'password): Renders characters typed into the input
  field as bullets.}

  @item{@rhombus(#'horizontal_label): When the input field has a label, show
  it to the left of the field. Horizontal label placement is the default
  if @rhombus(#'vertical_label) is not specified, and
  @rhombus(#'horizontal_label) and @rhombus(#'vertical_label) are mutually
  exclusive.}

  @item{@rhombus(#'vertical_label): When the progress gauge has a label,
  show it above the gauge.}

)


}

@doc(
  enum gui.Input.Event
  | input
  | return
  | focus_in
  | focus_out
){

 An event provided to the @rhombus(~action) callback function of an
 @rhombus(Input, ~class). The event @rhombus(#'input) corresponds to any
 change to the input text, while @rhombus(#'return) indicates that the
 Return or Enter key was pressed for an input field using the style
 @rhombus(#'single). The @rhombus(#'focus_in) and @rhombus(#'focus_out)
 events report keyboard-focus changes.

}

@// ------------------------------------------------------------
@section(~tag: "label", ~style: [#'hidden]){@rhombus(Label)}

@centered{@gallery("label")}

@doc(
  ~page
  class gui.Label():
    implements WindowChildView
    constructor (
      label :: ObsOrValue.of(View.LabelString),
      ~color: color :: ObsOrValue.of(maybe(Color)) = #false,
      ~font: font :: draw.Font = View.normal_control_font,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~styles: styles :: List.of(Progress.Style) = [#'horizontal],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )

  property (lbl :: gui.Label).at_label :: Obs.of(View.LabelString)
){

 Creates a text label.

 The @rhombus(Label.at_label) property returns an observable that is
 updated whenever the label's state changes through
 @rhombus(label) as an observable.

}


@// ------------------------------------------------------------
@section(~tag: "image", ~style: [#'hidden]){@rhombus(Image)}

@centered{@gallery("image")}

@doc(
  ~page
  class gui.Image():
    implements WindowChildView
    constructor (
      content :: ObsOrValue.of(PathString || draw.Bitmap),
      ~size: size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~display: display :: ObsOrValue.of(Image.DisplayMode) = #'fit,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [2, 2],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )
){

 Creates a bitmap image display showing either a
 @rhombus(draw.Bitmap, ~class) instance or one that is loaded from a
 specified file path.

 When @rhombus(size) has @rhombus(#false) for a dimension, then the size
 of @rhombus(content) is used. Within that size, the image is either
 scaled to fit in both dimensions when @rhombus(display) is
 @rhombus(#'fit), or it is stretched to fill in both dimensions when
 @rhombus(display) is @rhombus(#'fill).

 If the image view has a different size based on @rhombus(min_size) and
 @rhombus(stretch), then the image is centered within the view's area.

}

@doc(
  enum Image.DisplayMode
  | fit
  | fill
){

 Scaling options for @rhombus(Image).

}

@// ------------------------------------------------------------
@section(~tag: "spacer", ~style: [#'hidden]){@rhombus(Spacer)}

@doc(
  ~page
  class gui.Spacer():
    implements WindowChildView
    constructor (
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )
){

 Returns a representation of a spacer. By default, spacers extend to
 fill the space of their parents.

}
