#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Controls}

@(~version_at_least "8.14.0.4")

@doc(
  class gui.Button():
    implements View
    constructor (
      label :: ObsOrValue.of(View.LabelString
                               || draw.Bitmap
                               || [draw.Bitmap,
                                   View.LabelString,
                                   Button.LabelPosition]),
      ~action: action :: () -> ~any = fun (): #void,
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~styles: styles :: ObsOrValue.of(List.of(Button.Style)) = [],
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
    )
){

 Creates a button. When rendered, the function call @rhombus(action())
 is performed when the button is clicked.

}

@doc(
  class gui.Checkbox():
    implements View
    constructor (
      label :: ObsOrValue.of(View.LabelString),
      ~is_checked: is_checked :: ObsOrValue.of(Boolean) = #false,
      ~action: action :: maybe(Boolean -> ~any) = #false,
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
    )

  property (cb :: gui.Checkbox).at_is_checked :: Obs.of(Boolean)
){

 Creates a checkbox. When rendered, the function call
 @rhombus(action(#,(@rhombus(now_checked, ~var)))) is performed when the
 checkbox is clicked, where @rhombus(now_checked, ~var) indicates the
 state of the checkbox.

 If @rhombus(is_checked) is not an observable, then an observable
 @rhombus(at_is_checked, ~var) is created with initial value
 @rhombus(is_checked). Otherwise, @rhombus(at_is_checked, ~var) is
 @rhombus(is_checked). A observable derived from
 @rhombus(at_is_checked, ~var) can be obtained from the
 @rhombus(Checkbox.at_is_checked) property.

 The default @rhombus(set_is_checked, ~var) function for @rhombus(action)
 corresponds to

@rhombusblock(
  fun (on):
    #,(@rhombus(at_is_checked, ~var)).value := on
)

}

@doc(
  class gui.Choice():
    implements View
    constructor (
      choices :: ObsOrValue.of(List),
      ~choice_to_label: choice_to_label :: Any -> Any = values,
      ~choice_equal: choice_equal :: Function.of_arity(2) = (_ == _),
      ~selection: selection :: ObsOrValue.of(Any) = #false,
      ~action: action :: maybe(Any -> ~any) = #false,
      ~label: label :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      ~styles: styles :: ObsOrValue.of(List.of(Choice.Style)) = [],
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~min_size: min_size :: ObsOrValue.of(Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(Stretch) = [#true, #true],
    )

  property (chc :: gui.Choice).at_selection :: Obs
){

 Creates a popup choice selecotr where @rhombus(choices) provides the
 number and identity of choices, and @rhombus(selection) determines which
 of the tabs is selected. When rendered, the function call
 @rhombus(action(#,(@rhombus(now_selected, ~var)))) is performed when the
 selection is changed, where @rhombus(now_selected, ~var) indicates the
 newly selected choice.

 If @rhombus(selection) is not an observable, then an observable
 @rhombus(at_selection, ~var) is created with initial value
 @rhombus(selection). Otherwise, @rhombus(at_selection, ~var) is
 @rhombus(selection). A observable derived from
 @rhombus(at_selection, ~var) can be obtained from the
 @rhombus(Choice.at_selection) property.

 The @rhombus(choice_to_label) function converts an item in
 @rhombus(choices) to a label to be shown for the control, and
 @rhombus(choice_equal) defines equality for choice identities. By
 default, @rhombus(choices) is expected to be a list of
 @rhombus(View.LabelString, ~annot), since @rhombus(choice_to_label) is the identity
 function.

 The default @rhombus(action, ~var) function corresponds to

@rhombusblock(
  fun (selected):
    #,(@rhombus(at_selection, ~var)).value := selected
)

}

@doc(
  class gui.Slider():
    implements View
    constructor (
      label :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      ~value: value :: ObsOrValue.of(View.PositionInt) = 0,
      ~min_value: min_value :: ObsOrValue.of(View.PositionInt) = 0,
      ~max_value: max_value :: ObsOrValue.of(View.PositionInt) = 100,
      ~action: action :: maybe(View.PositionInt -> ~any) = #false,
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~styles: styles :: List.of(Slider.Style) = [#'horizontal],
    )

  property (sldr :: gui.Slider).at_value :: Obs.of(PositionInt)
){

 Creates a slider. When rendered, the function call
 @rhombus(action(#,(@rhombus(now_val, ~var)))) is performed when the
 slider is changed, where @rhombus(now_val, ~var) indicates the value of
 the slider.

 If @rhombus(value) is not an observable, then an observable
 @rhombus(at_value, ~var) is created with initial value
 @rhombus(value). Otherwise, @rhombus(at_value, ~var) is
 @rhombus(value). A observable derived from
 @rhombus(at_value, ~var) can be obtained from the
 @rhombus(Checkbox.at_value) property.

 The default @rhombus(set_value, ~var) function for @rhombus(action)
 corresponds to

@rhombusblock(
  fun (val):
    #,(@rhombus(at_value, ~var)).value := val
)

}


@doc(
  class gui.Label():
    implements View
    constructor (
      label :: ObsOrValue.of(View.LabelString),
      ~color: color :: ObsOrValue.of(maybe(Color)) = #false,
      ~font: font :: ObsOrValue.of(Font) = Label.normal_control_font,
    )

  property (lbl :: gui.Label).at_label :: Obs.of(View.LabelString)
){

 Creates a text label.

 If @rhombus(label) is not an observable, then an observable
 @rhombus(at_label, ~var) is created with initial value
 @rhombus(value). Otherwise, @rhombus(at_label, ~var) is
 @rhombus(value). A observable derived from
 @rhombus(at_value, ~var) can be obtained from the
 @rhombus(Label.at_label) property.

}


@doc(
  class gui.Input():
    implements View
    constructor (
      content :: ObsOrValue.of(Any),
      ~action: action :: maybe((Input.Action, String) -> ~any) = #false,
      ~label: label :: ObsOrValue.of(View.LabelString) = "",
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~background_color: bg_color :: ObsOrValue.of(maybe(Color)) = #false,
      ~styles: styles :: ObsOrValue.of(List.of(Input.Style)) = [#'single],
      ~font : font :: Font = normal_control_font,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~mixin: mixin :: Function = values,
      ~is_equal_value: is_equal :: maybe(Function.of_arity(2)) = (fun (a, b): a == b),
      ~value_to_text: val_to_txt :: Function = values
    )
){

  Returns a representation of a text field that calls @rhombus(action) on change.
  The first argument to the @rhombus(action) is the type of event that caused
  the input to change and the second is the contents of the text field.

  The @rhombus(~is_equal_value) argument controls when changes to the input data
  are reflected in the contents of the field. The contents of the input field only
  change when the new value of the underlying observable is not @rhombus(==) to the
  previous one. The only exception to this is when the textual value
  (via @rhombus(~value_to_text)) of the observable is the empty string, in which case
  the input is cleared regardless of the value of the underlying observable.

  The @rhombus(~value_to_text) argument controls how the input values are rendered
  to strings. If not provided, value must be either a string? or an observable
  of strings.

}


@doc(
  class gui.Spacer():
    implements View
){

   Returns a representation of a spacer.  Spacers extend to fill the
   space of their parents.

}


@doc(
  enum gui.Button.Style:
    border
    multi_line
    deleted
){

 A button style option.

}

@doc(
  enum gui.Button.LabelPosition:
    left
    top
    right
    bottom
){

 A button label-position option for text combined with an image.

}


@doc(
  enum gui.Choice.Style:
    horizontal_label
    vertical_label
    deleted
){

 A choice control style option.

}


@doc(
  enum gui.Slider.Style:
    horizontal
    vertical
    plain
    horizontal_label
    vertical_label
    deleted
){

 A slider style option.

}


@doc(
  enum gui.Input.StyleSymbol:
    deleted
    horizontal_label
    hscroll
    multiple
    password
    single
    vertical_label
){

 An input style option.

}

@doc(
  enum gui.Input.Action:
    input
    return
){

 An action provided to the @rhombus(~action) callback function of an
 @rhombus(Input, ~class). The action @rhombus(#'input) corresponds to any
 change to the input text, while @rhombus(#'return) indicates that the
 Return or Enter key was pressed.

}
