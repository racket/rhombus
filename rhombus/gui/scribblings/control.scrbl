#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Controls}

@doc(
  class Button():
    implements View
    constructor (label :: MaybeObs.of(LabelString
                                        || Bitmap
                                        || matching([_ :: Bitmap,
                                                     _ :: LabelString,
                                                     _ :: Button.LabelPosition])),
                 ~action: action :: Function.of_arity(0) = fun (): #void,
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~style: style :: MaybeObs.of(List.of(Button.StyleSymbol)) = [],
                 ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true])
){

 Creates a button. When rendered, the function call @rhombus(action())
 is performed when the button is clicked.

}

@doc(
  class Checkbox():
    implements View
    constructor (label :: MaybeObs.of(LabelString),
                 ~is_checked: is_checked :: MaybeObs.of(Boolean) = #false,
                 ~action: action :: Function.of_arity(1) = #,(@rhombus(set_is_checked, ~var)),
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true)

  property (cb :: Checkbox).at_is_checked :: Obs.of(Boolean)
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
  class Choice():
    implements View
    constructor (choices :: MaybeObs.of(List),
                 ~choice_to_label: choice_to_label :: Function.of_arity(1) = values,
                 ~choice_equal: choice_equal :: Function.of_arity(2) = (fun (a, b): a == b),
                 ~selection: selection :: MaybeObs.of(Any) = #false,
                 ~action: action :: maybe(Function.of_arity(1)) = #false,
                 ~label: label :: MaybeObs.of(maybe(LabelString)) = #false,
                 ~style: style :: MaybeObs.of(List.of(Choice.StyleSymbol)) = [],
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true])

  property (chc :: Choice).at_selection :: Obs
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
 @rhombus(LabelString), since @rhombus(choice_to_label) is the identity
 function.

 The default @rhombus(action, ~var) function corresponds to

@rhombusblock(
  fun (selected):
    #,(@rhombus(at_selection, ~var)).value := selected
)

}

@doc(
  class Slider():
    implements View
    constructor (label :: MaybeObs.of(maybe(LabelString)) = #false,
                 ~value: value :: MaybeObs.of(PositionInteger) = 0,
                 ~min_value: min_value :: MaybeObs.of(PositionInteger) = 0,
                 ~max_value: max_value :: MaybeObs.of(PositionInteger) = 100,
                 ~action: action :: maybe(Function.of_arity(1)) = #false,
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
                 ~style: style :: List.of(Slider.StyleSymbol) = [#'horizontal])

  property (sldr :: Slider).at_value :: Obs.of(PositionInteger)
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
  class Label():
    implements View
    constructor (label :: MaybeObs.of(LabelString),
                 ~color: color :: MaybeObs.of(maybe(Color)) = #false,
                 ~font: font :: MaybeObs.of(Font) = Label.normal_control_font)

  property (lbl :: Label).at_label :: Obs.of(LabelString)
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
  annot.macro 'Button.StyleSymbol'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'border)}
 @item{@rhombus(#'multi_line)} 
 @item{@rhombus(#'deleted)}

)

}

@doc(
  annot.macro 'Button.LabelPosition'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'left)}
 @item{@rhombus(#'top)} 
 @item{@rhombus(#'right)}
 @item{@rhombus(#'bottom)}

)

}


@doc(
  annot.macro 'Choice.StyleSymbol'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'horizontal_label)}
 @item{@rhombus(#'vertical_label)}
 @item{@rhombus(#'deleted)}

)

}




@doc(
  annot.macro 'Slider.StyleSymbol'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'horizontal)}
 @item{@rhombus(#'vertical)} 
 @item{@rhombus(#'plain)} 
 @item{@rhombus(#'horizontal_label)}
 @item{@rhombus(#'vertical_label)} 
 @item{@rhombus(#'deleted)}

)

}

