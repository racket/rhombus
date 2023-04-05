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
                 action :: Function.of_arity(0),
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
                 action :: Function.of_arity(1),
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~is_checked: is_checked :: MaybeObs.of(Boolean) = #false)
){

 Creates a checkbox. When rendered, the function call
 @rhombus(action(#,(@rhombus(now_checked, ~var)))) is performed when the
 checkbox is clicked, where @rhombus(now_checked, ~var) indicates the
 state of the checkbox.

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
