#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Buttons}

@doc(
  class Button():
    implements View
    constructor (label :: MaybeObs.of(LabelString
                                        || Bitmap
                                        || matching([_ :: Bitmap,
                                                     _ :: LabelString,
                                                     _ :: Button.LabelPosition])),
                 action :: Function,
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
