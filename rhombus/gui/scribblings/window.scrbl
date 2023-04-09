#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Windows}

@doc(
  class Window():
    implements WindowView
    constructor (~title: title :: MaybeObs.of(String) = "Untitled",
                 ~size: size :: MaybeObs.of(Size) = [#false, #false],
                 ~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
                 ~position: position :: MaybeObs.of(Position) = #'center,
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
                 ~style: style :: MaybeObs.of(List.of(Window.StyleSymbol)) = [],
                 ~mixin: mix :: Function = values,
                 child :: MaybeObs.of(View),
                 ...)
){

 Creates a top-level window view that can be instantiated with
 @rhombus(render). The @rhombus(child) views supply the window content.

}

@doc(
  annot.macro 'Window.StyleSymbol'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'no_resize_border)}
 @item{@rhombus(#'no_caption)} 
 @item{@rhombus(#'no_system_menu)}
 @item{@rhombus(#'hide_menu_bar)}
 @item{@rhombus(#'toolbar_button)}
 @item{@rhombus(#'float)}
 @item{@rhombus(#'metal)}
 @item{@rhombus(#'fullscreen_button)}
 @item{@rhombus(#'fullscreen_aux)}

)

}
