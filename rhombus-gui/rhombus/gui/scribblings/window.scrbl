#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Windows}

@doc(
  class Window():
    implements WindowView
    constructor (
      ~title: title :: MaybeObs.of(String) = "Untitled",
      ~size: size :: MaybeObs.of(Size) = [#false, #false],
      ~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
      ~position: position :: MaybeObs.of(Position) = #'center,
      ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
      ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
      ~styles: styles :: MaybeObs.of(List.of(Window.Style)) = [],
      ~mixin: mix :: Function = values,
      child :: MaybeObs.of(View), ...
    )
){

 Creates a top-level window view that can be instantiated with
 @rhombus(render). The @rhombus(child) views supply the window content.

}

@doc(
  enum Window.Style:
    no_resize_border
    no_caption
    no_system_menu
    hide_menu_bar
    toolbar_button
    float
    metal
    fullscreen_button
    fullscreen_aux
){

  A window style option.

}
