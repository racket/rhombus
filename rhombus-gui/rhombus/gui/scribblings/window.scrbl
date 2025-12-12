#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Windows}

@(~version_at_least "8.14.0.4")

@doc(
  class gui.Window():
    implements WindowView
    constructor (
      ~title: title :: ObsOrValue.of(String) = "Untitled",
      ~size: size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~alignment: alignment :: ObsOrValue.of(View.Alignment) = [#'center, #'top],
      ~position: position :: ObsOrValue.of(View.Position) = #'center,
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~styles: styles :: ObsOrValue.of(List.of(Window.Style)) = [],
      ~mixin: mix :: Function = values,
      child :: ObsOrValue.of(View), ...
    )
){

 Creates a top-level window view that can be instantiated with
 @rhombus(render). The @rhombus(child) views supply the window content.

}

@doc(
  enum gui.Window.Style
  | no_resize_border
  | no_caption
  | no_system_menu
  | hide_menu_bar
  | toolbar_button
  | float
  | metal
  | fullscreen_button
  | fullscreen_aux
){

  A window style option.

}
