#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "Windows"){Windows and Dialogs}

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
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~styles: styles :: ObsOrValue.of(List.of(Window.Style)) = [],
      ~menu_bar: menu_bar :: maybe(MenuBar) = #false,
      child :: ObsOrValue.of(WindowChildView), ...
    )
){

 Creates a top-level window view that can be instantiated with
 @rhombus(render). The @rhombus(child) views supply the window content.

}

@doc(
  class gui.Dialog():
    implements WindowView
    constructor (
      ~title: title :: ObsOrValue.of(String) = "Untitled",
      ~size: size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~alignment: alignment :: ObsOrValue.of(View.Alignment) = [#'center, #'top],
      ~position: position :: ObsOrValue.of(View.Position) = #'center,
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~styles: styles :: ObsOrValue.of(List.of(Dialog.Style)) = [],
      ~menu_bar: menu_bar :: maybe(MenuBar) = #false,
      child :: ObsOrValue.of(WindowChildView), ...
    )
){

 Similar to @rhombus(Window), but creates a modal dialog, instead.

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

@doc(
  enum gui.Dialog.Style
  | no_caption
  | no_sheet
  | resize_border
  | close_button
){

  A dialog style option.

}
