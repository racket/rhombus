#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "screenshot.rhm" open)

@title(~tag: "Windows", ~style: #'toc){Windows}

Use @rhombus(Window, ~annot) for movable windows that do not prevent a
user from switching to other windows in the application, and use
@rhombus(Dialog, ~annot) for modal dialogs that must be closed by a user
to access other windows.

@local_table_of_contents()

@// ------------------------------------------------------------
@section(~tag: "window", ~style: [#'hidden]){@rhombus(Window)}

@screenshot(gallery_file("window"))

@doc(
  ~page
  class gui.Window():
    implements WindowView
    constructor (
      ~title: title :: ObsOrValue.of(String) = "Untitled",
      ~style: style :: ObsOrValue.of(List.of(Window.Style)) = [],
      ~size: size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~position: position :: ObsOrValue.of(View.Position) = #'center,
      ~align: align :: ObsOrValue.of(View.Alignment) = [#'center, #'center],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~menu_bar: menu_bar :: maybe(MenuBar) = #false,
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false,
      child :: ObsOrValue.of(WindowChildView && !WindowView), ...
    )
){

 Creates a top-level window view that can be instantiated with
 @rhombus(render). The @rhombus(child) views supply the window content.

 The @rhombus(~size) argument supplies the window's initial or current
 size, while @rhombus(~min_size), the window content, and
 @rhombus(~stretch) constrain the size that a user can choose when
 resizing the window. The @rhombus(~position) argument similar provides
 the window's initial or current position, but a user can move the
 window---depending on window decorations controlled in part by the
 @rhombus(~style) argument, such as whether a title bar is present. See
 @secref("geometry") for information about @rhombus(~align),
 @rhombus(~min_size), and @rhombus(~stretch).

 A menu bar supplied as the @rhombus(~menu_bar) argument is active when
 the window is active. Depending on the platform, the menu bar is either
 within the window or at the top of the screen when the window is
 frontmost.

 When a window is disabled via @rhombus(~enable), all content within the
 window is also disabled. Whether the menu bar is disabled along with a
 window depends on whether it is in the window or at the top of the
 screen.

 @window_callbacks

}

@doc(
  enum gui.Window.Style
  | no_resize_border
  | no_title
  | no_system_menu
  | hide_menu_bar
  | float
  | fullscreen_button
  | fullscreen_aux
){

  A window style option:

@itemlist(

  @item{@rhombus(#'no_resize_border): Disables window resizing by a user.}

  @item{@rhombus(#'no_title): Omits a title bar for the window.}

  @item{@rhombus(#'no_system_menu): Omits the system menu (Windows).}

  @item{@rhombus(#'hide_menu_bar): Hides the menu bar and dock when the
  window is active (Mac OS) or asks the window manager to make the window
  fullscreen (Unix).}

  @item{@rhombus(#'float): Causes the window to stay in front of all
  other non-floating windows. On Mac OS, a floating window shares the
  focus with an active non-floating window. When this style is combined
  with @rhombus(#'no_caption), then showing the frame does not cause the
  keyboard focus to shift to the window, and on Unix, clicking the frame
  does not move the focus. on Windows, a floating window has no taskbar
  button.}

  @item{@rhombus(#'fullscreen_button): Includes a button on the window’s
  title bar to put the window in fullscreen mode (Mac OS 10.7 and later).}

  @item{@rhombus(#'fullscreen_aux): Allows the window to accompany
  another that is in fullscreen mode (Mac OS 10.7 and later).}

)

}

@// ------------------------------------------------------------
@section(~tag: "dialog", ~style: [#'hidden]){@rhombus(Dialog)}

@screenshot(gallery_file("dialog"))

@doc(
  ~page
  class gui.Dialog():
    implements WindowView
    constructor (
      ~title: title :: ObsOrValue.of(String) = "Untitled",
      ~style: style :: ObsOrValue.of(List.of(Dialog.Style)) = [],
      ~size: size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~align: align :: ObsOrValue.of(View.Alignment) = [#'center, #'top],
      ~position: position :: ObsOrValue.of(View.Position) = #'center,
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false,
      child :: ObsOrValue.of(WindowChildView && !WindowView), ...
    )
){

 Like to @rhombus(Window), but creates a modal dialog, instead, meaning
 that no other window within the application can be made active as long
 as the dialog is shown.

 @window_callbacks

}

@doc(
  enum gui.Dialog.Style
  | no_title
  | no_sheet
  | resize_border
  | close_button
){

  A dialog style option:

@itemlist(

  @item{@rhombus(#'no_title): Omits a title bar for the dialog.}

  @item{@rhombus(#'no_sheet): Uses a movable dialog, even if a parent
  window is provided (Mac OS).}

  @item{@rhombus(#'resize_border): Enables dialog resizing by a user.}

  @item{@rhombus(#'close_button): Includes a close button in the
  dialog's title bar.}
)

}
