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
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false,
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
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false,
      child :: ObsOrValue.of(WindowChildView), ...
    )
){

 Similar to @rhombus(Window), but creates a modal dialog, instead.

}

@doc(
  ~include lib("rhombus/gui/private/window-callback.rhm"):
    gui.WindowCallbacks
){

 Configuration and callbacks that apply to all
 @rhombus(WindowChildView, ~class) constructors, but grouped as
 @rhombus(WindowCallbacks, ~class) because they are less commonly needed.

@itemlist(

 @item{@rhombus(accepts_drop_file): Determines whether the view accepts
  drag-and-drop files.}

 @item{@rhombus(drop_file): Called when the view receives a drag-and-drop
  file (when enabled).}

 @item{@rhombus(focus): Called when the view gains or loses the keyboard
  focus, where the callback argument indicates whether the focus was
  gained.}

 @item{@rhombus(move): Called when the view changes position relative to
  its parent. The callback arguments indicate the new horizontal and
  vertical positions. This callback is mostly useful for
  @rhombus(Window, ~class) or @rhombus(Dialog, ~class) views.}

 @item{@rhombus(size): Like @rhombus(move), but called when the view
  changes size. The callback arguments indicate the new horizontal and
  vertical size.}

 @item{@rhombus(sub_key): Call when a key event is to be delivered
  to the view or a view that it contains. If the result is a true value,
  then the event is considered handled and @emph{not} propagated further.
  The first argument is the view whose (rendered form) is to receive the
  event if it is propagated.}

 @item{@rhombus(sub_mouse): Call when a mouse event is to be
  delivered to the view or a view that it contains. If the result is a
  true value, then the event is considered handled and @emph{not}
  propagated further. The first argument is the view whose is to receive
  the event if it is propagated.}

 @item{@rhombus(sub_focus): Call when the view or a view that it
  contains gains or loses the keyboard focus. The first argument is the
  view whose changed focus, and the second argument is true if it gained
  the focus.}

 @item{@rhombus(super_activate): Call when the view's enclosing
  window has become the active window for keyboard focus or lost that
  status. The callback argument is true when the window became the active
  window.}

 @item{@rhombus(super_enable): Called when the enable state of the
  view or an enclosing view changes. The callback argument indicates the
  new enable state}

 @item{@rhombus(super_show): Called when the visibility of the view
  or an enclosing view changes. The callback argument is true when the
  view becomes shown.}

)

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
