#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def unexported: @rhombus(hidden, ~var))

@title(~tag: "window-view"){Window View Interfaces}

@doc(
  interface gui.WindowChildView:
    extends View
    implementable #,(@unexported)
){

 A @deftech{window-child view} represents a view that can be included
 within a window.

 Create a @rhombus(WindowView, ~class) using classes like
 @rhombus(Button, ~class), @rhombus(Canvas, ~class), and
 @rhombus(VPanel, ~class),

}

@doc(
  interface gui.WindowView:
    extends WindowChildView
    implementable #,(@unexported)
){

 A @deftech{window view} creates a window when the view is rendered. A
 window view is an instance of @rhombus(WindowChildView, ~annot) because
 it supports the methods associated with that interface, but a window
 view cannot be provided as a child of a container view like
 @rhombus(HPanel, ~class) or @rhombus(TabsPanel, ~class).

 Create a @rhombus(WindowView, ~class) using @rhombus(Window, ~class)
 or @rhombus(Dialog, ~class).

}

@doc(
  method (wv :: gui.WindowView).render(parent :: maybe(Renderer) = #false)
    :: Renderer
){

 Renders the view by creating and showing a window or dialog. The
 resulting window or dialog might be closed by the user; use
 @rhombus(WindowView.close) to close it programmatically.

 The result rendered should be destroyed using
 @rhombus(Renderer.destroy). Otherwise, the underlying GUI objects may be
 retained, especially via registrations with @tech{observables}.

}

@doc(
  method (wv :: gui.WindowView).run(parent :: maybe(Renderer) = #false)
    :: Void
){

 Renders the view, waits until the rendered window or dialog is
 closed, and then destroys the renderer.

}


@doc(
  method (wv :: gui.WindowView).show(on :: Any.to_boolean) :: Void
){

 Shows or hides the @tech{most recent rendering} of @rhombus(wv).

 If a window is being shown via @rhombus(wv.run()), then hiding the
 window with @rhombus(wv.show(#false)) will not cause @rhombus(wv.run())
 to complete. Use @rhombus(wv.close()) to close the window.

}

@doc(
  method (wv :: gui.WindowView).close() :: Void
){

 Hides the @tech{most recent rendering} of @rhombus(wv) and notifies the
 renderer, which can allow  @rhombus(wv.run()) to complete for a window.

}

@doc(
  method (v :: gui.WindowChildView).focus() :: Void
){

 Moves keyboard focus to the @tech{most recent rendering} of
 @rhombus(v).

}

@doc(
  method (v :: gui.WindowChildView).client_to_screen(
    x :: View.PositionInt,
    y :: View.PositionInt
  ) :: values(View.PositionInt, View.PositionInt)
  method (v :: gui.WindowChildView).screen_to_client(
    x :: View.PositionInt,
    y :: View.PositionInt
  ) :: values(View.PositionInt, View.PositionInt)
){

 Maps a position within the @tech{most recent rendering} of
 @rhombus(v) to a position in screen coordinates, or vice versa.

}

@doc(
  method (v :: gui.WindowChildView).popup(
    menu :: PopupMenu,
    x :: View.PositionInt,
    y :: View.PositionInt
  ) :: Void
){

 Renders @rhombus(menu) as a poupup menu at the position specified by
 @rhombus(x) and @rhombus(y) within @rhombus(v).

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
