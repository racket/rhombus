#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def unexported: @rhombus(hidden, ~var))

@title(~tag: "window-view"){Window View Interface}

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

 A @deftech{window view} creates a window when the view is rendered.
 It is an instance of @rhombus(WindowChildView, ~annot) because it
 supports the methods associated with that interface, although a
 @rhombus(WindowView, ~class) is not rendered with it is incorporated
 directly in another @rhombus(WindowView, ~class).

 Create a @rhombus(WindowView, ~class) using @rhombus(Window, ~class)
 or @rhombus(Dialog, ~class).

}

@doc(
  method (wv :: gui.WindowView).render(parent :: maybe(Renderer) = #false)
    :: Renderer
){

 Renders the view in the same way as @rhombus(gui.render).

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
