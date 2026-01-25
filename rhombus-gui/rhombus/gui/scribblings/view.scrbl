#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def unexported: @rhombus(hidden, ~var))

@title{Views}

@doc(
  interface gui.View:
    implementable #,(@unexported)
){

 A @deftech{view} describes a GUI widget that is created when the view
 is rendered.

 Implementations of @rhombus(View, ~class) include
 @rhombus(Window, ~class), @rhombus(Button, ~class),
 @rhombus(Canvas, ~class), @rhombus(Menu, ~class), and
 @rhombus(MenuItem, ~class). Since the
 @rhombus(implementable, ~interface_clause) name of
 @rhombus(View, ~class) is kept private, all implementations or
 @rhombus(View, ~class) are part of the @rhombusmodname(gui) library.

}

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
  interface gui.MenuChildView:
    extends View
    implementable #,(@unexported)
){

 A @deftech{menu-child view} represents a view that can be added to a
 menu.

 Create a @rhombus(MenuChildView, ~class) using @rhombus(Menu, ~class),
 @rhombus(MenuItem, ~class), @rhombus(CheckableMenuItem, ~class), or
 @rhombus(MenuItemSeparator, ~class).

}

@doc(
  property (v :: gui.View).handle :: Any
){

 Returns a Racket object that corresponds to the view for use directly
 with @racketmodname(racket/gui/easy).

}

@doc(
  property (v :: gui.View).gui_handle :: Any
  method (v :: gui.View).get_gui_handle(
    ~who: who :: maybe(error.Who) = #false
  ) :: Any
){

 The @rhombus(View.gui_handle) property and
 @rhombus(View.get_gui_handle) method normally return the same value:
 a Racket-level object for the @racketmodname(racket/gui, ~indirect)
 library representing the @deftech{most recent rendering} of a view.

 If the view has not been rendered, yet, @rhombus(View.gui_handle)
 returns @rhombus(#false), while @rhombus(View.get_gui_handle) throws an
 exception. The @rhombus(who) argument to @rhombus(View.get_gui_handle)
 is included in the exception message, if any.

 A view might be rendered multiple times because it is nested multiple
 times within an enclosing view, or because @rhombus(render) is used
 multiple time on the view or an enclosing view. Only the most recent
 rendering is accessible, even if it has become hidden an an older
 rendering is still active.

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
