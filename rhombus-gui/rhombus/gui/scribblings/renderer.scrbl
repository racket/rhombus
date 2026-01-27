#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Renderers}

A @tech{view} describes a reactive GUI layout, and @deftech{render}ing
the view turns it into a live GUI window, control, or menu item. A view
can be rendered multiple times, although rendering a view just once is
common.

@doc(
  class gui.Renderer():
    expression ~none
){

 Represents one @tech{render}ing of a @tech{view}. A @rhombus(Renderer)
 object is useful for destroying the rendered form or for rendering a new
 window or dialog as a child of an existing window rendering.

}

@doc(
  fun gui.render(view :: WindowView,
                 parent :: maybe(Renderer) = #false)
    :: Renderer
){

 Equivalent to @rhombus(view.render(parent)).

}

@doc(
  property (rdr :: gui.Renderer).gui_handle :: Any
){

 Returns an object representing the window rendered by @rhombus(rdr), if
 any, as a Racket-level object for the
 @racketmodname(racket/gui, ~indirect) library.

 If @rhombus(rdr) is the result of rendering a @rhombus(MenuBar, ~class)
 directly, then the result is @rhombus(#false).

}

@doc(
  method (rdr :: gui.Renderer).destroy()
    :: Void
){

 Destroys any user-visible GUI element that remains of @rhombus(rdr) and
 removes any observers created by rendering.

}
