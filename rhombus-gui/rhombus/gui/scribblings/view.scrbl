#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def unexported: @rhombus(hidden, ~var))

@title(~tag: "view"){View Interface}

@doc(
  interface gui.View:
    implementable #,(@unexported)
){

 A @deftech{view} describes a GUI window, control, or menu that is
 created when the view is @tech{render}ed.

 Implementations of @rhombus(View, ~class) include
 @rhombus(Window, ~class), @rhombus(Button, ~class),
 @rhombus(Canvas, ~class), @rhombus(Menu, ~class), and
 @rhombus(MenuItem, ~class). Since the
 @rhombus(implementable, ~interface_clause) name of
 @rhombus(View, ~class) is kept private, all implementations or
 @rhombus(View, ~class) are part of the @rhombusmodname(gui) library.

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
  def View.normal_control_font :: draw.Font
  def View.small_control_font :: draw.Font
  def View.tiny_control_font :: draw.Font
  def View.view_control_font :: draw.Font
){

 Standard fonts for GUI controls.

}
