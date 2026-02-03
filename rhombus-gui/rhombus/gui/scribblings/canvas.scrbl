#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def draw_DC = @rhombus(draw.DC, ~annot))

A @rhombus(Canvas, ~annot) provides a place for arbitrary drawing using
the @rhombusmodname(draw) library. An @rhombus(EditorCanvas, ~annot) is
backed by the same drawing library, but it cooperates with additional
text-editing libraries.

@title(~tag: "all-canvas", ~style: #'toc){Canvases}

@local_table_of_contents()

@// ------------------------------------------------------------
@section(~tag: "canvas", ~style: [#'hidden]){@rhombus(Canvas)}

@centered(@(gallery("canvas")))

@doc(
  ~page
  class gui.Canvas():
    implements WindowChildView
    constructor (
      data :: ObsOrValue.of(Any),
      draw :: #,(draw_DC) Any -> ~any,
      ~mouse: mouse :: (KeyEvent, CanvasContext) -> ~any = Function.pass,
      ~key: key :: (MouseEvent, CanvasContext) -> ~any = Function.pass,
      ~label: label :: ObsOrValue.of(maybe(View.LabelString)) = "canvas",
      ~style: style :: ObsOrValue.of(List.of(Canvas.Style)) = [],
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false
    )
){

 Creates a canvas view. When the view is rendered, the @rhombus(draw)
 function is called as

@rhombusblock(
  draw(#,(@rhombus(dc, ~var)) :: #,(draw_DC), #,(@rhombus(data_val, ~var)))
)

 to draw the canvas's content to a backing store @rhombus(dc ~var),
 where @rhombus(data_val, ~var) is the value of @rhombus(Obs.peek(data))
 when @rhombus(data) is an @tech{observable}. The @rhombus(draw) function
 is called to update the canvas content when @rhombus(data) is an
 @tech{observable} and its value changes.

 When a mouse or key event is received by a rendered canvas, the
 @rhombus(mouse) or @rhombus(key) function is called with a
 @rhombus(MouseEvent, ~class) or @rhombus(KeyEvent, ~class),
 respectively, and a @rhombus(CanvasContext, ~class).

 A canvas's @rhombus(~label) string is not shown, but it is included for
 consistency with other @rhombus(WindowchildView)s.

 See @secref("geometry") for information about @rhombus(~margin),
 @rhombus(~min_size), and @rhombus(~stretch).

 @window_callbacks

}

@doc(
  interface gui.CanvasContext
  property (ctx :: gui.CanvasContext).client_size :: Size
){

 A @rhombus(CanvasContext, ~class) represents a canvas instance that
 receives mouse or keyboard events so that properties of the instance can
 be accessed, including its size.

}

@doc(
  enum gui.Canvas.Style
  | border
  | control_border
  | choice
  | vscroll
  | hscroll
  | resize_corner
  | gl
  | no_autoclear
  | transparent
  | no_focus
){

 A canvas style symbol:

@itemlist(

  @item{@rhombus(#'border): Gives the canvas a thin border.}

  @item{@rhombus(#'control_border): Gives the canvas a thick border like
  @rhombus(EditorCanvas).}

  @item{@rhombus(#'choice): Gives the canvas a popup button that is like
  an @rhombus(Input) with a @rhombus(~choices) list. This style is
  intended for use with @rhombus(#'control_border) and not with
  @rhombus(#'hscroll) or @rhombus(#'vscroll).}

  @item{@rhombus(#'hscroll): Enables horizontal scrolling (initially
  visible, but inactive).}

  @item{@rhombus(#'vscroll): Enables vertical scrolling (initially
  visible, but inactive).}

  @item{@rhombus(#'resize_corner): Leaves room for a resize control at
  the canvas’s bottom right when only one scrollbar is visible.}

  @item{@rhombus(#'gl): Creates a canvas for OpenGL drawing instead of
  (or in addition to) normal @rhombus(draw.DC, ~class) drawing. This style
  is usually combined with @rhombus(#'no_autoclear).}

  @item{@rhombus(#'no_autoclear): Prevents automatic erasing of the
  canvas by the windowing system.}

  @item{@rhombus(#'transparent): The canvas is ``erased'' by the
  windowing system by letting its parent show through.}

  @item{@rhombus(#'no_focus): Prevents the canvas from accepting the
  keyboard focus when the canvas is clicked or when the
  @rhombus(WindowChildView.focus) method is called.}

)

}

@// ------------------------------------------------------------
@section(~tag: "editor-canvas", ~style: [#'hidden]){@rhombus(EditorCanvas)}

@centered(@(gallery("editor-canvas")))

@doc(
  ~page
  class gui.EditorCanvas():
    implements WindowChildView
    constructor (
      editor :: ObsOrValue.of(maybe(EditorCanvasChild)),
      ~scrolls_per_page: scrolls_per_page :: Int.in(1..=10000) = 100,
      ~wheel_step: wheel_step :: ObsOrValue.of(maybe(Int.in(1..=10000))) = #false,
      ~line_count: line_count ::  ObsOrValue.of(maybe(Int.in(1..=1000))) = #false,
      ~inset: inset :: ObsOrValue.of(View.Margin) = [5, 5],
      ~label: label :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      ~style: style :: ObsOrValue.of(List.of(EditorCanvas.Style)) = [],
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~window_callbacks: wcb :: WindowCallbacks = WindowCallbacks()
    )

  interface gui.EditorCanvasChild:
    implementable #,(@rhombus(unexported, ~var))    
){

 Creates a canvas view to hold an editor. An editor is implemented as an
 instance of @rhombus(EditorCanvasChild, ~class) by libraries that
 cooperate with the @rhombusmodname(gui) library.

 The @rhombus(~scrolls_per_page) and @rhombus(~wheel_step) arguments
 control how the editor responds to mouse- or trackpad-based scrolling.

 The @rhombus(~line_count) argument sets the canvas’s graphical minimum
 height to display a particular number of lines of text. The line height
 is determined by measuring the difference between the top and bottom of
 a displayed editor's first line. The minimum height is not changed until
 the canvas gets an editor, and when the canvas's editor is changed, the
 minimum height is recalculated. If the line count is set to
 @rhombus(#false), then the canvas’s graphical minimum height is restored
 to its original value.

 The @rhombus(~inset) argument control how much an editor is inset
 within the editor canvas.

 An editor canvas's @rhombus(~label) string is not shown, but it is
 included for consistency with other @rhombus(WindowchildView)s.

 See @secref("geometry") for information about @rhombus(~margin),
 @rhombus(~min_size), and @rhombus(~stretch).

 @window_callbacks
}

@doc(
  enum gui.EditorCanvas.Style
  | no_boder
  | choice
  | no_hscroll
  | auto_hscroll
  | hide_hscroll
  | no_vscroll
  | auto_vscroll
  | hide_vscroll
  | resize_corner
  | transparent
  | no_focus
){

 An editor canvas style symbol:

@itemlist(

  @item{@rhombus(#'no_border): Omits a border around the editor canvas.}

  @item{@rhombus(#'choice): Gives the editor canvas a popup button that
  is like an @rhombus(Input) with a @rhombus(~choices) list.}

  @item{@rhombus(#'no_hscroll): Disables horizontal scrolling and hides
  the horizontal scrollbar.}

  @item{@rhombus(#'auto_hscroll): Automatically hides the horizontal
  scrollbar when unneeded (unless @rhombus(#'no_hscroll) or
  @rhombus(#'hide_hscroll) is also specified).}

  @item{@rhombus(#'hide_hscroll): Allows horizontal scrolling, but hides
  the horizontal scrollbar.}

  @item{@rhombus(#'no_vscroll): Disables vertical scrolling and hides
  the vertical scrollbar.}

  @item{@rhombus(#'auto_vscroll): Automatically hides the vertical
  scrollbar when unneeded (unless @rhombus(#'no_vscroll) or
  @rhombus(#'hide_vscroll) is also specified).}

  @item{@rhombus(#'hide_vscroll): Allows vertical scrolling, but hides
  the vertical scrollbar.}

  @item{@rhombus(#'resize_corner): Leaves room for a resize control at
  the editor canvas’s bottom right when only one scrollbar is visible.}

  @item{@rhombus(#'transparent): The editor canvas is ``erased'' by the
  windowing system by letting its parent show through.}

  @item{@rhombus(#'no_focus): Prevents the editor canvas from accepting
  the keyboard focus when the canvas is clicked or when the
  @rhombus(WindowChildView.focus) method is called.}

)

}
