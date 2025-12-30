#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    draw
    rhombus/meta open
    rhombus/runtime_path)

@(def draw_eval = make_rhombus_eval())
@(runtime_path.def water: "water.png")

@title(~tag: "overview"){Overview}

Drawing with @rhombusmodname(draw) requires a @tech{drawing context}
(DC), which is an instance of the @rhombus(DC, ~annot) interface.
For example, the @rhombus(PDFDC, ~class) class implements
@rhombus(DC, ~annot) for drawing to a PDF file, while
@rhombus(Bitmap.make_dc) produces @rhombus(DC, ~annot)
instance for drawing into a bitmap. When using the @rhombusmodname(gui)
library for GUIs, the drawing callback for a canvas returns a
@rhombus(DC, ~annot) instance for drawing into the canvas window.

@// ------------------------------------------------------------
@section{Lines and Simple Shapes}

To draw into a bitmap, first create the bitmap with
@rhombus(Bitmap), and then call its @rhombus(Bitmap.make_dc)
method to get a drawing context:

@examples(
  ~eval: draw_eval
  import draw
  def target = draw.Bitmap([30, 30]) // a 30x30 bitmap
  def dc = target.make_dc()
)

Use methods like @rhombus(DC.line) on the DC to draw into the
bitmap. For example, the sequence

@examples(
  ~eval: draw_eval
  :
    dc.rectangle([[0, 10],   // top-left at (0, 10), 10 down from top-left
                  [30, 10]]) // 30 pixels wide and 10 pixels high
  :
    dc.line([0, 0],          // start at (0, 0), the top-left corner
            [30, 30])        // and draw to (30, 30), the bottom-right corner
  :
    dc.line([0, 30],         // start at (0, 30), the bottom-left corner
            [30, 0])         // and draw to (30, 0), the top-right corner
)

draws an ``X'' on top of a smaller rectangle into the bitmap
@rhombus(target). If you save the bitmap to a file with
@rhombus(target.write("box.png", ~kind: #'png)), then @filepath{box.png}
contains the image

@centered(draw_eval('target.copy()'))

in PNG format. In DrRacket, simply printing the bitmap will show its
content, so further examples will rely on that.

A line-drawing drawing operation like @rhombus(DC.line) uses the
DC's current @defterm{pen} to draw the line, which can be accessed or
changed as the @rhombus(DC.pen) property. A pen has a color, line
width, and style, where pen styles include @rhombus(#'solid),
@rhombus(#'long_dash), and @rhombus(#'transparent). Enclosed-shape
operations like @rhombus(DC.rectangle) use both the current pen and
the DC's current @deftech{brush}, which is accessed or set as
@rhombus(DC.brush). A brush has a color and style, where brush
styles include @rhombus(#'solid), @rhombus(#'cross_hatch), and
@rhombus(#'transparent).

For example, set the brush and pen before the drawing operations to
draw a thick, red ``X'' on a green rectangle with a thin, blue border:

@examples(
  ~eval: draw_eval
  :
    dc.clear()  // erase previous content
  :
    dc.brush := draw.Brush(~color: "green") // #'solid is the default
  dc.pen := draw.Pen(~color: "blue")
  dc.rectangle([[0, 10], [30, 10]])
  dc.pen := draw.Pen(~color: "red", ~width: 3)
  dc.line([0, 0], [30, 30])
  dc.line([0, 30], [30, 0])
  ~fake:
    target
    target.copy()
)

To draw a filled shape without an outline, set the pen to
@rhombus(#'transparent) mode (with any color and line width) or,
equivalently, use @rhombus(Pen.none):

@examples(
  ~eval: draw_eval
  dc.pen := draw.Pen.none
  dc.brush := draw.Brush(~color: "gold")
  dc.ellipse([[5, 5], [20, 20]])
  target
)


@// ------------------------------------------------------------
@section{Saving and Restoring State}

A function that draws to a DC will usually need to change
@rhombus(DC.pen), @rhombus(DC.brush), or other properties of
the DC, and normally it should restore settings afterward. Use
@rhombus(DC.save) to push the current state to a internal stack,
and use @rhombus(DC.restore) to pop the stack and restore that
state---or use the @rhombus(DC.save_and_restore) form to wrap
@rhombus(DC.save) and @rhombus(DC.restore) around a body
sequence.

@examples(
  ~eval: draw_eval
  ~defn:
    def blue_brush = draw.Brush(~color: "blue")
    def yellow_brush = draw.Brush(~color: "yellow")
    def red_pen = draw.Pen(~color: "red", ~width: 2)
  ~defn:
    fun draw_face(dc :: draw.DC):
      dc.save_and_restore:
        dc.pen := draw.Pen.none
        dc.brush := blue_brush
        dc.ellipse([[25, 25], [100, 100]])

        dc.brush := yellow_brush
        dc.rectangle([[50, 50], [10, 10]])
        dc.rectangle([[90, 50], [10, 10]])

        dc.brush := draw.Brush.none
        dc.pen := red_pen
        dc.arc([[37, 27], [75, 75]], 5/4 * math.pi, 7/4 * math.pi)
  ~repl:
    def target = draw.Bitmap([150, 150])
    def dc = target.make_dc()
    draw_face(dc)
    ~fake:
      target
      target.copy()
)

Incidentally, a brush doesn't necessarily have just a solid color. If
@filepath{water.png} has the image

@centered{@draw.Bitmap.read("water.png")}

then it can be loaded into a bitmap and used as the @defterm{stipple}
for a brush:

@examples(
  ~eval: draw_eval
  ~defn:
    def water_bitmap = draw.Bitmap.read("water.png")
    def blue_brush = draw.Brush(~stipple: water_bitmap)
  ~repl:
    draw_face(dc)
    ~fake:
      target
      target.copy()
)


@// ------------------------------------------------------------
@section{Transformations}

Any coordinates or lengths supplied to drawing commands are transformed
by a DC's current transformation matrix. The transformation matrix can
scale an image, draw it at an offset, or rotate all drawing. The
transformation can be set directly, or the current transformation can be
transformed further with methods like @rhombus(DC.scale),
@rhombus(DC.translate), or @rhombus(DC.rotate):

@examples(
  ~eval: draw_eval
  dc.clear()
  dc.scale(0.5)
  dc.save_and_restore:
    draw_face(dc)
  dc.save_and_restore:
    dc.translate(150, 150)
    dc.rotate(1/2 * math.pi)
    draw_face(dc)
  dc.save_and_restore:
    dc.translate(300, 300)
    dc.rotate(math.pi)
    draw_face(dc)
  dc.save_and_restore:
    dc.translate(150, 150)
    dc.rotate(3/2 * math.pi)
    draw_face(dc)
  target
)

@// ------------------------------------------------------------
@section{Drawing Paths}

Drawing functions like @rhombus(DC.line) and
@rhombus(DC.rectangle) are actually convenience functions for the
more general @rhombus(DC.path) operation. The
@rhombus(DC.path) operation takes a @deftech{path}, which describes
a set of line segments and curves to draw with the pen and---in the case
of closed set of lines and curves---fill with the current brush.

An instance of @rhombus(Path) holds a path. Conceptually, a path
has a current pen position that is manipulated by methods like
@rhombus(Path.move_to), @rhombus(Path.line_to), and
@rhombus(Path.curve_to). The @rhombus(Path.move_to) method
starts a subpath, and @rhombus(Path.line_to) and
@rhombus(Path.curve_to) extend it. The @rhombus(Path.close)
method moves the pen from its current position in a straight line to its
starting position, completing the subpath and forming a closed path
that can be filled with the brush. A @rhombus(Path) object can have
multiple closed subpaths and one final open path, where the open path is
drawn only with the pen.

For example,

@examples(
  ~eval: draw_eval
  ~repl:
    def zee = draw.Path()
    zee.move_to([0, 0])
    zee.line_to([30, 0])
    zee.line_to([0, 30])
    zee.line_to([30, 30])
)

creates an open path. Drawing this path with a black pen of width 5
and a transparent brush produces

@examples(
  ~eval: draw_eval
  ~hidden:
    fun path_bitmap(zee, join, with_brush):
      let bm = draw.Bitmap([40, 40])
      let dc = bm.make_dc()
      dc.pen := draw.Pen(~width: 5, ~join: join)
      dc.brush := (if with_brush | blue_brush | draw.Brush.none)
      dc.path(zee, ~dx: 5, ~dy: 5)
      bm
)

@centered{@draw_eval('path_bitmap(zee, #'round, #false)')}

Drawing a single path with three line segments is not the same as
drawing three separate lines. When multiple line segments are drawn at
once, the corner from one line to the next is shaped according to the
pen's join style. The image above uses the default @rhombus(#'round)
join style. With @rhombus(#'miter), line lines are joined with sharp
corners:

@centered{@draw_eval('path_bitmap(zee, #'miter, #false)')}

If the subpath in @rhombus(zee) is closed with
@rhombus(Path.close), then all of the corners are joined, including
the corner at the initial point:

@examples(
  ~eval: draw_eval
  zee.close()
)

@centered{@draw_eval('path_bitmap(zee, #'miter, #false)')}

Using @rhombus(blue_brush) instead of a transparent brush causes the
interior of the path to be filled:

@centered{@draw_eval('path_bitmap(zee, #'miter, #true)')}

When a subpath is not closed, it is implicitly closed for brush
filling, but left open for pen drawing. When both a pen and brush are
available (i.e., not transparent), then the brush is used first, so
that the pen draws on top of the brush.

@// ------------------------------------------------------------
@section{Text}

Draw text using the @rhombus(DC.text) method, which takes a string
to draw and a location for the top-left of the drawn text:

@examples(
  ~eval: draw_eval
  ~repl:
    def text_target = draw.Bitmap([100, 30])
    def dc = text_target.make_dc()
    dc.brush := draw.Brush.none
  ~repl:
    dc.rectangle([[0, 0], [100, 30]])
    dc.text("Hello, World!", ~dx: 5, ~dy: 1)
  ~repl:
    ~fake:
      text_target
      text_target.copy()
)

The font used to draw text is determined by the DC's current font. A
font is described by a @rhombus(Font) object and installed as the
@rhombus(DC.font) property. The color of drawn text, which is
separate from either the pen or brush, can be set via the
@rhombus(DC.text_color) property.

@examples(
  ~eval: draw_eval
  ~repl:
    dc.clear()
    dc.font := draw.Font(~size: 14,
                         ~kind: #'roman,
                         ~weight: #'bold)
    dc.text_color := "blue"
    dc.rectangle([[0, 0], [100, 30]])
    dc.text("Hello, World!", ~dx: 5, ~dy: 1)
    ~fake:
      text_target
      text_target.copy()
)

To compute the size that will be used by drawn text, use
@rhombus(DC.text_extent), which returns four values: the total
width, total height, difference between the baseline and total height,
and extra space (if any) above the text in a line. For example, the
result of @rhombus(DC.text_extent) can be used to position text
within the center of a box:

@examples(
  ~eval: draw_eval
  ~repl:
    dc.clear()
    dc.rectangle([[0, 0], [100, 30]])
    def (w, h, d, a) = dc.text_extent("Hello, World!")
    dc.text("Hello, World!", ~dx: (100 - w) / 2, ~dy: (30 - h) / 2)
    ~fake:
      text_target
      text_target.copy()
)

@// ------------------------------------------------------------
@section{Alpha Channels and Compositing}

When you create or @rhombus(DC.clear) a bitmap, the content is nothing.
``Nothing'' isn't the same as white; it's the absence of drawing. For
example, if you take @rhombus(text_target) from the previous section and
copy it onto another DC using @rhombus(DC.bitmap), then the black
rectangle and blue text is transferred, and the background is left
alone, because the background was never filled and is still ``nothing'':

@examples(
  ~eval: draw_eval
  ~repl:
    def new_target = draw.Bitmap([100, 30])
    def dc = new_target.make_dc()
    dc.pen := draw.Pen.none
    dc.brush := draw.Brush(~color: "pink")
    dc.rectangle([[0, 0], [100, 30]])
    ~fake:
      new_target
      new_target.copy()
  ~repl:
    dc.bitmap(text_target)
    ~fake:
      new_target
      new_target.copy()
)

The information about which pixels of a bitmap are drawn (as opposed to
``nothing'') is the bitmap's @deftech{alpha channel}. Not all DCs keep
an alpha channel, but bitmaps keep an alpha channel by default. Bitmaps
loaded with @rhombus(Bitmap.from_file) preserve transparency in the
image file through the bitmap's alpha channel.

An alpha channel isn't all or nothing. When the edges text is
anti-aliased by @rhombus(DC.text), for example, the pixels are
partially transparent. When the pixels are transferred to another DC,
the partially transparent pixel is blended with the target pixel in a
process called @deftech{alpha blending}. Furthermore, a DC has an alpha
value that is applied to all drawing operations; an alpha value of
@rhombus(1.0) corresponds to solid drawing, an alpha value of
@rhombus(0.0) makes the drawing have no effect, and values in between
make the drawing translucent.

For example, setting the DC's alpha to @rhombus(0.25) before calling
@rhombus(DC.bitmap) causes the blue and black of the ``Hello,
World!'' bitmap to be quarter strength as it is blended with the
destination image:

@examples(
  ~eval: draw_eval
  ~repl:
    dc.clear()
    dc.rectangle([[0, 0], [100, 30]])
    dc.alpha := 0.25
    dc.bitmap(text_target)
    ~fake:
      new_target
      new_target.copy()
)

Setting a DC's opacity by itself does not always have the intended
effect. In particular, if the goal is to fade overlapping shapes, then
setting @rhombus(DC.alpha) affects individual steps and the way
they overlap, instead of affecting the overall drawing:

@examples(
  ~eval: draw_eval
  ~defn:
    fun draw_overlap(dc :: draw.DC):
      dc.save_and_restore:
        dc.pen := draw.Pen.none
        dc.brush := draw.Brush(~color: "blue")
        dc.rectangle([[0, 0], [40, 40]])
        dc.brush := draw.Brush(~color: "red")
        dc.rectangle([[20, 20], [40, 40]])
  ~repl:
    def target = draw.Bitmap([60, 60])
    def o_dc = target.make_dc()
    draw_overlap(o_dc)
    ~fake:
      target
      target.copy()
  ~repl:
    o_dc.clear()
    o_dc.save_and_restore:
      o_dc.alpha := 0.3
      draw_overlap(o_dc)
    ~fake:
      target
      target.copy()
)

Use @rhombus(DC.using_alpha) to apply alpha compositing to a
sequence of drawing commands, rendering them all at once with the given
opacity. The @rhombus(DC.using_alpha) form wraps
@rhombus(DC.start_alpha) and @rhombus(DC.end_alpha) around its
body.

@examples(
  ~eval: draw_eval
  ~repl:
    o_dc.clear()
    o_dc.using_alpha (0.3):
      draw_overlap(o_dc)
    ~fake:
      target
      target.copy()
)

@// ------------------------------------------------------------
@section{Clipping}

In addition to tempering the opacity of drawing operations, a DC has a
@deftech{clipping region} that constrains all drawing to inside the
region. In the simplest case, a clipping region corresponds to a closed
path, but it can also be the union, intersection, subtraction, or
exclusive-or of two paths.

For example, a clipping region could be set to three circles to clip the
drawing of a rectangle (with the @rhombus(0.25) alpha still in effect):

@examples(
  ~eval: draw_eval
  def r = draw.Region()
  def p:
    let p = draw.Path()
    p.ellipse([[ 0, 0], [35, 30]])
    p.ellipse([[35, 0], [30, 30]])
    p.ellipse([[65, 0], [35, 30]])
    r.path(p)
  dc.clipping_region := r
  dc.brush := draw.Brush(~color: "green")
  dc.rectangle([[0, 0], [100, 30]])
  ~fake:
    new_target
    new_target.copy()
)

The clipping region can be viewed as a convenient alternative to path
filling or drawing with stipples. Conversely, stippled drawing can be
viewed as a convenience alternative to clipping repeated calls of
@rhombus(DC.bitmap).

Combining regions with @rhombus(Brush) objects that have gradients,
however, is more than just a convenience, as it allows us to draw shapes
in combinations we could not otherwise draw. To illustrate, here is some
code that draws text with its reflection below it.

@examples(
  ~eval: draw_eval
  ~defn:
    def str = "Rhombus!"
    def font = draw.Font(~size: 24, ~kind: #'swiss, ~weight: #'bold)
  ~defn:
    :
      // First compute the size of the text we're going to draw,
      // using a small bitmap that we never draw into
      def (tw, th):
        def b_dc = draw.Bitmap([1, 1]).make_dc()
        b_dc.font := font
        let (tw, th, ta, td) = b_dc.text_extent(str)
        values(math.exact(math.ceiling(tw)),
               math.exact(math.ceiling(th)))
  ~defn:
    :
      // Now we can create a correctly sized bitmap to
      // actually draw into and enable smoothing
      def bm = draw.Bitmap([tw, 2*th])
    def b_dc = bm.make_dc()
  ~defn:
    :
      // Next, build a path that contains the outline of the text
      def upper_path = draw.Path()
    upper_path.text_outline(str, ~font: font)
  ~defn:
    :
      // Next, build a path that contains the mirror image
      // outline of the text
      def lower_path = draw.Path()
    lower_path.append(upper_path)
    lower_path.transform(draw.Transformation(1, 0, 0, -1, 0, 0))
    lower_path.translate(0, 2*th)
  ~defn:
    :
      // This helper accepts a path, sets the clipping region
      // of bdc to be the path (but in region form), and then
      // draws a big rectangle over the whole bitmap;
      // the brush will be set differently before each call to
      // draw-path, in order to draw the text and then to draw
      // the shadow
      fun draw_path(path):
        let r = draw.Region()
        r.path(path)
        b_dc.save_and_restore:
          b_dc.clipping_region := r
          b_dc.pen := draw.Pen.none
          b_dc.rectangle([[0, 0], [tw, 2*th]])
  ~defn:
    :
      // Now we just draw the upper-path with a solid brush
      b_dc.brush := draw.Brush(~color: "black")
    draw_path(upper_path)
  ~defn:
    :
      // To draw the shadow, we set up a brush that has a
      // linear gradient over the portion of the bitmap
      // where the shadow goes
      def stops:
        [[0, draw.Color(0, 0, 0, 0.4)],
         [1, draw.Color(0, 0, 0, 0.0)]]
    b_dc.brush := draw.Brush(~gradient:
                               draw.LinearGradient([0, th],
                                                   [0, 2*th],
                                                   stops))
    draw_path(lower_path)
  ~repl:
    ~fake:
      bm
      bm.copy()
)

@// ------------------------------------------------------------
@section(~tag: "portability"){Portability and Bitmap Variants}

Drawing effects are not completely portable across platforms, across
different classes that implement @rhombus(DC, ~class), or different
kinds of bitmaps. Fonts and text, especially, can vary across platforms
and types of DC, but so can the precise set of pixels touched by drawing
a line.

Different kinds of bitmaps can produce different results:

@itemlist(

 @item{Drawing to a bitmap produced by the @rhombus(Bitmap) constructor
 draws in the most consistent way across platforms.}

 @item{Drawing to a bitmap produced by
 @rhombus(Bitmap.make_platform) uses platform-specific drawing
 operations as much as possible. Depending on the platform, however, a
 bitmap produced by @rhombus(Bitmap.make_platform) may have no alpha
 channel, and it may use more constrained resources than one produced by
 the @rhombus(Bitmap) constructor (on Windows due to a system-wide,
 per-process GDI limit).

 As an example of platform-specific difference, text is smoothed by
 default with sub-pixel anti-aliasing on Mac OS, while text smoothing in
 the result of the @rhombus(Bitmap) constructor uses only grays. Line or
 curve drawing may touch different pixels than in a bitmap produced by
 the @rhombus(Bitmap) constructor, and bitmap scaling may differ.

 A possible approach to dealing with the GDI limit under Windows is to
 draw into the result of a @rhombus(Bitmap.make_platform) call and then copy
 the contents of the drawing into the result of the @rhombus(Bitmap) constructor.
 This approach preserves the drawing results of
 @rhombus(Bitmap.make_platform), but it retains constrained resources only
 during the drawing process.}

 @item{Drawing to a bitmap produced by @rhombus(make_screen_bitmap) from
 @rhombusmodname(gui) uses the same platform-specific drawing
 operations as drawing into a @rhombus(gui.Canvas) instance. A bitmap
 produced by @rhombus(make_screen_bitmap) uses the same platform-specific
 drawing as @rhombus(Bitmap.make_platform) on Windows or Mac OS, but
 possibly scaled, and it may be scaled or sensitive to the display on
 Unix.

 On Mac OS, when the main screen is in Retina mode (at the time that the
 bitmap is created), the bitmap is also internally scaled so that one
 drawing unit uses two pixels. Similarly, on Windows or Unix, when the
 main display's text scale is configured at the operating-system level
 (see @rhombus(get_display_resolution), the bitmap is internally scaled,
 where common configurations map a drawing unit to @math{1.25},
 @math{1.5}, or @math{2} pixels.

 Use @rhombus(make_screen_bitmap) when drawing to a bitmap as an
 offscreen buffer before transferring an image to the screen, or when
 consistency with screen drawing is needed for some other reason.}

 @item{A bitmap produced by @rhombus(gui.Canvas.make_bitmap) is like a
 bitmap from @rhombus(make_screen_bitmap), but on Mac OS, the bitmap may
 be optimized for drawing to the screen (by taking advantage of system
 APIs that can, in turn, take advantage of graphics hardware).

 Use @rhombus(gui.Canvas.make_bitmap) for similar purposes as
 @rhombus(make_screen_bitmap), particularly when the bitmap will be drawn
 later to a known target canvas.}

)

@// ----------------------------------------

@close_eval(draw_eval)
