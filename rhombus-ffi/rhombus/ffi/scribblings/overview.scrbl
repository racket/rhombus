#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      ffi/finalize
      draw
      rhombus/memory)

@(def ffi_eval = make_rhombus_eval())
@examples(
  ~eval: ffi_eval
  ~hidden:
    import ffi open
    use_static
)

@title(~tag: "overview", ~style: #'toc){Overview}

The @rhombusmodname(ffi) library supports directly calling C functions
from Rhombus. To set up those calls, the C function must be described on
the Rhombus side, including the types for its arguments and results that
imply conversions between Rhombus and C values. In principle,
descriptions of C functions could be extracted from header files, but
most C bindings for Rhombus are written independent of header files. This
section steps you through the process with an extended example.

@margin_note_block{The running tutorial in this section is based on the original
 @hyperlink("https://prl.khoury.northeastern.edu/blog/2016/06/27/tutorial-using-racket-s-ffi/"){FFI
  tutorial series} written by Asumu Takikawa.}

@local_table_of_contents()

@// --------------------------------------------------
@section(~tag: "unsafe"){Unsafety of Foreign Functions}

Although using the FFI requires writing no new C code, it provides
relatively little insulation against the issues that C programmers
face related to safety and memory management. An FFI programmer must
be particularly aware of memory management issues for data that spans
the Rhombus--C divide. Although the library name @rhombusmodname(ffi)
and its exported bindings do not include the word
``unsafe,'' importing the library should be considered as a
declaration that your code is itself unsafe, therefore can lead to
serious problems in case of bugs. It is the responsibility of each
library that is implemented with @rhombusmodname(ffi) to provide a
safe interface to its clients.

The running example in this section is intended to work within DrRacket,
which can display image results. Beware, however, that DrRacket runs
programs in the same Racket/Rhombus process that runs DrRacket itself. A misuse
of unsafe functionality can therefore crash not only the buggy program,
but also DrRacket. Take appropriate care while editing, and consider
falling back to command-line Racket to view error messages that might
appear just before a crash.

Using the @rhombuslangname(rhombus/static) language (or the
@rhombus(use_static) declaration) is an especially good idea when
working with foreign functions. The @rhombusmodname(ffi) library
particularly relies on static information for pointer operations, and
static mode can help avoid confusing or incorrect behavior.

@// --------------------------------------------------
@section(~tag: "tutorial1"){Foreign-Function Basics}

To use the FFI, you must have in mind

@itemlist(

 @item{@italic{A particular shared library with functions you want to
   call}: A shared library corresponds to a file with a suffix such as
  @filepath{.dll}, @filepath{.so}, or @filepath{.dylib} (depending on the
  platform), or it might be a library within a @filepath{.framework}
  directory on Mac OS.}

 @item{@italic{A particular set of names exported by the library}:
  Shared libraries can export names for any kind of C value, but they
  mostly export functions. The exported names generally match the names as
  used from C.}

 @item{@italic{The C-level type of the exported name}: This is typically
  a function type that involves a sequence of argument types and a result
  type.}

)

As a running example, let's try calling functions exported by the Cairo
graphics library. Specifically, we'll start out by imitating the ``multi
segment caps'' C sample code on Cairo's
@hyperlink("https://www.cairographics.org/samples/"){samples page}:

@verbatim(~indent: 2){
cairo_move_to (cr, 50.0, 75.0);
cairo_line_to (cr, 200.0, 75.0);

cairo_move_to (cr, 50.0, 125.0);
cairo_line_to (cr, 200.0, 125.0);

cairo_move_to (cr, 50.0, 175.0);
cairo_line_to (cr, 200.0, 175.0);

cairo_set_line_width (cr, 30.0);
cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
cairo_stroke (cr);
}

The @tt{cr} variable here is a Cairo drawing context, which renders
commands to a drawing surface. As it happens, Rhombus bitmaps are
implemented using Cairo, and we can get a drawing surface that targets a
bitmap using the @rhombus(draw.Bitmap.handle) property to get a Racket object,
and then call that object's @rhombus(#{get-handle}) method. The method returns
a Cairo surface pointer representation that is compatible with
@racketmodname(ffi/unsafe, ~indirect)), so we must convert for
@rhombusmodname(ffi) it using @rhombus(cpointer_to_ptr).

@examples(
  ~eval: ffi_eval
  ~defn:
    import:
      draw
      rhombus/rkt_obj
  ~defn:
    def bt = draw.Bitmap([256, 256])
    def bt_surface = cpointer_to_ptr(rkt_obj.send bt.handle.#{get-handle}())
)

@// --------------------------------------------------

@subsection{Loading C Libraries}

The @rhombus(Lib.load) function gets a handle to a library by searching
the filesystem. The search is based on the library file's base name
(without a platform-specific suffix for shared libraries) and a list of
versions to try (where @rhombus(#false) tries omitting the version):

@examples(
  ~eval: ffi_eval
  ~defn:
    import:
      ffi open
  ~defn:
    def cairo_lib = Lib.load("libcairo", ["2", #false])
)

Knowing the library's name and/or path is often the trickiest part of
using the FFI. Sometimes, when using a library name without a path
prefix or file suffix, the library file can be located automatically,
especially on Unix.

In the case of Cairo, the problem is simplified by the fact that the
library is included with a Racket distribution for Windows or Mac OS.
Using the base file name @filepath{libcairo} with version @filepath{2}
is likely to find the library as bundled with Racket or as supplied by
the operating system---usually something like
@filepath{/usr/lib/libcairo.2.so} on a Unix installation.

On Windows, if you are not running in DrRacket, then
@filepath{libcairo-2.dll} will be found, but loading it may fail
because its dependencies cannot be found; the DLLs bundled with Racket
are not in the operating system's search path. The easy solution is to
import @rhombus(lib("racket/draw/unsafe/cairo-lib.rkt")), which will
load all dependencies.

@// --------------------------------------------------

@subsection{Finding C Functions}

Assuming that @rhombus(cairo_lib) is successfully defined for the loaded
Cairo shared library, we can use it to extract the addresses of functions
from the library. Let's start with @tt{cairo_create}, which accepts a
surface (like the one we have from a Rhombus bitmap) to create a drawing
context.

@verbatim(~indent: 2){
cairo_t * cairo_create (cairo_surface_t *target);
}

We can get a pointer to this function using @rhombus(Lib.find):

@examples(
  ~eval: ffi_eval
  cairo_lib!!.find("cairo_create")
)

To call this function, we will need to give that pointer a type, casting
via @rhombus(cast) to convert it to a Rhombus function. We could give
the function the simplest possible type, which is a function type using
@rhombus_t(->) that expects a generic pointer argument and returns a
generic pointer result:

@examples(
  ~eval: ffi_eval
  ~defn:
    def cairo_create:
      cast (ptr_t -> ptr_t) cairo_lib!!.find("cairo_create")
)

At this point, calling @rhombus(cairo_create) on @rhombus(bt_surface)
would work:

@examples(
  ~eval: ffi_eval
  cairo_create(bt_surface)
)

@// --------------------------------------------------

@subsection{Increasing Safety with Tagged Pointers}

Using a generic pointer type is especially dangerous, because there will
be many different pointer types that are used with Cairo functions. It's
better to give each pointer type a specific name. Named pointer types
not only improve readability, they enable checks that help prevent using
the wrong kind of pointer as a function argument.

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.type cairo_t
    foreign.type cairo_surface_t
)

The type @rhombus_t(cairo_surface_t) is defined here as an opaque type,
meaning that its C representation is unspecified, but the derived type
@rhombus_t(cairo_surface_t*) is a pointer type tagged with
@litchar{cairo_surface_t*}. A value with type @rhombus_t(cairo_surface_t*)
is accepted wherever @rhombus_t(ptr_t) is expected, but a @rhombus_t(ptr_t)
value is not accepted where @rhombus_t(cairo_surface_t*) is expected. A
@rhombus(foreign.type) form for @rhombus_t(cairo_surface_t) also defines
@rhombus(cairo_surface_t, ~annot) as an annotation to recognize
Rhombus pointer representations that are specifically tagged as
@rhombus_t(cairo_surface_t*) pointers. A function that expects a
@rhombus_t(cairo_surface_t*) pointer uses that annotation to check arguments.

@examples(
  ~eval: ffi_eval
  ~defn:
    def cairo_create:
      cast (cairo_surface_t* -> cairo_t*) cairo_lib!!.find("cairo_create")
)

In this case, calling @rhombus(cairo_create) with @rhombus(bt_surface)
would fail, because @rhombus(bt_surface) has type @rhombus_t(ptr_t), not
@rhombus_t(cairo_surface_t*). We are sure that @rhombus(bt_surface) really
is a surface pointer, so we can cast it using @rhombus(cast):

@examples(
  ~eval: ffi_eval
  bt_surface is_a cairo_surface_t
  cast (cairo_surface_t*) bt_surface
  cast (cairo_surface_t*) bt_surface is_a cairo_surface_t
  cairo_create(cast (cairo_surface_t*) bt_surface)
)

@// --------------------------------------------------

@subsection{Reducing Boilerplate for Function Definitions}

Instead of finding a function pointer and then separately casting it to
the right function type, combine those two steps using
@rhombus(foreign.fun):

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.fun cairo_create(cairo_surface_t*) :: cairo_t*:
      ~lib: cairo_lib
)

In this definition, @rhombus(cairo_create) is used both as the name to
define and the name to locate in the C library. As we will see in later
examples, an optional @rhombus(~c_id) clause for @rhombus(foreign.fun)
can separately specify the name to find in the C library, if needed.

Since we will define many functions from @rhombus(cairo_lib), it's even
better to use @rhombus(foreign.linker) to define a new form that's like
@rhombus(foreign.fun), except that it has the @rhombus(~lib) part built in:

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.linker cairo:
      ~lib: cairo_lib
  ~defn:
    cairo.fun cairo_create(cairo_surface_t*) :: cairo_t*
    cairo.fun cairo_move_to(cairo_t*, double_t, double_t) :: void_t
    cairo.fun cairo_line_to(cairo_t*, double_t, double_t) :: void_t
    cairo.fun cairo_set_line_width(cairo_t*, double_t) :: void_t
    cairo.fun cairo_stroke(cairo_t*) :: void_t
)

You may notice that the C type names @tt{double} and @tt{void} are
represented here by names that end in @litchar{_t}: @rhombus_t(double_t)
and @rhombus_t(void_t). The @rhombusmodname(ffi) convention is to use a
@litchar{_t} suffix for all type and type-constructor names, even for
traditional C types like @tt{int}, @tt{double}, and @tt{void}.

At this point, we have enough functions to draw a line on the bitmap.

@examples(
  ~eval: ffi_eval
  ~defn:
    import:
      pict
  ~defn:
    fun show(bt):
      pict.rectangle(~around: pict.bitmap(bt), ~line_width: 2)
)

@examples(
  ~eval: ffi_eval
  ~repl:
    def cr = cairo_create(cast (cairo_surface_t*) bt_surface)
    cairo_move_to(cr, 50.0, 50.0)
    cairo_line_to(cr, 206.0, 206.0)
    cairo_set_line_width(cr, 5.0)
    cairo_stroke(cr)
    show(bt)
)

@// --------------------------------------------------

@subsection{Defining a Type Conversion}

To match the original Cairo example, we'll need to call
@tt{cairo_set_line_cap}. The @tt{cairo_set_line_cap} function takes a
@tt{cairo_line_cap_t} argument, where @tt{cairo_line_cap_t} is defined
in C using @tt{enum}. The most natural translation to Rhombus is to use
a symbol for each variant, instead of integer constants. We can define a
new type @rhombus_t(cairo_line_cap_t) using @rhombus(foreign.type):

@examples(
  ~eval: ffi_eval
  ~defn:
    def line_cap_symbols = [#'butt, #'round, #'square]
  ~defn:
    foreign.type cairo_line_cap_t:
      ~extends int_t
      ~predicate: fun (v): v in line_cap_symbols
      ~rhombus_to_c: fun (sym): line_cap_symbols.index(sym)
      ~c_to_rhombus: fun (i): line_cap_symbols[i]
  ~defn:
    cairo.fun cairo_set_line_cap(cairo_t*, cairo_line_cap_t) :: void_t
    cairo.fun cairo_get_line_cap(cairo_t*) :: cairo_line_cap_t
  ~repl:
    cairo_set_line_cap(cr, #'round)
    cairo_get_line_cap(cr)
)

Mapping between symbols and integers is common enough that
@rhombusmodname(ffi) provides @rhombus(foreign.enum), which achieves the
same result as using @rhombus(foreign.type) with explicit @rhombus(~predicate),
@rhombus(~rhombus_to_c), and @rhombus(~c_to_rhombus) options.

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.enum cairo_line_cap_t int_t
    | butt
    | round
    | square
)

@// --------------------------------------------------

@subsection{Putting it All Together}

We can now implement the original Cairo example. Here's the complete
code and the result that DrRacket shows:

@examples(
  ~eval: ffi_eval
  ~defn:
    ~fake:
      #,(@hash_lang()) #,(@rhombuslangname(rhombus/static))
      #void
  ~defn:
    import:
      ffi open
      draw
      pict
      rhombus/rkt_obj
  ~defn:
    def cairo_lib = Lib.load("libcairo", ["2", #false])
    foreign.linker cairo:
      ~lib: cairo_lib
  ~defn:
    foreign.type cairo_t
    foreign.type cairo_surface_t
    foreign.enum cairo_line_cap_t int_t
    | butt
    | round
    | square
  ~defn:
    cairo.fun cairo_create(cairo_surface_t*) :: cairo_t*
    cairo.fun cairo_move_to(cairo_t*, double_t, double_t) :: void_t
    cairo.fun cairo_line_to(cairo_t*, double_t, double_t) :: void_t
    cairo.fun cairo_set_line_width(cairo_t*, double_t) :: void_t
    cairo.fun cairo_stroke(cairo_t*) :: void_t
    cairo.fun cairo_set_line_cap(cairo_t*, cairo_line_cap_t) :: void_t
  ~defn:
    fun make():
      def bt = draw.Bitmap([256, 256])
      def bt_surface = cpointer_to_ptr(rkt_obj.send bt.handle.#{get-handle}())
      values(bt, cairo_create(cast (cairo_surface_t*) bt_surface))
    fun show(bt):
      pict.rectangle(~around: pict.bitmap(bt), ~line_width: 2)
  ~defn:
    def (bt, cr) = make()
  ~defn:
    cairo_move_to(cr, 50.0, 75.0)
    cairo_line_to(cr, 200.0, 75.0)
  ~defn:
    cairo_move_to(cr, 50.0, 125.0)
    cairo_line_to(cr, 200.0, 125.0)
  ~defn:
    cairo_move_to(cr, 50.0, 175.0)
    cairo_line_to(cr, 200.0, 175.0)
  ~defn:
    cairo_set_line_width(cr, 30.0)
    cairo_set_line_cap(cr, #'round)
    cairo_stroke(cr)
  ~defn:
    show(bt)
)

@// --------------------------------------------------
@section(~tag: "tutorial2"){Composite Foreign Data}

Many C functions work with simple forms of data---such as integers,
floating-point numbers, and opaque pointers---but things get more
interesting with arrays, C structures, and callback functions.

@// --------------------------------------------------

@subsection{Array Arguments}

Let's look at the ``dash'' example from Cairo's
@hyperlink("https://www.cairographics.org/samples/"){samples page}:

@verbatim(~indent: 2){
double dashes[] = {50.0, 10.0, 10.0, 10.0};
int    ndash  = sizeof(dashes)/sizeof(dashes[0]);
double offset = -50.0;

cairo_set_dash (cr, dashes, ndash, offset);
cairo_set_line_width (cr, 10.0);

cairo_move_to (cr, 128.0, 25.6);
cairo_line_to (cr, 230.4, 230.4);
cairo_rel_line_to (cr, -102.4, 0.0);
cairo_curve_to (cr, 51.2, 230.4, 51.2, 128.0, 128.0, 128.0);

cairo_stroke (cr);
}

Two of the new functions are straightforward to define:

@examples(
  ~eval: ffi_eval
  ~defn:
    cairo.fun cairo_rel_line_to(cairo_t*, double_t, double_t) :: void_t
    cairo.fun cairo_curve_to(cairo_t*,
                             double_t, double_t,
                             double_t, double_t,
                             double_t, double_t) :: void_t
)

The most interesting function is @tt{cairo_set_dash}, which takes an
array argument. The C type signature for @tt{cairo_set_dash} is

@verbatim(~indent: 2){
void cairo_set_dash (cairo_t *cr,
                     const double *dashes,
                     int num_dashes,
                     double offset);
}

where the @tt{num_dashes} argument tells @tt{cairo_set_dash} the
length of the @tt{dashes} array.

With @rhombusmodname(ffi), we set up the array argument by allocating
memory using @rhombus(new) and then filling the memory with assignments.
The allocated memory is then passed to @tt{cairo_set_dash} as a pointer.
The following approach defines @rhombus(cairo_set_dash_raw) as a direct
reference to the C function that expects a pointer, and then it defines
a Rhombus-friendly wrapper that converts a list into an allocated array.

@examples(
  ~eval: ffi_eval
  ~defn:
    :
      // Low-level binding: takes a pointer and length
      cairo.fun cairo_set_dash_raw(cairo_t*, ptr_t, int_t, double_t) :: void_t:
        ~c_id: cairo_set_dash
  ~defn:
    :
      // Rhombus-friendly wrapper that takes a list
      fun cairo_set_dash(ctx, dashes :: List.of(Flonum), offset :: Flonum):
        def n = List.length(dashes)
        def arr = new ~manual double_t[n]
        for (i in 0 .. n):
          arr[i] := dashes[i]
        cairo_set_dash_raw(ctx, cast (ptr_t) arr, n, offset)
        free(arr)
  ~repl:
    cairo_set_dash(cr, [50.0, 10.0, 10.0, 10.0], -50.0)
)

This wrapper approach is fine, but the conversion from a Rhombus list to
a C array is also something that can be handled by a new converting
type. In fact, the @rhombusmodname(ffi) library provides a
@rhombus_t(list_t) type constructor to implement that conversion, where
@rhombus_t(list_t) takes the element type as an argument. At the same
time, we need to pass the length of the list/array to @tt{cairo_set_dash},
and that's a separate argument. To deal with that kind of interaction
among arguments, when you define a function type with @rhombus_t(->), you
can give names to arguments and refer to them in later expressions that
supply automatic arguments.

@examples(
  ~eval: ffi_eval
  ~defn:
    cairo.fun cairo_set_dash(
      cairo_t*,
      lst :: list_t(double_t),
      int_t = List.length(lst),
      double_t
    ) :: void_t
)

The @rhombus(lst ::) part of this definition gives a name to the value
supplied as the second argument to @rhombus(cairo_set_dash). The
@rhombus_t(list_t(double_t)) part insists that the argument is a list of
floating-point numbers, and it converts that list to an allocated array.
For the third argument, meanwhile, the @rhombus(= List.length(lst)) part
computes the argument automatically (i.e., it's not merely optional, but
never provided by a caller). The end result is that @rhombus(cairo_set_dash)
behaves the same as the wrapper @rhombus(cairo_set_dash) function defined
above.

@examples(
  ~eval: ffi_eval
  ~defn:
    def (bt, cr) = make()
  ~defn:
    def dashes = [50.0, 10.0, 10.0, 10.0]
    def offset = -50.0
  ~defn:
    cairo_set_dash(cr, dashes, offset)
    cairo_set_line_width(cr, 10.0)
  ~defn:
    cairo_move_to(cr, 128.0, 25.6)
    cairo_line_to(cr, 230.4, 230.4)
    cairo_rel_line_to(cr, -102.4, 0.0)
    cairo_curve_to(cr, 51.2, 230.4, 51.2, 128.0, 128.0, 128.0)
  ~defn:
    cairo_stroke(cr)
  ~repl:
    show(bt)
)

@// --------------------------------------------------

@subsection{C Structs}

For a more advanced example, let's measure text to scale it into our bitmap.
The relevant Cairo function is @tt{cairo_text_extents}:

@verbatim(~indent: 2){
void cairo_text_extents (cairo_t *cr,
                         const char *utf8,
                         cairo_text_extents_t *extents);
}

where @tt{cairo_text_extents_t} is a @tt{struct}:

@verbatim(~indent: 2){
typedef struct {
    double x_bearing;
    double y_bearing;
    double width;
    double height;
    double x_advance;
    double y_advance;
} cairo_text_extents_t;
}

We can define a @rhombus_t(cairo_text_extents_t) type for Rhombus using
@rhombus(foreign.struct):

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.struct cairo_text_extents_t(
      x_bearing :: double_t,
      y_bearing :: double_t,
      width :: double_t,
      height :: double_t,
      x_advance :: double_t,
      y_advance :: double_t
    )
)

This single declaration automatically creates several bindings:

@itemlist(

 @item{@rhombus_t(cairo_text_extents_t): works both as a type and as a
  type for use with @rhombus(new) to allocate a
  @rhombus_t(cairo_text_extents_t) instance.}

 @item{@rhombus_t(cairo_text_extents_t*): a type for a pointer to a
  @rhombus_t(cairo_text_extents_t) instance.}

 @item{@rhombus(cairo_text_extents_t, ~annot): an annotation for pointers to
  @rhombus_t(cairo_text_extents_t) instances.}

 @item{@rhombus(cairo_text_extents_t) as a veneer: field accessors like
  @rhombus(.width) and @rhombus(.x_bearing) that take a pointer to a
  @rhombus_t(cairo_text_extents_t) instance and extract a corresponding
  field value, converting it from C to Rhombus as needed.}

)

Using this definition, you can construct instances directly:

@examples(
  ~eval: ffi_eval
  ~defn:
    def extents = new cairo_text_extents_t(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
)

Or allocate one uninitialized:

@examples(
  ~eval: ffi_eval
  ~defn:
    def extents = new cairo_text_extents_t
)

We can define a ``raw'' version of @rhombus(cairo_text_extents) that will
write into a struct pointer that the caller provides:

@examples(
  ~eval: ffi_eval
  ~defn:
    cairo.fun cairo_text_extents_raw(
      cairo_t*, string_t, cairo_text_extents_t*
    ) :: void_t:
      ~c_id: cairo_text_extents
)

A @rhombus_t(string_t) type for the second argument lets a caller provide a
Rhombus string that is converted into a UTF-8-encoded, null-terminated
representation of the string for the C side.

Note that the third argument to @rhombus(cairo_text_extents_raw) is
@rhombus_t(cairo_text_extents_t*) (ending with @rhombus_t(*)), not
@rhombus_t(cairo_text_extents_t) (no @rhombus(*)). The C function accepts a
pointer argument and not an immediate struct argument. On the Rhombus
side, @rhombus_t(cairo_text_extents_t*) and @rhombus_t(cairo_text_extents_t)
have the same representation, but the distinction is crucial on the C
side, because functions accept and return struct values differently than
pointers.

To use @rhombus(cairo_text_extents_raw), a caller must allocate their own
@rhombus_t(cairo_text_extents_t) instance:

@examples(
  ~eval: ffi_eval
  ~repl:
    def extents = new cairo_text_extents_t
    cairo_text_extents_raw(cr, "hello world", extents)
    extents.width
)

Similar to the way we made @rhombus(cairo_set_dash) allocate an array,
we can improve @rhombus(cairo_text_extents) by having it automatically
allocate a @rhombus_t(cairo_text_extents_t) instance. A body after the
result type in a @rhombus(foreign.fun) definition provides a result
expression to substitute for the C function's result. An extra
@rhombus(:: foreign.type cairo_text_extents_t*) provides a Rhombus
result annotation, as opposed to the C function's result.

@margin_note{The result could also be written as
 @rhombus(foreign.type, ~annot) followed by @rhombus_t(cairo_text_extents_t*) instead of just
 @rhombus(cairo_text_extents_t). The Racket representation is the same
 for @rhombus_t(cairo_text_extents_t) and
 @rhombus_t(cairo_text_extents_t*), but using the @rhombus_t(*)
 operator for a type would mean that we must use
 @rhombus(foreign.type, ~annot) to bring the Rhombus annotation and C
 type contexts, while @rhombus(cairo_text_extents_t) is defined for both
 contexts.}

@examples(
  ~eval: ffi_eval
  ~defn:
    cairo.fun cairo_text_extents(
      cairo_t*,
      string_t,
      exts :: cairo_text_extents_t* = new cairo_text_extents_t
    ) :: void_t :: cairo_text_extents_t:
      exts
  ~repl:
    cairo_text_extents(cr, "hello world").width
)

To round out the example, let's implement a function that draws text
scaled to fit the bitmap width:

@examples(
  ~eval: ffi_eval
  ~defn:
    cairo.fun cairo_show_text(cairo_t*, string_t) :: void_t
    cairo.fun cairo_scale(cairo_t*, double_t, double_t) :: void_t
  ~defn:
    fun fit_text(cr, str):
      def padding = 20
      cairo_move_to(cr, padding / 2.0, 128.0)
      def extents = cairo_text_extents(cr, str)
      def x_bearing = extents.x_bearing
      def width = extents.width
      def scale = (256.0 - padding) / (x_bearing + width)
      cairo_scale(cr, scale, scale)
      cairo_show_text(cr, str)
  ~repl:
    def (txt_bt, txt_cr) = make()
    fit_text(txt_cr, "Saluton, Mondo / Hallo, mundo")
    show(txt_bt)
)

@// --------------------------------------------------

@subsection(~tag: "tutorial:callbacks"){Callbacks from C to Rhombus}

To save a Cairo-based drawing to a PNG file, we could use Rhombus's
drawing utilities. Let's instead directly use Cairo's
@tt{cairo_surface_write_to_png_stream} to write bytes to a file. The C
signature is

@verbatim(~indent: 2){
cairo_status_t
cairo_surface_write_to_png_stream (cairo_surface_t *surface,
                                   cairo_write_func_t write_func,
                                   void *closure_data);
}

where @tt{cairo_write_func_t} is a function type:

@verbatim(~indent: 2){
cairo_status_t
(*cairo_write_func_t) (void *closure_data,
                       const unsigned char *data,
                       unsigned int length);
}

The @tt{closure_data} pointer as the third argument to
@tt{cairo_surface_write_to_png_stream} is passed along as the first
argument to @tt{write_func}, which is C's manual way of forming
closures. We'll let Rhombus take care of closures automatically for us,
so we'll just use a null pointer as @tt{closure_data}. The
interesting part is the function pointer passed as @tt{write_func}.

For the type of the function argument to
@rhombus(cairo_surface_write_to_png_stream), we can nest a
@rhombus_t(->) type for the argument inside a @rhombus_t(->) type for
the function. We'll also handle the result status by raising an
exception if the status is not @rhombus(0).

@examples(
  ~eval: ffi_eval
  ~defn:
    cairo.fun cairo_surface_write_to_png_stream(
      cairo_surface_t*,
      (ptr_t, ptr_t, int_t) -> int_t,
      ptr_t
    ) :: (r :: int_t) :: Void:
      unless r == 0 | error("write failed")
)

Then, when we pass a Rhombus function as the second argument to
@rhombus(cairo_surface_write_to_png_stream), it will be called to
write out bytes of the PNG encoding:

@examples(
  ~eval: ffi_eval
  ~defn:
    def png_out = Port.Output.open_file("/tmp/img.png", ~exists: #'truncate)
  ~defn:
    cairo_surface_write_to_png_stream(
      cast (cairo_surface_t*) bt_surface,
      fun (ignored, data, len):
        def png_data = Bytes.make(len)
        memcpy(cast ~from (bytes_ptr_t) ~to (ptr_t) png_data, data, len)
        Port.Output.write_bytes(png_out, png_data)
        // return 0 to mean "success"
        0,
      uintptr_to_ptr(0)
    )
  ~defn:
    Port.Output.close(png_out)
)

This example illustrates a technique for converting a pointer to bytes
into a byte string. The @rhombus(Port.Output.write_bytes) function wants a
byte string, not a C-level pointer to bytes, so we copy from the
@rhombus(data) pointer into a freshly allocated byte string. The copy
operation needs two pointers, so we cast a byte string to a pointer
using @rhombus_t(bytes_ptr_t). Note that casting from @rhombus_t(bytes_t),
instead of @rhombus_t(bytes_ptr_t), would make a copy of the byte string,
which would not work.

One catch here is that a callback from a foreign function is always in
@tech{atomic mode}. In this case, the callback writes to a port
@rhombus(png_out) that is not used from any other thread, so atomic mode
causes no problems; writing will not get stuck trying to acquire a lock.

Another subtlety is that the callback must remain live as long as it
might be called. The Rhombus procedure supplied as a callback will not be
garbage collected while it is being called, of course, but the C
representation of the callback is a wrapper that refers to the Rhombus
procedure, not the other way around. So, the callback could potentially
trigger a garbage collector that reclaims the callback wrapper. In this
case, the callback is kept live by virtue of being an argument to the
foreign function, and the callback is not used after
@rhombus(cairo_surface_write_to_png_stream) returns. When a callback is
registered for future use, then more care must be taken to retain the
pointer representing a callback.

@// --------------------------------------------------
@section(~tag: "tutorial-memory"){Memory Management}

A big difference between Rhombus and C is that memory allocation is
generally automatic in Rhombus and explicit in C. This difference creates
two challenges for a Rhombus binding to C functions:

@itemlist(

 @item{When a foreign function allocates an object, often the object needs to
  be freed by a matching call to another foreign function.}

 @item{When a foreign function is given Rhombus-allocated arguments, and
  when the Rhombus memory manager runs later, then Rhombus-allocated objects
  might get freed or moved in memory. In that case, references held by
  foreign code become invalid. This is a problem only when foreign code
  retains a reference across calls or when it invokes a callback that
  returns to Rhombus.}

)

@// --------------------------------------------------

@subsection{Reliable Release of Resources}

We are being sloppy with our calls to @rhombus(cairo_create). As the
function name suggests, @rhombus(cairo_create) allocates a new drawing
context, and we are never deallocating it. The Rhombus pointer object
that refers to a @rhombus_t(cairo_t*) value is reclaimed, but not the
memory (and other resources) of the @rhombus_t(cairo_t) itself.

A good first step is to define @rhombus(cairo_destroy) and apply it to
any drawing context that we no longer need, but what if we forget, or
what if an error occurs before we can reach a @rhombus(cairo_destroy)
call? The @rhombusmodname(ffi) library supports @defterm{finalization}
on an object to associate a clean-up action with a Rhombus object when
the object would otherwise be garbage-collected. Explicit deallocation
is generally better than relying on finalization, but finalization can
be appropriate in some cases and a good back-up in many cases.

The @rhombusmodname(ffi/finalize) library further wraps finalization
support to make it easy to pair an allocator with a deallocator. It
supplies @rhombus(finalize.allocator) and @rhombus(finalize.deallocator)
constructors that are meant to be used with @rhombus(~wrap) in
@rhombus(foreign.fun) and similar forms.

@examples(
  ~eval: ffi_eval
  ~defn:
    import:
      ffi/finalize
  ~defn:
    cairo.fun cairo_destroy(cairo_t*) :: void_t:
      ~wrap: finalize.deallocator()
    cairo.fun cairo_create(cairo_surface_t*) :: cairo_t*:
      ~wrap: finalize.allocator(cairo_destroy)
)

We define @rhombus(cairo_destroy) first so that it can be referenced by
the definition of @rhombus(cairo_create). The
@rhombus(~wrap: finalize.deallocator()) part of the definition of
@rhombus(cairo_destroy) identifies it as a deallocator, which will
unregister finalization (if any) for its argument. The
@rhombus(~wrap: finalize.allocator(cairo_destroy)) part of the definition
of @rhombus(cairo_create) identifies it as an allocator whose result can
be finalized by calling the given deallocator.

@// --------------------------------------------------

@subsection{Pointers and GC-Managed Allocation}

Going back to the @seclink("tutorial:callbacks"){callback example},
suppose we decide to use the @tt{closure_data} argument, after all, to
pass along a pointer to an integer that is updated on each callback
invocation:

@examples(
  ~eval: ffi_eval
  ~defn:
    ~fake:
      def the_counter = new int_t
      def the_counter = new ~immobile int_t
    the_counter[0] := 0
  ~defn:
    def png_out = Port.Output.open_file("/tmp/img.png", ~exists: #'truncate)
  ~defn:
    :
      // CAUTION: maybe do not run, because this may crash!
      cairo_surface_write_to_png_stream(
        cast (cairo_surface_t*) bt_surface,
        fun (counter :~ foreign.type int_t*, data, len):
          def png_data = Bytes.make(len)
          memcpy(
            cast ~from (bytes_ptr_t) ~to (ptr_t) png_data,
            data,
            len
          )
          Port.Output.write_bytes(png_out, png_data)
          counter[0] := counter[0] + 1
          0,
        the_counter
      )
  ~defn:
    Port.Output.close(png_out)
  ~repl:
    the_counter[0]
)

This may crash, or the counter may not increment correctly. Adding a
@rhombus(memory.gc()) call just before or after
@rhombus(Port.Output.write_bytes(png_out, png_data)) greatly increases the
chance of a failing counter increment.

The problem is that a garbage collection during the callback is likely
to move the object allocated by @rhombus(new) for
@rhombus(the_counter), but @tt{cairo_surface_write_to_png_stream} will
continue to provide the address that it was originally given as its
third argument.

One solution is to follow C allocation conventions and use manual memory
management for @rhombus(the_counter). Use the @rhombus(~manual)
allocation mode for @rhombus(new), instead of the default
@rhombus(~gcable) mode, and release the counter with @rhombus(free)
when it is no longer needed.

@examples(
  ~eval: ffi_eval
  ~defn:
    def the_counter = new ~manual int_t
    :
      // ...
      def end_count = the_counter[0]
    free(the_counter)
)

To avoid the drawbacks of manual memory management, another strategy is
to use the @rhombus(~immobile) allocation mode, which allocates
an object with automatic memory management, but the memory manager is
not allowed to relocate the object as long as it is live.

@examples(
  ~eval: ffi_eval
  ~defn:
    def the_counter = new ~immobile int_t
)

The constructor created by @rhombus(foreign.struct) uses @rhombus(new),
and it similarly accepts an allocation mode, as does the
@rhombus_t(list_t) type constructor. In all cases, the default is
@rhombus(~gcable) allocation, because most foreign functions use their
arguments and return without calling back to Rhombus. Beware of the
@rhombus(new) used for conversion by @rhombus_t(string_t) and
@rhombus_t(bytes_t), which is always in @rhombus(~gcable) mode; those
convenience types may not be suitable for some foreign functions.

@// --------------------------------------------------
@section(~tag: "tutorial3"){Foreign Unions and Pointer Arithmetic}

At the Rhombus level, when a representation involves a choice between
different shapes, then a predicate associated with each shape easily
distinguishes the cases. At the C level, different shapes can be merged
with a @tt{union} type to superimpose representations while relying on
some explicit indicator of which choice applies at any given point.

This level of representation detail and punning is closely related to
pointer arithmetic, which also ends up being used in similar places. As
in C, the @rhombusmodname(ffi) library provides a convenient veneer for
certain forms of pointer arithmetic by viewing pointers as references to
arrays.

@// --------------------------------------------------

@subsection{Declaring Union Types}

Let's work with Cairo
@hyperlink("https://www.cairographics.org/manual/cairo-Paths.html"){path}
objects, which encode vector-graphics drawing inputs. A path in Cairo is
defined as a @tt{cairo_path_t}:

@verbatim(~indent: 2){
typedef struct {
    cairo_status_t status;
    cairo_path_data_t *data;
    int num_data;
} cairo_path_t;
}

The @tt{data} field is a pointer, but not to just one
@tt{cairo_path_data_t}; it is a pointer to an array of
@tt{cairo_path_data_t}s, where @tt{num_data} indicates the length of
that array.

Individual elements of the array are defined by a @tt{union}:

@verbatim(~indent: 2){
union _cairo_path_data_t {
    struct {
        cairo_path_data_type_t type;
        int length;
    } header;
    struct {
        double x, y;
    } point;
};
}

That is, each element is either a header or a point. A header is followed
by @tt{length}-1 points. The @tt{cairo_path_data_type_t} within a header
is an integer that indicates whether it encodes a move-to, line-to, etc.,
operation.

For the Rhombus version of these types, it will be helpful to pull out
@rhombus_t(path_header_t) and @rhombus_t(path_point_t) into their own
definitions before combining them with @rhombus(foreign.union).

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.enum cairo_path_data_type_t int_t
    | move_to
    | line_to
    | curve_to
    | close_path
  ~defn:
    foreign.struct path_header_t(
      type :: cairo_path_data_type_t,
      length :: int_t
    )
  ~defn:
    foreign.struct path_point_t(
      x :: double_t,
      y :: double_t
    )
  ~defn:
    foreign.union cairo_path_data_t(
      header :: path_header_t,
      point :: path_point_t
    )
  ~defn:
    foreign.type cairo_status_t = int_t
  ~defn:
    foreign.struct cairo_path_t(
      status :: cairo_status_t,
      data :: cairo_path_data_t*,
      num_data :: int_t
    )
  ~defn:
    cairo.fun cairo_path_destroy(cairo_path_t*) :: void_t:
      ~wrap: finalize.deallocator()
    cairo.fun cairo_copy_path(cairo_t*) :: cairo_path_t*:
      ~wrap: finalize.allocator(cairo_path_destroy)
)

Let's create a fresh context, add path elements in it, and get the
accumulated path before it is used by @rhombus(cairo_stroke):

@examples(
  ~eval: ffi_eval
  ~defn:
    def (bt, cr) = make()
    cairo_move_to(cr, 50.0, 50.0)
    cairo_line_to(cr, 206.0, 206.0)
    cairo_move_to(cr, 50.0, 206.0)
    cairo_line_to(cr, 115.0, 115.0)
    def a_path = cairo_copy_path(cr)
    cairo_stroke(cr)
    cairo_destroy(cr)
)

The path should have status @rhombus(0) (success), and it should
have 8 components:

@examples(
  ~eval: ffi_eval
  ~repl:
    a_path.status
    a_path.num_data
    def data = a_path.data
    data
)

To access an individual element of the @rhombus(data) array, use
array indexing on the @rhombus_t(cairo_path_data_t*) pointer. Since
@rhombus(foreign.union) defines @rhombus_t(cairo_path_data_t), the pointer
type @rhombus_t(cairo_path_data_t*) carries static information that
enables @rhombus([]) indexing:

@examples(
  ~eval: ffi_eval
  ~repl:
    data[0]
)

A @rhombus_t(cairo_path_data_t) can be either the @rhombus(header) case or
the @rhombus(point) case. The @rhombus(foreign.union) definition gives us
@rhombus(.header) and @rhombus(.point) to access the union fields as
either a @rhombus_t(path_header_t) or @rhombus_t(path_point_t), respectively.
We know that the first element of @rhombus(data) must be the
@rhombus(header) case.

@examples(
  ~eval: ffi_eval
  ~repl:
    def head1 = data[0].header
    head1.type
    head1.length
)

A @rhombus(#'move_to) command has a single point, so we know that the
second element of @rhombus(data) is the @rhombus(point) case.

@examples(
  ~eval: ffi_eval
  ~repl:
    def pt1 = data[1].point
    pt1.x
    pt1.y
)

Next is @rhombus(#'line_to), and so on:

@examples(
  ~eval: ffi_eval
  ~repl:
    def head2 = data[2].header
    head2.type
    def pt2 = data[3].point
    pt2.x
    pt2.y
)

Now that we're done with this experiment, let's be good citizens by
cleaning up, although finalization will clean up after us if it must.

@examples(
  ~eval: ffi_eval
  ~repl:
    cairo_path_destroy(a_path)
)

Operations like @rhombus(data[i]) and field accessors like @rhombus(.header)
(or, more generally, accessors that access compound types within other
compound types) ultimately perform a kind of pointer arithmetic. In case
you ever need to take control of pointer arithmetic yourself,
@rhombusmodname(ffi) provides @rhombus(mem) with @rhombus(&) for shifting
pointer addresses by a given offset.

@// --------------------------------------------------

@subsection{Hiding Pointers through Conversion}

The Cairo path example illustrates how to traverse a complex structure,
but users of a set of Cairo bindings likely will not want to deal with
all of that complexity. Let's define a new type that converts the C
representation on demand by wrapping it as a sequence that's compatible
with @rhombus(for).

To make a sequence, we need a new Rhombus class that implements
@rhombus(Sequenceable, ~class). We'll define
@rhombus_t(auto_cairo_path_t) as a type that wraps a pointer as a
@rhombus(CairoPath) instance.

@examples(
  ~eval: ffi_eval
  ~defn:
    class CairoPath(ptr :~ cairo_path_t):
      implements Sequenceable
      override method to_sequence():
        in_cairo_path(this)
  ~defn:
    foreign.type auto_cairo_path_t:
      ~extends: cairo_path_t*
      ~predicate: fun (v): v is_a CairoPath
      ~c_to_rhombus: fun (p): CairoPath(p)
      ~rhombus_to_c: fun (rkt :~ CairoPath): rkt.ptr
)

The reference to @rhombus(in_cairo_path) is the doorway to the main
conversion, which iterates through Cairo path data:

@examples(
  ~eval: ffi_eval
  ~defn:
    fun in_cairo_path(path :~ CairoPath):
      def pp = path.ptr
      def array_ptr = pp.data
      def len = pp.num_data
      Sequence.make(
        ~initial_position: 0,
        ~continue_at_position: fun (pos): pos < len,
        ~position_to_element:
          fun (pos):
            def header = array_ptr[pos].header
            def type = header.type
            def count = header.length - 1
            [type,
             & for List (i in 0 .. count):
                 def pt = array_ptr[pos + 1 + i].point
                 [pt.x, pt.y]],
        ~position_to_next:
          fun (pos): pos + array_ptr[pos].header.length
      )
)

Let's redefine @rhombus(cairo_copy_path) and @rhombus(cairo_path_destroy)
and try the earlier example again.

@examples(
  ~eval: ffi_eval
  ~defn:
    cairo.fun cairo_path_destroy (auto_cairo_path_t) :: void_t:
      ~wrap: finalize.deallocator()
    cairo.fun cairo_copy_path (cairo_t*) :: auto_cairo_path_t:
      ~wrap: finalize.allocator(cairo_path_destroy)
  ~defn:
    def (bt, cr) = make()
    cairo_move_to(cr, 50.0, 50.0)
    cairo_line_to(cr, 206.0, 206.0)
    cairo_move_to(cr, 50.0, 206.0)
    cairo_line_to(cr, 115.0, 115.0)
    def auto_path :~ Sequence = cairo_copy_path(cr)
    cairo_stroke(cr)
    cairo_destroy(cr)
  ~repl:
    for (elem in auto_path):
      println(elem)
    cairo_path_destroy(auto_path)
)

@close_eval(ffi_eval)
