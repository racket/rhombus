#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread open
      ffi/finalize)

@title(~tag: "overview"){Overview}

Although using the FFI requires writing no new C code, it provides
relatively little insulation against the issues that C programmers face
related to safety and memory management. An FFI programmer must be
particularly aware of memory management issues for data that spans the
Rhombus--C divide. Although the library name @rhombusmodname(ffi) and
its exported bindings do not include the word ``unsafe,'' importing the
library should be considered as a declaration that your code is itself
unsafe, therefore can lead to serious problems in case of bugs. It is
the responsibility of each user of @rhombusmodname(ffi) to provide a
safe interface to its own clients.

@// --------------------------------------------------

@section{Libraries, C Types, and Objects}

To use the FFI, you must have in mind

@itemlist(

 @item{a particular library from which you want to access a function
       or value, }

 @item{a particular symbol exported by the file, and}

 @item{the C-level type (typically a function type) of the exported
       symbol.}

)

The library corresponds to a file with a suffix such as
@filepath{.dll}, @filepath{.so}, or @filepath{.dylib} (depending on
the platform), or it might be a library within a @filepath{.framework}
directory on Mac OS.

Knowing the library's name and/or path is often the trickiest part of
using the FFI.  Sometimes, when using a library name without a path
prefix or file suffix, the library file can be located automatically,
especially on Unix.

The @rhombus(Lib.load) function gets a handle to a library. To extract
exports of the library, it's simplest to use @rhombus(foreign.linker):

@rhombusblock(
  #,(@hash_lang()) #,(@rhombuslangname(rhombus/static))
  import:
    ffi open

  when stdin !is_a Port.FileStream.Terminal
  | error("not in a terminal")

  foreign.linker curses:
    ~lib: Lib.load("libcurses")
)

This @rhombus(foreign.linker) declaration introduces a
@rhombus(curses.fun) form for binding a Rhombus name to a value
extracted from @filepath{libcurses}---which might be located at
@filepath{/usr/lib/libcurses.so}, depending on the platform. This
@rhombus(curses.fun) is the same as @rhombus(foreign.fun), but
specialized to the library list after @rhombus(~lib).

To use @rhombus(curses.fun), we need the names and C types of functions
from @filepath{libcurses}. We'll start by using the following functions:

@verbatim(~indent: 2){
  WINDOW* initscr(void);
  int waddstr(WINDOW *win, char *str);
  int wrefresh(WINDOW *win);
  int endwin(void);
}

We make these functions callable from Rhombus as follows:

@margin_note_block{By convention, a @litchar{_t} suffix indicates a
 representation of a C type, even if the type does not use that suffix in
 C headers.}

@rhombusblock(
  foreign.type WINDOW_t

  curses.fun initscr() :: WINDOW_t*
  curses.fun waddstr(WINDOW_t*, string_t) :: int_t
  curses.fun wrefresh(WINDOW_t*) :: int_t
  curses.fun endwin() :: int_t
)

The definition of @rhombus_t(WINDOW_t) binds a type name that reflects
an opaque C type. Since the C representation of an opaque type is not
known, @rhombus_t(WINDOW_t) by itself will not be useful, but
@rhombus_t(WINDOW_t*) is represented on the C side by an address and on
the Racket side by a @tech{pointer} object that is tagged with
@litchar{WINDOW_t*}.

Each @rhombus(curses.fun) form uses the given identifier as both the
name of the library export and the Rhombus identifier to
bind.@margin_note{An optional @rhombus(~c_id) clause for
 @rhombus(foreign.fun) or @rhombus(curses.fun) can specify a name for the
 library export that is different from the Rhombus identifier to bind.}
After the function name, a parenthesized sequence describes the types
arguments to the function, and a result type is shown after
@rhombus(::, ~datum). Note that, unlike @rhombus(fun), argument names
are not required, while a type (instead of an annotation) is always
required for each argument and for the result.

The pre-defined @rhombus_t(int_t) type corresponds to the @tt{int} C
type, while @rhombus_t(string_t) corresponds to the @tt{char*} type when
it is intended as a string that is passed to a foreign function.

At this point, @rhombus(initscr), @rhombus(waddstr), @rhombus(wrefresh),
and @rhombus(endwin) are normal Rhombus bindings to Rhombus functions
(that happen to call C functions), and so they can be exported from
the defining module or called directly:

@rhombusblock(
  def win = initscr()
  waddstr(win, "Hello") |> Function.pass
  wrefresh(win) |> Function.pass
  Thread.sleep(1)
  endwin() |> Function.pass
)

@// --------------------------------------------------

@section{Defining a Type Conversion}

Our initial use of functions like @rhombus(waddstr) is sloppy, because
we ignore return codes. C functions often return error
codes, and checking them is a pain. A better approach is to build the
check into the @rhombus(waddstr) binding and raise an exception when
the code is non-zero.

We can use @rhombus(foreign.type) again to derive a type from
@rhombus_t(int_t), where the new type's conversion from C to Rhombus
raises an exception if the value is non-zero. More precisely, we
define a type constructor that is parameterized over a name to use
when reporting an error.

@rhombusblock(
  foreign.type status_t(who):
    ~extends int_t
    ~c_to_rhombus:
      fun (v):
        unless v == 0
        | error(~who: who, "failed: " +& v)

  curses.fun initscr() :: WINDOW_t*
  curses.fun waddstr(WINDOW_t*, string_t) :: status_t(#'waddstr)
  curses.fun wrefresh(WINDOW_t*) :: status_t(#'wrefresh)
  curses.fun endwin() :: status_t(#'endwin)
)

Using @rhombus_t(status_t(#'waddstr)) as a result type is the same as
using @rhombus_t(int_t) in terms of its C representation, but the
function specified with @rhombus(~c_to_rhombus) is applied to the Rhombus
representation of an @rhombus_t(int_t) result, and that function can inspect or
convert the value. In this case, @rhombus(status_t(#'waddstr)) returns
@rhombus(#void), since the status result is not needed after checking,
so @rhombus(waddstr), @rhombus(wrefresh), and @rhombus(endwin) will all
either return @rhombus(#void) or raise an exception when they are
called.

@// --------------------------------------------------

@section{References as Arguments}

To get mouse events from @filepath{libcurses}, we must explicitly
enable them through the @rhombus(mousemask) function:

@verbatim(~indent: 2){
typedef unsigned long mmask_t;
#define BUTTON1_CLICKED 004L

mmask_t mousemask(mmask_t newmask, mmask_t *oldmask);
}

Setting @rhombus(BUTTON1_CLICKED) in the mask enables button-click
events.  At the same time, @rhombus(mousemask) returns the current mask
by installing it into the pointer provided as its second
argument.

These kinds of call-with-a-reference interfaces are common in C. On the
Rhombus side, a procedure that returns two values would be better. We
could create a binding to @rhombus(mousemask) and then write a Rhombus
function to wrap it with an improved interface. Since that pattern is
common, @rhombus(foreign.fun) or @rhombus(curses.fun) helps with locally
named arguments, automatic argument-value expression, and
post-processing to provide a more Rhombus-like interface.

@rhombusblock(
  foreign.type mmask_t = ulong_t
  def BUTTON1_CLICKED = 0o004

  curses.fun mousemask(
    mmask_t,
    old :: mmask_t* = new mmask_t
  ) :: (r :: mmask_t):
    values(mem *old, r)

  def (old, supported) = mousemask(BUTTON1_CLICKED)
)

In this definition of @rhombus(mousemask), the second argument is
allocated automatically with @rhombus(= new mmask_t). Also, the argument
is given a name with @rhombus(old ::) so that it can be referenced after
the C procedure returns. The return value is also given a name with
@rhombus((r :: mmask_t)). Finally, the body of the definition has
an expression that produces the result for a call to @rhombus(mousemask),
in this case working with the allocated @rhombus(old) pointer and the
original result @rhombus(r).

@// --------------------------------------------------

@section{C Structs}

Assuming that mouse events are supported, the @filepath{libcurses}
library reports them via @rhombus(getmouse), which accepts a pointer to
a @tt{MEVENT} struct to fill with mouse-event information:

@verbatim(~indent: 2){
 typedef struct {
    short id;
    int x, y, z;
    mmask_t bstate;
 } MEVENT;

 int getmouse(MEVENT *event);
}

To work with @tt{MEVENT} values, we use @rhombus(foreign.struct) to
define @rhombus(MEVENT_t):

@rhombusblock(
  foreign.struct MEVENT_t(
    id :: short_t,
    x :: int_t,
    y :: int_t,
    z :: int_t,
    bstate :: mmask_t
  )
)

This definition binds @rhombus_t(MEVENT_t) as a type representing the
struct type, as a type that cooperates with @rhombus(new) to allocate an
instance of @rhombus_t(MEVENT_t), and as a namespace that provides field
accessors like @rhombus(x) and @rhombus(y). Static information for the
Rhombus representation of a @rhombus_t(MEVENT_t) instance allows
@rhombus(.)-based access to fields like @rhombus(x) and @rhombus(y).

With this C struct declaration, we can define the function type for
@rhombus(getmouse). The simplest approach is to define
@rhombus(getmouse) to accept an @rhombus_t(MEVENT_t*) pointer, so a caller
must explicitly allocate @rhombus(MEVENT_t) instance before calling
@rhombus(getmouse):

@rhombusblock(
  curses.fun getmouse(MEVENT_t*) :: int_t

  def m = new MEVENT_t(0, 0, 0, 0, 0)
  when getmouse(m) == 0
  | [m.x, m.y]
)

For a more Rhombus-like function, define @rhombus(getmouse) to allocate
automatically, where an extra declaration @rhombus(:: maybe(MEVENT_t))
describes the Rhombus result from @rhombus(getmouse) (as opposed to the
C function's result type):

@rhombusblock(
  curses.fun getmouse(
    m :: MEVENT_t* = new MEVENT_t(0, 0, 0, 0, 0)
  ) :: (r :: int_t) :: maybe(MEVENT_t):
    r == 0 && m

  waddstr(win, "click me fast...")
  wrefresh(win)
  Thread.sleep(1)

  def m = getmouse()
  when m
  | waddstr(win, "at " +& m!!.x +& ", " +& m!!.y)
    wrefresh(win)
  Thread.sleep(1)

  endwin()
)

On the Racket side, both @rhombus_t(MEVENT_t*) and @rhombus_t(MEVENT_t)
are represented by a pointer to a @rhombus_t(MEVENT_t) instance, but the
the difference between @rhombus_t(MEVENT_t*) and @rhombus_t(MEVENT_t) is
crucial on the C side. If the declared argument type were
@rhombus_t(MEVENT_t) instead of @rhombus_t(MEVENT_t*), then calling
@rhombus(getmouse) would pass a structure to the C function instead of a
pointer---likely triggering a crash. Along similar lines, a pointer to
an @rhombus_t(MEVENT_t) with uninitialized content could be created with
@rhombus(new MEVENT_t) instead of @rhombus(new MEVENT_t(0, 0, 0, 0, 0)),
and that would work fine in this case, but @rhombus(new MEVENT_t*) would
only allocate enough space to hold a pointer---likely triggering a crash
only later, unfortunately.

@// --------------------------------------------------

@section{Pointers and Manual Allocation}

To get text from the user instead of a mouse click, @filepath{libcurses}
provides @rhombus(wgetnstr):

@verbatim(~indent: 2){
int wgetnstr(WINDOW *win, char *str, int n);
}

While the @tt{char*} argument to @rhombus(waddstr) is treated as a
null-terminated string, the @tt{char*} argument to @rhombus(wgetnstr) is
treated as a buffer whose size is indicated by the final @tt{int}
argument. The type @rhombus_t(string_t) does not work for such buffers.

One way to approach this function from Rhombus is to describe the
arguments in their rawest form, using plain @rhombus(ptr_t) for the
second argument to @rhombus(wgetnstr):

@rhombusblock(
  curses.fun wgetnstr(WINDOW_t*, ptr_t, int_t) :: int_t
)

To call this raw version of @rhombus(wgetnstr), allocate memory, zero
it, and pass the size minus one (to leave room a null terminator) to
@rhombus(wgetnstr):

@rhombusblock(
  def SIZE = 256
  def buffer = malloc ~manual (SIZE)
  memset(buffer, 0, SIZE)

  wgetnstr(win, buffer, SIZE-1)
)

When @rhombus(wgetnstr) returns, it has written bytes to
@rhombus(buffer). At that point, we can use @rhombus(cast) to convert
the value from a raw pointer to a string:

@rhombusblock(
  cast (string_t)buffer
)

Conversion via the @rhombus_t(string_t) type causes the data referenced
by the original pointer to be copied (and UTF-8 decoded), so the
memory referenced by @rhombus(buffer) is no longer needed. Since the
buffer was allocated in @rhombus(~manual) mode, use @rhombus(free)
later to release the allocated memory:

@rhombusblock(
  free(buffer)
)

@// --------------------------------------------------

@section{Pointers and GC-Managed Allocation}

Instead of allocating @rhombus(buffer) with @rhombus(~manual), we could
have allocated it with the default @rhombus(~gcable) mode:

@margin_note_block{The @rhombus(new) form supports the same allocation
 modes as @rhombus(malloc).}

@rhombusblock(
  def buffer = malloc(SIZE) // or  malloc ~gcable (SIZE)
)

Memory allocated with @rhombus(~gcable) is managed by the garbage
collector, so @rhombus(free) is neither necessary nor allowed when the
memory referenced by @rhombus(buffer) is no longer needed. Instead, when
@rhombus(buffer) becomes inaccessible, the allocated memory will be
reclaimed automatically.

Allowing the garbage collector (GC) to manage memory is usually
preferable. It's easy to forget to call @rhombus(free), and exceptions
or thread termination can easily skip a @rhombus(free).

At the same time, using GC-managed memory adds a different burden on the
programmer: data managed by the GC may be moved to a new address as the
GC compacts allocated objects to avoid fragmentation. C functions,
meanwhile, expect to receive pointers to objects that will stay put.

Fortunately, unless a C function calls back into the Rhombus runtime
system (perhaps through a function that is provided as an argument),
no garbage collection will happen between the time that a C function
is called and the time that the function returns.

Let's look a few possibilities related to allocation and pointers:

@itemlist(

 @item{Ok:

  @rhombusblock(
    def p = malloc ~gcable (SIZE)
    wgetnstr(win, p, SIZE-1)
  )

 Although the data allocated by @rhombus(malloc) can move
 around, @rhombus(p) will always point to it, and no garbage collection
 will happen between the time that the address is extracted from
 @rhombus(p) to pass to @rhombus(wgetnstr) and the time that
 @rhombus(wgetnstr) returns.}

 @item{Bad:

  @rhombusblock(
    def p = malloc ~gcable (SIZE)
    def i = ptr_to_uintptr(p)
    wgetnstr(win, uintptr_to_ptr(i), SIZE-1)
  )

 The data referenced by @rhombus(p) can move after the address is
 converted to an integer, in which case @rhombus(i) cast back to a
 pointer will be the wrong address.

 Obviously, casting a pointer to an integer is generally a bad idea, but
 the cast simulates another possibility, which is passing the pointer to
 a C function that retains the pointer in its own private store for later
 use. Such private storage is invisible to the Rhombus GC, so it has the
 same effect as casting the pointer to an integer.}

 @item{Ok:

  @rhombusblock(
    def p  = new byte_t[SIZE]
    def p2 = mem &p[4]
    wgetnstr(win, p2, SIZE-5)
  )

 The pointer @rhombus(p2) retains the original reference and only adds
 the offset @rhombus(4) at the last minute before calling
 @rhombus(wgetnstr) (i.e., after the point that garbage collection is
 allowed).}

 @item{Ok:

  @rhombusblock(
    def p = malloc ~immobile (SIZE)
    def i  = ptr_to_uintptr(p)
    wgetnstr(win, uintptr_to_ptr(i), SIZE-1)
    Function.black_box(p)
  )

 This is ok because @rhombus(p) itself stays accessible, so that the
 data it references isn't reclaimed. Allocating with @rhombus(~immobile)
 puts data at a particular address and keeps it there. A garbage
 collection will not change the address in @rhombus(p), and so
 @rhombus(i) (cast back to a pointer) will always refer to the data.}

)

Keep in mind that C struct constructors like @rhombus(new MEVENT_t(....)) are
effectively the same as @rhombus(malloc(...)); by default, the
result values can move in memory during a garbage collection. The same
is true of byte strings allocated with @rhombus(Bytes.make), which (as
a convenience) can be used directly as a pointer value via the
@rhombus(byte_ptr_t) type (unlike character strings, which are always
copied for UTF-8 encoding or decoding).

@// --------------------------------------------------

@section{Reliable Release of Resources}

Using GC-managed memory saves you from manual @rhombus(free)s for
plain memory blocks, but C libraries often allocate resources and
require a matching call to a function that releases the resources. For
example, @filepath{libcurses} supports windows on the screen that are
created with @rhombus(newwin) and released with @rhombus(delwin):

@verbatim(~indent: 2){
WINDOW *newwin(int lines, int ncols, int y, int x);
int delwin(WINDOW *win);
}

In a sufficiently complex program, ensuring that every @rhombus(newwin)
is paired with @rhombus(delwin) can be challenging, especially if the
functions are wrapped by otherwise safe functions that are provided from
a library. A library that is intended to be safe for use in a sandbox,
say, must protect against resource leaks within the Rhombus process as a
whole when a sandboxed program misbehaves or is terminated.

The @rhombusmodname(ffi/finalize) library provides functions to connect
resource-allocating functions and resource-releasing functions. The
library then arranges for finalization to release a resource if it
becomes inaccessible (according to the GC) before it is explicitly
released. At the same time, the library handles tricky atomicity
requirements to ensure that the finalization is properly registered and
never run multiple times.

Using @rhombusmodname(ffi/finalize), the @rhombus(newwin) and
@rhombus(delwin) functions can be defined with
@rhombus(finalize.allocator) and @rhombus(finalize.deallocator)
wrappers, respectively:

@rhombusblock(
  import:
    ffi/finalize

  curses.fun delwin(WINDOW_t*) :: status_t(#'delwin):
    ~wrap: finalize.deallocator()

  curses.fun newwin(int_t, int_t, int_t, int_t) :: WINDOW_t*:
    ~wrap: finalize.allocator(delwin)
)

A @rhombus(finalize.deallocator) wrapper makes a function cancel any
existing finalizer for the function's argument. An
@rhombus(finalize.allocator) wrapper refers to the deallocator, so that
the deallocator can be run if necessary by a finalizer.

@// --------------------------------------------------

@section{Threads}

Although older versions of @filepath{libcurses} are not thread-safe,
Rhombus coroutine threads do not correspond to OS-level threads, so
using Rhombus coroutine threads to call @filepath{libcurses} functions
creates no particular problems.

Rhombus parallel threads, however, correspond to OS-level threads. Using
a foreign library from multiple parallel threads works when the library
is thread-safe. Calling a non-thread-safe library from multiple parallel
threads requires more care.

The simplest way to use a non-thread-safe library from multiple places
is to specify the @rhombus(~in_original) option, which routes every call
to the function through the original Rhombus OS thread instead of the
calling thread. Most of the functions that we initially used from
@filepath{libcurses} can be made thread-safe simply:

@rhombusblock(
  curses.fun initscr() :: WINDOW_t*:
    ~in_original
  curses.fun wrefresh(WINDOW_t*) :: status_t(#'wrefresh):
    ~in_original
  curses.fun endwin() :: status_t(#'endwin):
    ~in_original
)

The @rhombus(waddstr) function is not quite as straightforward. The
problem with

@rhombusblock(
  curses.fun waddstr(WINDOW_t*, string_t) :: status_t(#'waddstr):
    ~in_original
)

is that it will block all threads from garbage collection until
@rhombus(waddstr) returns. Adding @rhombus(~collect_safe) addresses that
problem:

@rhombusblock(
  curses.fun waddstr(WINDOW_t*, string_t) :: status_t(#'waddstr):
    ~in_original
    ~collect_safe
)

Now, however, the string argument to @rhombus(waddstr) might move before
the @rhombus(waddstr) call completes. To safely call @rhombus(waddstr)
while allowing garbage collection, we can define a
@rhombus(string_immobile_t) type that allocates bytes for the string
argument with @rhombus(~immobile):

@rhombusblock(
  foreign.type string_immobile_t:
    ~extends ptr_t
    ~predicate: fun (v): v is_a String
    ~rhombus_to_c:
      fun (s :~ String):
        def bstr = s.utf8_bytes()
        def len = bstr.length()
        def p = new ~immobile byte_t[len]
        memcpy(p, cast ~from (bytes_ptr_t) ~to (ptr_t) bstr, len)
        mem p[len] := 0
        p
    ~c_to_rhombus:
      fun (p):
        cast (string_t)p

  curses.fun waddstr(WINDOW_t*, string_immobile_t) :: status_t(#'waddstr):
    ~in_original
    ~collect_safe
)
