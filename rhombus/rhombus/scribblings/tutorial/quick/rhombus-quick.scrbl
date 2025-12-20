#lang rhombus/scribble/manual
@(import:
    rhombus/meta open
    rhombus/runtime_path
    "../../common.rhm" open
    meta_label:
      pict open
      pict/rhombus open)

@(def pict_eval = make_rhombus_eval())
@examples(
  ~eval: pict_eval
  ~hidden:
    import pict open
)

@(expr.macro 'repl($arg, ...)':
    '@examples(~eval: pict_eval, $arg, ...)')

@(def bracket = @elem{@litchar{[}…@litchar{]}})

@(runtime_path.def rhombus_quick_scrbl: "rhombus-quick.scrbl")
@(def meta_tutorial_doc:
    ModulePath'lib("rhombus/scribblings/tutorial/metaprogramming/rhombus-meta-tutorial.scrbl")')

@title(~category: #'tutorial){Quick Introduction to Rhombus with Pictures}

This tutorial provides a brief introduction to the
@seclink("top", ~doc: guide_doc){Rhombus} programming language by using
one of its picture-drawing libraries. Even if you don't intend to use
Rhombus for your artistic endeavors, the picture library supports
interesting and enlightening examples. After all, a picture is worth
five hundred ``hello world''s.

Along the same lines, we assume that you will run the examples using
@seclink("Quick_Start", ~doc: guide_doc){DrRacket}. (Rhombus is built on
@hyperlink("https://racket-lang.org/"){Racket} and uses many racket
tools.) Using DrRacket is the fastest way to get a sense of what the
language and system feels like, even if you eventually use Rhombus with
Emacs, vi, VS Code, or some other editor.

@// ----------------------------------------------------------------------
@section{Ready...}

@hyperlink("https://rhombus-lang.org/download.html"){Download Rhombus},
install, and then start DrRacket.

@// ----------------------------------------------------------------------
@section{Set...}

To draw pictures, we must first import some picture functions.  Copy the
following into the @defterm{definitions area}, which is the top text
area that you see in DrRacket:
@margin_note{See @seclink(~indirect: #true,
                          ~doc: ModulePath'lib ("scribblings/drracket/drracket.scrbl") ',
                          "interface-essentials"){the DrRacket documentation}
             for a brief overview of the DrRacket IDE.}

@rhombusblock(
  #,(@hash_lang()) #,@(rhombuslangname(rhombus))
  import:
    #,(@racketmodname(pict)) open
)

Then click the @onscreen{Run} button. You'll see the text caret move
to the bottom text area, which is the @defterm{interactions area}.

@// ----------------------------------------------------------------------
@section{Go!}

When you type an expression after the @onscreen{>} in the interactions
window and hit Enter, DrRacket evaluates the expression and prints its
result. An expression can be just a value, such as the number
@rhombus(5) or the string @rhombus("art gallery"):

@repl(
  5
  "art gallery"
)

Arithmetic, simple function calls, and method calls look the same as in
many other languages:

@repl(
  1 + 2
  math.max(1, 3)
  "art gallery".titlecase()
  circle()
)

A result from the @rhombus(circle) function is a picture value, which
prints as an expression result in much the same way that numbers or
strings print.

The @rhombus(circle) function accepts optional keyword arguments. A
keyword is written starting with @litchar{~}, and a keyword argument is
written as a keyword followed by @litchar{:} and the argument.

@repl(
  circle(~size: 20, ~fill: "red")
  rectangle(~width: 10, ~height: 20, ~line: "blue")
)

Try giving @rhombus(circle) wrong arguments, just to see what happens:

@repl(
  ~error:
    circle(20, "red")
)

Note that DrRacket highlights in pink the expression that triggered
the error (but pink highlighting is not shown in this documentation).

In addition to basic picture constructors like @rhombus(circle) and
@rhombus(rectangle), there's a @rhombus(beside) function that
combines pictures:

@repl(
  beside(circle(~size: 20),
         rectangle(~width: 10, ~height: 20),
         ~sep: 5)
)

If you wonder what other functions exist---perhaps a way to stack
pictures vertically and left-aligned?---move the text caret to the name
@rhombus(beside) and press the F1 key in DrRacket. A browser window will
open, and it will give you a link to the documentation for
@rhombus(beside). Click the link, and you'll see lots of other
functions.

If you're reading this in a browser, you can also just click on
@rhombus(beside) or any other imported identifier that is used in this
tutorial.

@// ----------------------------------------------------------------------
@section{Definitions}

To use a particular circle and rectangle picture many times, it's
simpler to give them names. Move back to the definitions area (the top
area) and add two definitions, so that the complete content of the
definitions area looks like this:

@rhombusblock(
  #,(@hash_lang()) #,@(rhombuslangname(rhombus))
  import:
    pict open

  def c = circle(~size: 10)
  def r = rectangle(~width: 10, ~height: 20)
)

@repl(
  ~hidden:
    def c = circle(~size: 10)
    def r = rectangle(~width: 10, ~height: 20)
)

Then click @onscreen{Run} again. Now, you can just type @rhombus(c) or
@rhombus(r):

@repl(
  r
  beside(c, r)
  beside(~sep: 20, c, r, c)
)

As you can see, the @rhombus(beside) function accepts any number of
picture arguments, while an optional @rhombus(~sep) argument specifies
the amount of space to add between pictures.

We could have evaluated the @rhombus(def) forms for @rhombus(c) and
@rhombus(r) in the interactions area instead of the definitions area. In
practice, though, the definitions area is where your program
lives---it's the file that you save---while the interaction area is for
transient explorations and debugging tasks.

Let's add a function definition to the program. A function definition
uses @rhombus(fun, ~defn), then a function name, a parenthesized
sequence of @litchar{,}-separated names for the function arguments, a
@litchar{:}, and then the body indented under the @litchar{:}.

@repl(
  ~defn:
    fun cell(n):
      // Two slashes start start a line comment.
      // The expression below is the function body.
      rectangle(~width: n, ~height: n, ~fill: ColorMode.inherit)
)

The use of @rhombus(ColorMode.inherit) will let us apply a fill color
externally. Meanwhile, it defaults to black:

@repl(
  cell(10)
)

In the same way that definitions can be evaluated in the interactions
area, expressions can be included in the definitions area. When a
program is run, expression results from the definition area are shown
in the interaction area. From now on, we'll write our example
definitions and expressions together, and you can put them in
whichever area you prefer. The examples will build on each other,
however, so it's best to put at least the definitions in the
definition area.

@// ----------------------------------------------------------------------
@section{Annotations}

Our first try at @rhombus(cell) is a little sloppy, because it takes any
argument and passes it on to @rhombus(rectangle), which can trigger an
error from @rhombus(rectangle) if that argument is bad.

@repl(
  ~error:
    cell("red")
)

A better definition of @rhombus(cell) @defterm{annotates} its arguments
using @rhombus(::, ~bind) to impose a check that the argument is valid,
and it declares an annotation for the function's result using
@rhombus(::, ~bind). The result annotation is written after the
parentheses for arguments and before @litchar{:} for the function body.

@repl(
  ~defn:
    fun cell(n :: NonnegReal) :: Pict:
      rectangle(~width: n, ~height: n, ~fill: ColorMode.inherit)
  ~repl:
    ~error:
      cell("red")
    cell(10).colorize("blue")
)

If @rhombus(cell) accidentally returned a value that is not a picture,
then the @rhombus(:: Pict, ~bind) result annotation would catch the
error before returning that value. More importantly, the result
annotation for @rhombus(cell) makes the call to the @rhombus(colorize)
method in @rhombus(cell(10).colorize("blue")) resolve statically to the
@rhombus(Pict.colorize) method, instead of calling just any method on
the target object that happens to be named @rhombus(colorize). Although
calling a dynamically discovered @rhombus(colorize) is sometimes useful,
static dispatch is normally better because it's faster and
safer.@margin_note{@defterm{Static} here does not mean static in the
 sense of static methods in Java, but in the sense of static typing. If
 @rhombus(Pict, ~annot) has subclasses that override @rhombus(colorize),
 then a call to @rhombus(Pict.colorize) dispatches to an overriding
 implementation, if any.}

A @rhombus(::, ~bind) result annotation does incur the cost of a
run-time check (unless the check is proven unneecssary by the
optimizer). Instead of @rhombus(::, ~bind), use @rhombus(:~, ~bind) to
declare static information without an accompanying run-time check. In
that case, the static information is just assumed to be correct.
Meanwhile, the declaration @rhombus(use_static) or the
@rhombuslangname(rhombus/static) language ensures that operators like
@rhombus(.) are only ever used in a way that can be statically resolved.

@// ----------------------------------------------------------------------
@section{Local Binding}

The @rhombus(def) form can be used in some places to create local
bindings. For example, it can be used inside a function body:

@repl(
  ~defn:
    fun checker(p1 :: Pict, p2 :: Pict) :: Pict:
      def top = beside(p1, p2)
      def bottom = beside(p2, p1)
      stack(top, bottom)
  ~repl:
    checker(cell(10).colorize("black"),
            cell(10).colorize("red"))
)

Within a local block, Rhombus programmers will more often use
@rhombus(let) than @rhombus(def). The difference is that @rhombus(let)
allows a later definition with @rhombus(let) to use the same name. The
later definition does not modify the earlier definition's variable; it
just makes the new definition's variable the one that is seen afterward
in the block.

@repl(
  ~defn:
    fun checkerboard(p :: Pict) :: Pict:
      let result = checker(p.colorize("black"),
                           p.colorize("red"))
      let result = checker(result, result)
      let result = checker(result, result)
      result
  ~repl:
    checkerboard(cell(10))
)


@// ----------------------------------------------------------------------
@section{Functions are Values}

Instead of calling @rhombus(circle) as a function, try evaluating just
@rhombus(circle) as an expression:

@repl(
  circle
)

The identifier @rhombus(circle) is bound to a function just like
@rhombus(c) is bound to a circle. Unlike a circle picture, there's not a
simple way of completely printing the function, so DrRacket just prints
@litchar{#<function:circle>}.

This example shows that functions are values, just like numbers and
pictures (even if they don't print as nicely). Since functions are
values, you can define functions that accept other functions as
arguments. The @rhombus(->, ~annot) annotation constructor lets you
describe a function in terms of its argument and result annotations.

@repl(
  ~defn:
    fun series(mk :: Int -> Pict) :: Pict:
      beside(~sep: 4, mk(5), mk(10), mk(20))
  ~repl:
    series(cell)
    ~error:
      series(circle)
)

Passing @rhombus(circle) to @rhombus(series) doesn't work, because
@rhombus(circle) expects a keyword argument instead of a single
by-position argument. We could define a new function just for this
purpose, but if if we only need to adapt @rhombus(circle) for just one
use, then writing an extra definition is a hassle. The alternative is to
use the expression form @rhombus(fun), which creates an anonymous
function:

@repl(
  series(fun (n): circle(~size: n))
)

The expression form of @rhombus(fun) is just like the definition form,
but without the function name. An annotation could have been written on
the argument @rhombus(n) or for the result of the function, but
annotations don't particularly help in this case.

A @rhombus(fun, ~defn) definition form is really a shorthand for
@rhombus(def) plus a @rhombus(fun) expression. For example, the
@rhombus(series) definition could be written as

@repl(
  ~defn:
    def series = (fun (mk :: Int -> Pict) :: Pict:
                    beside(~sep: 4, mk(5), mk(10), mk(20)))
)

As a small syntactic adjustment, @litchar{:} can be used with
@rhombus(def) instead of @rhombus(=), which usually makes more sense
when the right-hand side of the definition spans multiple lines.

@repl(
  ~defn:
    def series:
      fun (mk :: Int -> Pict) :: Pict:
        beside(~sep: 4, mk(5), mk(10), mk(20))
)

In any case, Rhombus programmers generally prefer to use the shorthand
@rhombus(fun, ~defn) definition form instead of this @rhombus(def) plus
@rhombus(fun) combination.

@// ----------------------------------------------------------------------
@section{Lists}

Lists in Rhombus are written with @brackets square brackets.

@repl(
  ["red", "green", "blue"]
  [circle(~size: 10), rectangle(~width: 10, ~height: 20)]
)

Rhombus lists are immutable. If you add or remove a list item, then you
get a new list, and the old one remains unmodified.

@repl(
  ~defn:
    def colors = ["red", "orange", "yellow", "green", "blue", "purple"]
    def favorite_colors = colors.remove("green").add("pink")
  ~repl:
    favorite_colors
    colors
)

Use @brackets square brackets as a postfix operation to
extract an element from a list by position (counting from 0). The
@rhombus(++) infix operator appends lists. Lists support many other
typical methods, such as @rhombus(List.remove) and @rhombus(List.add)
shown above. Despite the immutable nature of lists, they take advantage
of sharing internally so that most operations take @math{O(log N)} time
for a list of size @math{N}---even operations like @rhombus(List.append)
or @rhombus(List.sublist)---so Rhombus programmers usually don't need
to worry about the efficiency of lists.

@repl(
  colors[0]
  favorite_colors ++ ["black", "white"]
  colors.take(3)
)

The @rhombus(List.map) method takes a function to apply to each element
of the list, and it creates a new list.

@repl(
  ~defn:
    fun rainbow(p :: Pict) :: List.of(Pict):
      colors.map(fun (c): p.colorize(c))
  ~repl:
    rainbow(cell(10))
)

When calling a function, you can use @rhombus(&) to splice a list of
arguments into the function call.

@repl(
  stack(& rainbow(cell(10)))
)

Note that @rhombus(stack(rainbow(cell(10)))) would not work, because
@rhombus(stack) does not want a list as an argument; it wants individual
arguments that are @rhombus(Pict, ~annot)s, but it is willing to accept
any number of arguments.

@// ----------------------------------------------------------------------
@section{Patterns and Repetitions}

In most places within a Rhombus program where a variable is bound, the
binding can be a @defterm{pattern} instead of just a plain identifier.
Patterns imitate value-construction forms, so a list pattern is written
with @brackets square brackets.

@repl(
  ~defn:
    fun grid([[a :: Pict, b :: Pict],
              [c :: Pict, d :: Pict]]) :: Pict:
      stack(beside(a, b),
            beside(c, d))
  ~repl:
    grid([[cell(10).colorize("red"), cell(10).colorize("orange")],
          [cell(10).colorize("yellow"), cell(10).colorize("green")]])
)

Most uses of lists involve any number of elements, instead of a fixed
number. To support matching those kinds of lists, a list pattern can use
@rhombus(...) after a subpattern to bind a @defterm{repetition} of the
subpattern. Identifiers bound by the subpattern can be used later in a
constructor that recognizes @rhombus(...). Here's another way to define
@rhombus(rainbow) by letting @rhombus(c) stand for each color in the
@rhombus(colors) list.

@repl(
  ~defn:
    fun rainbow(p :: Pict) :: List.of(Pict):
      let [c, ...] = colors
      [p.colorize(c), ...]
  ~repl:
    rainbow(cell(10))
)

Concrete shapes and @rhombus(...) can be mixed. The next example uses
the ``don't care'' pattern @rhombus(_, ~bind) to match any number of
additional list elements in the result of @rhombus(rainbow), as long as
there are at least four elements.

@repl(
  def [a, b, c, d, _, ...] = rainbow(cell(10))
  grid([[a, b],
        [c, d]])
)

@// ----------------------------------------------------------------------
@section{Code as Data}

The @rhombus(text) function from @rhombusmodname(pict) creates a picture
of text.

@repl(
  text("Hello").colorize("blue")
  text("Bye").scale(2)
)

Suppose, though that we are creating a tutorial about the
@rhombusmodname(pict) library for Rhombus, and we want to show literally
the code that is shown in the interaction that calls @rhombus(rainbow).
In that case, the literal @rhombus(10) should use the color for literals
and the parentheses @rhombus(()) should use the color for parentheses.
Building up the expression with individually colorized @rhombus(text)
calls would be tedious.

The @rhombusmodname(pict/rhombus) module provides a @rhombus(rhombus)
form that typesets its ``argument'' instead of evaluating it:

@repl(
  ~defn:
    import:
      pict/rhombus open
  ~repl:
    rhombus(rainbow(cell(10))).scale(2)
)

The @rhombus(rhombus) form is possible because it is implemented as a
@defterm{macro} instead of a function. Metaprogramming is often used to
define or extend a programming language more generally, and that's the
subject of @seclink("top", ~doc: meta_tutorial_doc){another tutorial}
(that's much longer).

This example may seem frivolous at first glance, but consider that you
are currently reading a tutorial about Rhombus that is filled with
example code. Here's the source:
@elem(~style: Style(~properties: [Style.HTML.LinkResource(rhombus_quick_scrbl)])){rhombus-quick.scrbl}.
You'll see many uses of forms that quote and typeset code---for
documentation rendering instead of pictures, but it's the same idea.
Metaprogramming is pervasive in software development, and Rhombus
supports it directly.

@// ----------------------------------------------------------------------
@section{Modules}

Since your program in the definitions window starts with

@rhombusblock(
  #,(@hash_lang()) #,(@rhombuslangname(rhombus))
)

all of the code that you put in the definitions window is inside a
module. Furthermore, the module initially imports everything from the
@rhombuslangname(rhombus) language.

We have imported additional libraries using the @rhombus(import) form.
For convenience, we have @rhombus(open, ~impo)ed each import, but if
@rhombus(open, ~impo) is omitted, then imported bindings are available
through a dotted name that starts with the last component of the module
path. For example, the @rhombusmodname(pict/radial) library provides a
@rhombus(flower) function:

@repl(
  ~defn:
    import:
      pict/radial
  ~repl:
    radial.flower(~fill: "pink")
)

Modules are named and distributed in various ways:

@itemlist(

 @item{Some modules are packaged in the Rhombus or Racket distribution
 or otherwise installed into a hierarchy of @defterm{collections}. For
 example, the module name @rhombusmodname(pict/radial) means ``the module
 implemented in the file @filepath{radial.rhm} that is located in the
 @filepath{pict} collection.'' When a module name includes no slash, then
 it refers to a @filepath{main.rhm} file.}

 @item{Some collections of modules are distributed as
 @defterm{packages}. Packages can be installed using the
 @onscreen{Install Package...} menu item in DrRacket's @onscreen{File}
 menu, or they can be installed using the @exec{raco pkg} command-line
 tool. For example, installing the @filepath{avl} package makes the
 @racketmodname(avl, ~indirect) module available.

 Packages can be registered at @url("https://pkgs.racket-lang.org/"), or
 they can be installed directly from a Git repository, web site, file, or
 directory. See @docref(ModulePath'lib("pkg/scribblings/pkg.scrbl")')
 for more information about packages.}

 @item{Some modules live relative to other modules, without necessarily
 belonging to any particular collection or package. For example, in
 DrRacket, if you save your definitions so far in a file
 @filepath{quick.rhm} and add the line

@rhombusblock(
  export:
    cell
    rainbow
)

 then you can open a new tab or window in DrRacket, type the new program
 @filepath{use.rhm} in the same directory as @filepath{quick.rhm}:

@rhombusblock(
  #,(@hash_lang()) #,(@rhombuslangname(rhombus))
  import:
    "quick.rhm"
  quick.rainbow(quick.cell(5))
)

 and when you run @filepath{use.rhm}, a rainbow list of squares is the
 output.}

)

Rhombus programmers typically write new programs and libraries as
modules that import each other through relative paths and
collection-based paths. When a program or library developed this way
seems useful to others, it can be registered as a package, especially if
the implementation is hosted in a Git repository.


@// ----------------------------------------------------------------------
@section{Where to Go From Here}

To start learning about the full Rhombus language, move on to
@docref(guide_doc).

If you are familiar with Rhombus-style languages---enough that you're
willing to wing it---and you are particularly interested in Rhombus's
metaprogramming facilities, then you might instead try
@docref(meta_tutorial_doc).
