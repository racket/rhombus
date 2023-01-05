#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "Modules"){Modules, Variables, and Functions}

A Rhombus module, which implements a program or a library, always
starts @litchar{#lang rhombus}. If you write an expression at the top of a
module, then its value gets printed out.

@rhombusblock(
  $$(hash_lang()) $$(@rhombusmodname(rhombus))

  1+4  // prints 5
  
  "Hello, world!"  // prints "Hello, world!", including the quotes
)

@aside{If you have installed the @pkg{rhombus-prototype} package, then
 you can run Rhombus modules in DrRacket or by supplying the file path to
 @exec{racket} on the command line.}

The ways to define names in a module include @rhombus(def) and
@rhombus(fun). The @rhombus(def) form defines an immutable variable, and
it expects an identifier to define followed by either @rhombus(=) or a block. The
@rhombus(fun) form defines a function when it see an identifier,
parentheses around argument names, and then a block. Function calls have
the usual shape: a function name (or, more generally, an expression that
produces a function) followed by comma-separated arguments in
parentheses.

@rhombusblock(
  $$(hash_lang()) $$(@rhombusmodname(rhombus))

  def fahrenheit_freezing = 32
                           
  fun fahrenheit_to_celsius(f):
    (f - 32) * 5/9

  fahrenheit_to_celsius(fahrenheit_freezing)  // prints 0
)

@aside{The interactions area in DrRacket will work to call
 @rhombus(fahrenheit_to_celsius). In interactions, a single input line is
 accepted as complete as long as it's openers and closers are balanced,
 and as long as it doesn't contain @litchar{:} or @litchar{;} outside of
 an opener--closer pair. A blank line terminates multi-line input. For
 multi-line input where the first line would otherwise parse as complete,
 add @litchar{:} or @litchar{;} at the front, either on the same line or
 its own line.}

@aside(~italic: #false){To get a Rhombus read-eval-print loop on the
 command line, use @exec{racket -I rhombus}. The rules for single-line
 and multi-line input are the same as in DrRacket's interactions area.
 Use can use the @litchar{,enter} command to load a module and evaluate
 additional expressions in the context of that module's body.}

The definition of @rhombus(fahrenheit_freezing) could also have been
written with @litchar{:} instead of @rhombus(=), like this:

@rhombusblock(
  def fahrenheit_freezing: 32
)

By convention, however, @rhombus(=) is used for single expressions, while
@litchar{:} is useful for multi-line definitions and blocks. A @rhombus(=) is
interchangable for @litchar{:} only in certain forms, like
@rhombus(def).

A Rhombus module can export definitions to other modules using
@rhombus(export), and it can import other modules using
@rhombus(import). The @litchar{#lang rhombus} line is a kind of
@rhombus(import) already, so normally more @rhombus(import)s are written
at the top of a module, and then @rhombus(export)s, and then the
definitions.

@rhombusblock(
  // f2c.rhm
  $$(hash_lang()) $$(@rhombusmodname(rhombus))

  export:
    fahrenheit_freezing
    fahrenheit_to_celsius

  def fahrenheit_freezing = 32

  fun fahrenheit_to_celsius(f):
    (f - 32) * 5/9
)

@rhombusblock(
  // freezing.rhm
  $$(hash_lang()) $$(@rhombusmodname(rhombus))

  import:
    "f2c.rhm"

  f2c.fahrenheit_to_celsius(f2cfahrenheit_freezing)  // prints 0
)

Unlike Racket, imported bindings must accessed using a prefix name and
then @litchar{.}, at least by default. The prefix is inferred from a module
path by taking its last component and removing any extension, so
that’s why the import of @rhombus("f2c.rhm") leads to the @rhombus(f2c) prefix. To
supply an explicit prefix, use the @rhombus(as, ~impmod) modifier:

@rhombusblock(
  import:
    "f2c.rhm" as convert

  convert.fahrenheit_to_celsius(convert.fahrenheit_freezing)
)

Use the @rhombus(open, ~impmod) modifier to import without a prefix, but
this kind of ``namespace dumping'' is considered bad style in most
cases:

@rhombusblock(
  import:
    "f2c.rhm" open

  fahrenheit_to_celsius(fahrenheit_freezing)
)

Module paths are written with a @rhombus(/, ~impmod) separator as in Racket, and the
last path element is the one that determines the default import
prefix.

@rhombusblock(
  import:
    rhombus/math

  math.pi  // prints 3.141592653589793
)

@aside{The use of @litchar{.} with an import name as a hierarchical reference is not
the same as the @rhombus(.) operator described in the next section. We stick with
@rhombus(/, ~impmod) for module paths to avoid overloading @litchar{.} further.}

Unlike Racket, the default file suffix for unquoted module paths is
@filepath{.rhm}. To reference a Racket module, use a
@rhombus(lib, ~impmod) path with a @filepath{.rkt} suffix.

@rhombusblock(
  import:
    lib("racket/math.rkt")

  math.pi  // prints 3.141592653589793
)

There’s a lot more to the syntax or @rhombus(import) and
@rhombus(export) for renaming, re-exporting, and so on. See the
documentation of @rhombus(import) and @rhombus(export) for more
information.

For examples in the remainder of this overiew, we will mostly not write
modules explicitly. Examples will sometimes show definitions, which
meant as part of some implicit module, which interactive examples with
are shown with a leading @litchar{> } prompt and the expected result.

@demo(
  ~defn:
    def fahrenheit_freezing = 32
    fun fahrenheit_to_celsius(f):
      (f - 32) * 5/9
  ~repl:
    fahrenheit_to_celsius(fahrenheit_freezing)
)
