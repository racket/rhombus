#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title(~tag: "Modules"){Modules, Variables, and Functions}

A Rhombus module, which implements a program or a library, always starts
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus))). If you write an
expression at the top of a module, then its value gets printed out.

@rhombusblock(
  #,(hash_lang()) #,(@rhombuslangname(rhombus))

  1+4  // prints 5

  "Hello, world!"  // prints "Hello, world!", including the quotes
)

@margin_note{If you have installed the @pkg{rhombus-prototype} package, then
 you can run Rhombus modules in DrRacket or by supplying the file path to
 @exec{racket} on the command line.}

Ways to define names in a module include @rhombus(def) and
@rhombus(fun). The @rhombus(def) form defines an immutable variable, and
it expects an identifier to define followed by either @rhombus(=) or a block. The
@rhombus(fun) form defines a function when followed by an identifier,
parentheses around argument names, and then a block. Function calls have
the usual shape: a function name (or, more generally, an expression that
produces a function) followed by comma-separated arguments in
parentheses.

@rhombusblock(
  #,(hash_lang()) #,(@rhombuslangname(rhombus))

  def fahrenheit_freezing = 32

  fun fahrenheit_to_celsius(f):
    (f - 32) * 5/9

  fahrenheit_to_celsius(fahrenheit_freezing)  // prints 0
)

@margin_note{In DrRacket's interactions area, a single input line is
 accepted as complete as long as it's openers and closers are balanced,
 and as long as it doesn't contain @litchar{:} or @litchar{;} outside of
 an opener--closer pair. A blank line terminates multi-line input. For
 multi-line input where the first line would otherwise parse as complete,
 add @litchar{:} or @litchar{;} at the front, either on the same line or
 its own line.

 To get a Rhombus read-eval-print loop on the
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
@rhombus(import). The
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus))) line is a kind
of @rhombus(import) already, so normally more @rhombus(import)s are
written at the top of a module, and then @rhombus(export)s, and then the
definitions.

@rhombusblock(
  // f2c.rhm
  #,(hash_lang()) #,(@rhombuslangname(rhombus))

  export:
    fahrenheit_freezing
    fahrenheit_to_celsius

  def fahrenheit_freezing = 32

  fun fahrenheit_to_celsius(f):
    (f - 32) * 5/9
)

@rhombusblock(
  // freezing.rhm
  #,(hash_lang()) #,(@rhombuslangname(rhombus))

  import:
    "f2c.rhm"

  f2c.fahrenheit_to_celsius(f2cfahrenheit_freezing)  // prints 0
)

Refer to imported bindings using a prefix name and
then @litchar{.}. The prefix is inferred from a module
path by taking its last component and removing any extension, so
that’s why the import of @rhombus("f2c.rhm") leads to the @rhombus(f2c) prefix. To
supply an explicit prefix, use the @rhombus(as, ~impo) modifier:

@rhombusblock(
  import:
    "f2c.rhm" as convert

  convert.fahrenheit_to_celsius(convert.fahrenheit_freezing)
)

Use the @rhombus(open, ~impo) modifier to import without a prefix---but
this kind of ``namespace dumping'' is considered bad style in most
cases:

@rhombusblock(
  import:
    "f2c.rhm" open

  fahrenheit_to_celsius(fahrenheit_freezing)
)

Module paths for installed libraries are written with a
@rhombus(/, ~impo) separator, and the last path element is the one that
determines the default import prefix.

@rhombusblock(
  import:
    rhombus/math

  math.pi  // prints 3.141592653589793
)

@margin_note{Technically, the use of @litchar{.} with an import name as a hierarchical reference is not
the same as the @rhombus(.) operator described in the next section. We stick with
@rhombus(/, ~impo) for module paths to avoid overloading @litchar{.} further.}

The default file suffix for unquoted module paths is
@filepath{.rhm}. To reference a Racket module, use a
@rhombus(lib, ~impo) path with a @filepath{.rkt} suffix.

@rhombusblock(
  import:
    lib("racket/math.rkt")

  math.pi  // prints 3.141592653589793
)

There’s a lot more to the syntax of @rhombus(import) and
@rhombus(export) for renaming, re-exporting, and so on. See the
documentation of @rhombus(import) and @rhombus(export) for more
information.

For examples in most of this guide, we will mostly not write
modules explicitly. Examples will sometimes show definitions, which are
meant as part of some implicit module, with interactive examples
shown with a leading @litchar{> } prompt and the expected result.

@examples(
  ~defn:
    def fahrenheit_freezing = 32
    fun fahrenheit_to_celsius(f):
      (f - 32) * 5/9
  ~repl:
    fahrenheit_to_celsius(fahrenheit_freezing)
)
