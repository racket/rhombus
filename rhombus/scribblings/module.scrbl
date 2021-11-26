#lang scribble/rhombus/manual
@(import:
    "util.rhm": no_prefix    
    "common.rhm": no_prefix)

@title{Modules}

A Rhombus module, which implements a program or a library, always
starts @litchar{#lang rhombus}. If you write an expression at the top of a
module, then its value gets printed out.

@(rhombusblock:
    #lang rhombus

    1+4  // prints 5
    
    "Hello, world!"  // prints "Hello, world!", including the quotes
  )

The ways to define names in a module include @rhombus[val] and @rhombus[fun].
The @rhombus[val] form defines an immutable variable, and it expects an
identifier to define followed by a block. The @rhombus[fun] form defines
a function when it see an identifier, parentheses, and then a block.

@(rhombusblock:
    #lang rhombus

    val fahrenheit_freezing: 32
                             
    fun fahrenheit_to_celsius(f):
      (f - 32) * 5/9

    fahrenheit_to_celsius(fahrenheit_freezing)  // prints 0
    )

@aside{If you have installed the @pkg{shrubbery-rhombus-0} package, then
 the interactions window in DrRacket will work to call
 @rhombus[fahrenheit_to_celsius]. In interactions, a single input line is
 accepted as complete as long as it's openers and closers are balanced,
 and as long as it doesn't contain @litchar{:} or @litchar{;} outside of
 an opener--closer pair. A blank line terminates multi-line input. For
 multi-line input where the first line would otherwise parse as complete,
 add @litchar{:} or @litchar{;} at the front, either on the same line or
 its own line.}

A Rhombus module can export definitions to other modules using
@rhombus[export], and it can import other modules using
@rhombus[import]. The @litchar{#lang rhombus} line is a kind of
@rhombus[import] already, so normally more @rhombus[import]s are written
at the top of a module, and then @rhombus[export]s, and then the
definitions.

@(rhombusblock:
    // f2c.rhm
    #lang rhombus

    export:
      fahrenheit_freezing
      fahrenheit_to_celsius

    val fahrenheit_freezing: 32

    fun fahrenheit_to_celsius(f):
      (f - 32) * 5/9)

@(rhombusblock:
    // freezing.rhm
    #lang rhombus

    import:
      "f2c.rhm"

    f2c.fahrenheit_to_celsius(f2cfahrenheit_freezing)  // prints 0
  )

Unlike Racket, imported bindings must accessed using a prefix name and
then @litchar{.}, at least by default. The prefix is inferred from a module
path by taking its last component and removing any extension, so
that’s why the import of @rhombus["f2c.rhm"] leads to the @rhombus[f2c] prefix. To
supply an explicit prefix, use the @rhombus[prefix] modifier:

@(rhombusblock:
    import:
      "f2c.rhm": prefix convert

    convert.fahrenheit_to_celsius(convert.fahrenheit_freezing))

Use the @rhombus[no_prefix] modifier to import without a prefix, but
this kind of ``namespace dumping'' is considered bad style in most
cases:

@(rhombusblock:
    import:
      "f2c.rhm": no_prefix

    fahrenheit_to_celsius(fahrenheit_freezing))

Module paths are written with a @litchar{/} separator as in Racket, and the
last path element is the one that determines the default import
prefix:

@(rhombusblock:
    import:
      racket/math

    math.pi  // prints 3.141592653589793
    )

@aside{The use of @litchar{.} with an import name as a hierarchical reference is not
the same as the @rhombus[.] operator described in the next section. We stick with @litchar{/}
for module paths to avoid overloading @litchar{.} further.}

There’s a lot more to the syntax or @rhombus[import] and @rhombus[export] for
renaming, re-exporting, and so on. See [a separate
document](import-export.md) for more information.
