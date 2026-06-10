#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Module Syntax}

The @hash_lang() syntax works only at the start of a Rhombus module in a
file. For defining modules in other contexts, Rhombus provides a
@rhombus(module) form.

@// ----------------------------------------------------------------------
@section(~tag: "module-syntax-form"){The @rhombus(module, ~decl) Form}

A module that is not in its own file can be written as

@rhombusblock(
  module #,(@rhombus(name_id, ~var)) ~lang #,(@rhombus(initial_module_path, ~var)):
    #,(@rhombus(decl, ~var))
    #,(more_args)
)

where the @rhombus(name_id, ~var) is a name for the module,
@rhombus(initial_module_path, ~var) is an initial import, and each
@rhombus(decl, ~var) is an import, export, definition, or expression.

The @rhombus(initial_module_path, ~var) is needed because even the
@rhombus(import) form must be imported for further use in the module
body. In other words, the @rhombus(initial_module_path, ~var) import
bootstraps the syntax that is available in the body. The most commonly
used @rhombus(initial_module_path, ~var) is @rhombuslangname(rhombus)
(or @rhombuslangname(rhombus/static)), which supplies most of the
bindings described in this guide, including @rhombus(import),
@rhombus(def), and @rhombus(export).

For example, the @filepath{cake.rhm} example of the
@seclink("module-basics"){previous section} could be written within
another module or even in a @tech{REPL} as@margin_note{To write this in
 the Rhombus @tech{REPL}, it's easiest to remove the blank lines.}

@rhombusblock(
  module cake ~lang #,(@rhombuslangname(rhombus)):
    export:
      print_cake

    fun print_cake(n :: Int):
      show("   ", n, Char".", "   ")
      show(" .-", n, Char"|", "-. ")
      show(" | ", n, Char" ", " | ")
      show("---", n, Char"-", "---")

    fun show(pre, n, ch, post):
      println(pre ++ String.make(n, ch) ++ post)
)

This @rhombus(cake, ~datum) module is not associated with any file.
To refer to such an unassociated module, use the @rhombus(self, ~impo)
module path with @rhombus(!, ~impo):

@rhombusblock(
  import self!cake open
  print_cake(3)
)

Declaring a module does not immediately evaluate the body definitions
and expressions of the module. The module must be explicitly
@rhombus(import)ed at the top level to trigger evaluation. After
evaluation is triggered once, later @rhombus(import)s do not
re-evaluate the module body.

@rhombusblock(
  module hi ~lang #,(@rhombuslangname(rhombus)):
    println("Hello")
  import self!hi  // prints "Hello"
  import self!hi  // does not print again
)

Language names after @hash_lang() other than @rhombuslangname(rhombus)
can have a different syntax for the module body, such as for
@rhombus(#,(@hash_lang()) #,(@racketmodname(racket))). For languages in
the Rhombus family, however, a module that is documented as a
``language'' using the @hash_lang() notation will typically not only
work with @rhombus(module, ~decl) plus @rhombus(~lang), they will work
with @rhombus(import) to import the language's bindings into a module
that is implemented with a different language.

@// ----------------------------------------------------------------------
@section(~tag: "submodules"){Submodules}

A @rhombus(module, ~decl) form can be nested within a module, in which
case the nested @rhombus(module, ~decl) form declares a
@deftech{submodule}. Submodules can be referenced directly by the
enclosing module using @rhombus(self, ~impo) and @rhombus(!, ~impo).
The following example prints @rhombus("Tony") by importing
@rhombus(tiger) from the @rhombus(zoo, ~datum) submodule:

@filebox(
  "park.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    module zoo ~lang #,(@rhombuslangname(rhombus)):
      export:
        tiger
      def tiger = "Tony"

    import self!zoo open

    println(tiger)
  )
)

Running a module does not necessarily run its submodules. In the above
example, running @filepath{park.rhm} runs its submodule
@rhombus(zoo, ~datum) only because the @filepath{park.rhm} module
@rhombus(import)s the @rhombus(zoo, ~datum) submodule. Otherwise, a
module and each of its submodules can be run independently.
Furthermore, if @filepath{park.rhm} is compiled to a bytecode file
(via @exec{raco make}), then the code for @filepath{park.rhm} or the
code for @rhombus(zoo, ~datum) can be loaded independently.

Submodules can be nested within submodules, and a submodule can be
referenced directly by a module other than its enclosing module by
chaining @rhombus(!, ~impo) in a module path.

A @rhombus(module, ~decl) form @emph{without} @rhombus(~lang) is
similar to a nested @rhombus(module, ~decl) form, but it inverts the
possibilities for reference between the submodule and enclosing
module:

@itemlist(

 @item{A submodule declared with @rhombus(module name ~lang #,(@rhombus(lang_path, ~var)))
       can be @rhombus(import)ed by its enclosing module, but the
       submodule cannot @rhombus(import) the enclosing module or
       lexically reference the enclosing module's bindings.}

 @item{A submodule declared with @rhombus(module name) (without
       @rhombus(~lang)) inherits the enclosing module's bindings and
       can @rhombus(import) the enclosing module's bindings, but the
       enclosing module cannot @rhombus(import) the submodule.}

)

A @rhombus(module, ~decl) form without @rhombus(~lang) sees all of
the enclosing module's bindings---including bindings that are not
exported via @rhombus(export).

One use of submodules declared with @rhombus(module, ~decl) without
@rhombus(~lang) is to export additional bindings through a submodule
that are not normally exported from the module:

@filebox(
  "cake.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    export:
      print_cake

    fun print_cake(n :: Int):
      layer("   ", n, Char".", "   ")
      layer(" .-", n, Char"|", "-. ")
      layer(" | ", n, Char" ", " | ")
      layer("---", n, Char"-", "---")

    fun layer(pre, n, ch, post):
      println(pre ++ String.make(n, ch) ++ post)

    module extras:
      export:
        layer
  )
)

In this revised @filepath{cake.rhm} module, @rhombus(layer) is not
imported by a module that uses @rhombus(import: "cake.rhm"), since
most clients of @filepath{cake.rhm} will not want the extra function.
A module can import the @rhombus(extras, ~datum) @tech{submodule}
using @rhombus(import: "cake.rhm"!extras) to access the otherwise
hidden @rhombus(layer) function.

@// ----------------------------------------------------------------------
@section(~tag: "main-and-test"){Main and Test Submodules}

The following variant of @filepath{cake.rhm} includes a
@rhombus(main, ~datum) submodule that calls @rhombus(print_cake):

@filebox(
  "cake.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    fun print_cake(n :: Int):
      layer("   ", n, Char".", "   ")
      layer(" .-", n, Char"|", "-. ")
      layer(" | ", n, Char" ", " | ")
      layer("---", n, Char"-", "---")

    fun layer(pre, n, ch, post):
      println(pre ++ String.make(n, ch) ++ post)

    module main:
      print_cake(10)
  )
)

Running a module does not run its non-@rhombus(~lang) submodules.
Nevertheless, running the above module via @exec{rhombus},
@exec{racket}, or DrRacket prints a cake with 10 candles, because the
@rhombus(main, ~datum) @tech{submodule} is a special case.

When a module is provided as a program name to the @exec{rhombus} or
@exec{racket} executable or run directly within DrRacket, if the
module has a @as_indexed{@rhombus(main, ~datum) submodule}, the
@rhombus(main, ~datum) submodule is run after its enclosing module.
Declaring a @rhombus(main, ~datum) submodule thus specifies extra
actions to be performed when a module is run directly, instead of
@rhombus(import)ed as a library within a larger program.

A @rhombus(main, ~datum) submodule does not have to be declared
without @rhombus(~lang). If the @rhombus(main, ~datum) module does not
need to use bindings from its enclosing module, it can be declared
with @rhombus(~lang) and an explicit language. More commonly,
@rhombus(main, ~datum) is declared without @rhombus(~lang), so the
submodule's body sees the enclosing module's bindings.

In addition, multiple @rhombus(module, ~decl) forms (without
@rhombus(~lang)) can specify the same submodule name, in which case
the bodies of the @rhombus(module, ~decl) forms are combined to create
a single submodule.

The combining behavior of repeated @rhombus(module, ~decl) is
particularly useful for defining a @rhombus(test, ~datum) submodule,
which can be conveniently run using
@seclink("exe", ~doc: raco_doc, ~indirect: #true){@exec{raco test}} in
much the same way that @rhombus(main, ~datum) is conveniently run with
@exec{rhombus} or @exec{racket}. For example, the following
@filepath{physics.rhm} module exports @rhombus(drop) and
@rhombus(to_energy) functions, and it defines a @rhombus(test, ~datum)
module to hold unit tests:

@filebox(
  "physics.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    module test:
      def #,(epsilon) = 1e-10

    export:
      drop
      to_energy

    fun drop(t):
      1/2 * 9.8 * t * t

    module test:
      check:
        drop(0)
        ~is_approx 0 ~within #,(epsilon)
      check:
        drop(10)
        ~is_approx 490 ~within #,(epsilon)

    fun to_energy(m):
      m * math.expt(299792458.0, 2)

    module test:
      check:
        to_energy(0)
        ~is_approx 0 ~within #,(epsilon)
      check:
        to_energy(1)
        ~is_approx 9e+16 ~within 1e+15
  )
)

Importing @filepath{physics.rhm} into a larger program does not run
the @rhombus(drop) and @rhombus(to_energy) tests---or even trigger
the loading of the test code, if the module is compiled---but running
@exec{raco test physics.rhm} at a command line runs the tests.

The combining behavior of repeated @rhombus(module, ~decl) is also
sometimes helpful for a @rhombus(main, ~datum) module.
