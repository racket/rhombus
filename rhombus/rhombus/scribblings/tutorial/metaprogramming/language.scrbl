#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/bnf)

@title(~tag: "language"){Whole Languages}

Throughout the tutorial, we have appealed to predefined elements of
Rhombus as examples of the kind of extensibility that Rhombus enables.
The difference between Rhombus functionality and the kinds of macros
that we have written is that Rhombus forms are packaged into a language
that you can use with @hash_lang().

Packaging a set of macros into a form for use with @hash_lang() in the
same way as @rhombuslangname(rhombus) requires two steps:

@itemlist(

 @item{defining a module that exports all of the bindings that are in
 the language, including @seclink("implicit"){implicit forms}; and}

 @item{making the module part of your Racket installation, typically
 through a package, since the name after @hash_lang() is resolved to a
 implementing module through the installation.}

)

We'll consider the first step here, but we'll leave details of the second step
to @seclink("lang", ~doc: rhombus_doc){the Rhombus documentation}.

@section{Modules and Macros}

Each Rhombus source file that starts with
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus))) defines a
@deftech{module}. A module can export bindings for use in other modules
using @rhombus(export, ~decl), and other modules reference the exporting
module using @rhombus(import, ~decl). In the case of modules that are
adjacent in the filesystem, they can refer to each other via
relative-path strings.

For example, if @filepath{fib.rhm} contains

@filebox(
  "fib.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    export:
      fib

    fun fib(n):
      match n
      | 0 || 1: 1
      | n: fib(n-1) + fib(n-2)
  )
)

then @filepath{main.rhm} in the same directory can use
@filepath{fib.rhm} like this:

@filebox(
  "main.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    import:
      "fib.rhm" open

    fib(5)
  )
)

Macros can be exported just the same as functions. For example, if you
start with the solution @local_file("my_operator_soln.rhm") to an
@seclink("ex-operator"){earlier exercise}, then you can use
@rhombus(my_operator) as exported from that module:

@filebox(
  "main.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    import:
      "my_operator_soln.rhm" open

    my_operator a <!!!> b:
      [a && b, a || b]

    #true <!!!> #false
  )
)

The @rhombus(my_operator) macro expands to a use of @rhombus(expr.macro),
which is not directly accessible in @filepath{main.rhm}, since it does
not import @rhombusmodname(rhombus/meta) or use
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/and_meta))). the
import @rhombus(my_operator) macro works, anyway, because syntax objects
retain scope information and enable @deftech{hygienic} macros.

@section{Modules and Spaces}

Suppose that we want to use @rhombus(interp) and @rhombus(prog) of
@local_file("interp_space.rhm") (from an @seclink("ex-implicit"){earlier
 exercise}) in @filepath{main.rhm}:

@filebox(
  "main.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    import:
      "interp_space.rhm" open

    interp(prog: 1 + 2,
           {})
  )
)

Clearly, we'll need to adjust @filepath{interp_space.rhm} to export
@rhombus(interp) and @rhombus(prog):

@rhombusblock(
  export:
    interp
    prog
)

This turns out not to be enough, however. After adding this
@rhombus(export), attempting to run @filepath{main.rhm} reports an error
about a missing @rhombus(#%literal) for the @rhombus(1) in
@rhombus(prog: 1 + 2). The missing @rhombus(#%literal) is in the one in
the @rhombus(lc) space. It will turn out that the @rhombus(+) for
@rhombus(lc) is also not available.

To fix the problem, put an additional @rhombus(export) form after the
definition of the @rhombus(lc) space @filepath{interp_space.rhm} (about
2/3 of the way down from the top of the file):

@rhombusblock(
  export:
    only_space lc:
      +
      ==
      let
      fun
      #%call
      #%parens
      #%literal      
)

This example illustrates how Rhombus provides fine-grained control over
the bindings that are accessible from a module. To be able to import
these bindings, however, we needed at least @rhombus(import) from the
@rhombuslangname(rhombus) language. By creating a module that can be
referenced through a @hash_lang() line, we can create a language that is
@emph{smaller} than @rhombuslangname(rhombus).

@section{Modules and Languages}

We will not be able to write
@rhombus(#,(@hash_lang()) "interp_space.rhm"), because quoted relative
paths are not allowed after @hash_lang(). Fortunately, the
@rhombuslangname(shrubbery) language can give us a little help. When
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(shrubbery))) is by itself on a line,
then it works the way we used in an @seclink("ex-shrubbery"){earlier exercise}.
But when @rhombuslangname(shrubbery) is followed by a quoted path as in
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(shrubbery)) "interp_space.rhm"),
then it imports the path for use in the module body, instead of
just printing the body's parsed shrubbery form.

Our goal is to make this @filepath{main.rhm} module work by using the
@rhombus(prog) parser and @rhombus(interp) function from
@filepath{interp_space.rhm} instead of compiling it as Rhombus code.
@margin_note{Again, writing an interpreter is not really the best way to
 create a language in Rhombus. You should just compile your language
 directly to Rhombus code via macros. We continue to use the interpreter
 example here, anyway.}

@filebox(
  "main.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(shrubbery)) "interp_space.rhm"

    let x = 1:
      let f = (fun (y): y + 2):
        f(x) + 3
  )
)

Attempting to run this new @filepath{main.rhm} will fail, however, with
an error message about a missing @tt{#%module-begin} binding. As the
@litchar{#%} prefix suggests, @tt{#%module-begin} is an implicit that
wraps a module body. It turns out that @tt{#%module-begin} is from the
Racket layer of Rhombus, and it works in Racket-native terms. Rhombus
has its own @rhombus(#%module_block) protocol that works in
Rhombus-native terms, and Rhombus exports a @tt{#%module-begin} that
bridges to @rhombus(#%module_block). The Rhombus spelling of the Racket
@tt{#%module-begin} identifier (which has a hyphen) is
@rhombus(#{#%module-begin}).

So, to make the new @filepath{main.rhm} work, we need to change
@filepath{interp_space.rhm} to

@itemlist(

 @item{re-export @rhombus(#{#%module-begin}) from
 @rhombuslangname(rhombus); and}

 @item{define and export a @rhombus(#%module_block) declaration form
 that expands to use @rhombus(interp) and @rhombus(prog).}

)

Here's the @rhombus(decl.macro) and @rhombus(export) form to add to
@filepath{interp_space.rhm}:

@rhombusblock(
  export:
    #{#%module-begin}
    rename:
      my_module_block as #%module_block

  decl.macro 'my_module_block:
                $body':
    '#%module_block:
       interp(prog: $body,
              {})'
)

The exported @rhombus(#%module_block, ~datum) macro is defined as
@rhombus(my_module_block) so that it can expand to a use of the Rhombus
@rhombus(#%module_block, ~decl) form, and then it is renamed to
@rhombus(#%module_block, ~datum) on export.

@section(~tag: "ex-lang"){Exercise}

Change the @rhombus(my_module_block) form in @filepath{interp_space.rhm}
so that it allows multiple @bnf.nt{group}s in the module body, and it
independently @rhombus(interp)s and prints a result for each of them.

Solution: @local_file("interp_space_lang_soln.rhm").
