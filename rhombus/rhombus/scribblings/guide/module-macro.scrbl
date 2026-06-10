#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "module-macro"){Modules and Macros}

Rhombus's module system cooperates closely with Rhombus's macro system
(see @secref("expr-macro") and @secref("defn-macro")). For example, in
the same way that the @rhombuslangname(rhombus) module provides syntax
for @rhombus(import) and @rhombus(fun), importing other modules can
introduce new macro-based syntactic forms---in addition to more
traditional kinds of imports, such as functions or constants.

Macros that are defined within a module work the right way when they are
imported into another module. Here's a module that defines and exports
@rhombus(noisy_fun), but it does not export the @rhombus(show_arguments)
function:

@rhombusblock(
  module noisy ~lang #,(@rhombuslangname(rhombus)):
    import:
      rhombus/meta open

    export:
      noisy_fun

    defn.macro 'noisy_fun $id($arg, ...): $body; ...':
      'fun $id($arg, ...):
         show_arguments(#'$id, [$arg, ...])
         $body
         ...'

    fun show_arguments(name, args):
      println("calling " +& name +& " with arguments " +& args)
)

The @rhombus(noisy_fun) form is like @rhombus(fun, ~defn) for a function
definition, but it causes each call to the function to print the
arguments that are provided to the function. After importing the
@rhombus(noisy, ~datum) module, an example of using
@rhombus(noisy_fun) to define a function looks like this:

@rhombusblock(
  import self!noisy open

  noisy_fun f(x, y):
    x + y

  f(1, 2)  // prints "calling f with arguments [1, 2]"
)

Roughly, the @rhombus(noisy_fun) form works by replacing

@rhombusblock(
  noisy_fun f(x, y):
    x + y
)

with

@rhombusblock(
  fun f(x, y):
    show_arguments(#'f, [x, y])
    x + y
)

Since @rhombus(show_arguments) isn't exported by the
@rhombus(noisy, ~datum) module, however, this literal textual
replacement is not quite right. The actual replacement correctly
tracks the origin of identifiers like @rhombus(show_arguments), so
they can refer to other definitions in the place where the macro is
defined---even if those identifiers are not available at the place
where the macro is used.

There's more to the macro and module interaction than identifier
binding. The @rhombus(defn.macro) form is itself a macro, and it expands
to compile-time code that implements the transformation from
@rhombus(noisy_fun) into @rhombus(fun). The module system keeps track of
which code needs to run at compile time and which needs to run normally.
