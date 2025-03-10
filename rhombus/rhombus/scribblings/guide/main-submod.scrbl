#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "main-submod"){Using @rhombus(main, ~datum) and @rhombus(test, ~datum) Submodules}

Rhombus makes no syntactic distinction between a module that is intended
as a library (to @rhombus(import) into other modules) and the main
module of a program. Sometimes, though, it's convenient to use the same
module for both purposes, but have additional actions taken when a
module is run as the main module. When DrRacket, @exec{rhombus}, or
@exec{racket} runs a Rhombus module, it looks for a submodule named
@rhombus(main, ~datum) and loads that module if present.

For example, this module exports a @rhombus(greeting) function for use
in other modules, but if it is run directly, then it calls
@rhombus(greeting) and (implicitly) prints the result:

@rhombusblock(
  #,(hash_lang()) #,(@rhombuslangname(rhombus))

  export:
    greeting

  fun greeting():
    "hello"

  module main:
    greeting()
)

In addition, DrRacket looks for a @rhombus(test, ~datum) submodule and
runs that, too, which makes @rhombus(test, ~datum) useful to hold unit
tests that should run while the module is being developed, but not when
the module is deployed as a library or main program.

To run tests for @filepath{prog.rhm} on the command line, either name
the @rhombus(test, ~datum) submodule directly with

@margin_note_block{The @litchar{!} is quoted in this example, because
 @litchar{!} is a special character in most command-line shells. Your
 shell and its quoting form may vary.}

@nested(~style: #'inset){@exec{racket prog.rhm'!'test}}

or use Racket's @exec{raco test} tool as

@nested(~style: #'inset){@exec{raco test prog.rhm}}
