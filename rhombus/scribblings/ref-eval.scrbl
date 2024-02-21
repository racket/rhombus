#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Eval}

An @deftech{evaluator} is an interactive evaluation context. Each
evaluator has its own set of bindings and, potentially, its own set of
module declarations. A Rhombus read-eval-print loop (REPL) is backed
by an evaluator, for example.

@doc(
  fun eval(seq :: Syntax,
           ~as_interaction: as_interaction :: Any = #false)
){

 Evaluates a term, group, or multi-group sequence @rhombus(seq) in the
 current @tech{evaluator}.

 If @rhombus(as_interaction) is true, then @rhombus(seq) is evaluated as
 if in a read-eval-print loop. Depending on the bindings of the current
 evaluator (as determined by @rhombus(Evaluator.current)), that can be
 different that evaluating non-interaction top-level forms.

 A suitable evalutor must have been created and installed as the value
 of the @rhombus(Evaluator.current) context parameter, since the default
 evaluator is empty.

@examples(
  parameterize { Evaluator.current: Evaluator.make_rhombus() }:
    eval('1 + 2')
)

}

@doc(
  annot.macro 'Evaluator'
){

 Represents an evaluator for interactive evaluation via @rhombus(eval).
 An evaluator is installed as the current one using
 @rhombus(Evaluator.current), usually via @rhombus(parameterize).

}

@doc(
  fun Evaluator.make_rhombus() :: Evaluator
){

 Creates a fresh evaluator with @rhombuslangname(rhombus) imported.

}

@doc(
  fun Evaluator.make_rhombus_empty() :: Evaluator
){

 Creates a fresh evaluator with the @rhombuslangname(rhombus) module
 attached, but not imported.

}

@doc(
  fun Evaluator.import(mod :: ModulePath) :: Void
){

 Imports @rhombus(mod) into the current evaluator, even if the @rhombus(import)
 definition form is not available in the evaluator itself.

}

@doc(
  Parameter.def Evaluator.current :: Evaluator
){

 A @tech{context parameter} for the current evaluator.

}
