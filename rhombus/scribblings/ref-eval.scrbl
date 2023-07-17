#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Eval}

An @deftech{evaluator} is an interactive evaluation context. Each
evaluator has its own set of bindings and, potentially, its own set of
module declarations. A Rhombus read-eval-print loop (REPL) is backed
by an evaluator, for example.

@doc(
  fun eval(seq :: Syntax)
){

  Evaluates a term, group, or multi-group sequence @rhombus(seq) in the
  current @tech{evaluator}.

}

@doc(
  annot.macro 'Evaluator'
){

 Represents an evaluator for inetarctive evaluation via @rhombus(eval).
 An evaluator is installed as the current one using
 @rhombus(current_evaluator), usually via @rhombus(parameterize).

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
  fun Evaluator.import(mod :: ModulePath) :: Evaluator
){

 Imports @rhombus(mod) into the evaluator, even if the @rhombus(import)
 definition form is not available in the evaluator itself.

}

@doc(
  def Evaluator.current :: Parameter
  fun Evaluator.current() :: Evaluator
  fun Evaluator.current(ns :: Evaluator) :: #void
){

 A @tech{context parameter} for the current evaluator.

}
