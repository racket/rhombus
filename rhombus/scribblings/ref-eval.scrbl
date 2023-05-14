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
  fun make_rhombus_evaluator() :: Evaluator
){

 Creates a fresh evaluator with @rhombusmodname(rhombus) imported.

}

@doc(
  fun make_rhombus_empty_evaluator() :: Evaluator
){

 Creates a fresh evaluator with the @rhombusmodname(rhombus) module
 attached, but not imported.

}

@doc(
  fun current_evaluator() :: Evaluator,
  fun current_evaluator(ns :: Evaluator) :: #void
){

 A @tech{parameter} for the current evaluator.

}
