#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Eval}

@doc(
  fun eval(seq :: Syntax)
){

  Evaluates a term, group, or multi-group sequence @rhombus(seq) in the
  current namespace.

}


@doc(
  fun make_rhombus_namespace() :: Namespace
){

 Creates a fresh namespace with @rhombusmodname(rhombus) imported.

}

@doc(
  fun make_rhombus_empty_namespace() :: Namespace
){

 Creates a fresh namespace with the @rhombusmodname(rhombus) module
 attached, but not imported.

}

@doc(
  fun current_namespace() :: Namespace,
  fun current_namespace(ns :: Namespace) :: #!void
){

 A @tech{parameter} for the current namespace.

}
