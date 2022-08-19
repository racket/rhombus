#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Eval}

@doc(
  fun eval(seq :: Syntax)
){

  Evaluates a term, group, or multi-group sequence @rhombus(seq) in the
  current toplevel.

}


@doc(
  fun make_rhombus_toplevel() :: Toplevel
){

 Creates a fresh toplevel with @rhombusmodname(rhombus) imported.

}

@doc(
  fun make_rhombus_empty_toplevel() :: Toplevel
){

 Creates a fresh toplevel with the @rhombusmodname(rhombus) module
 attached, but not imported.

}

@doc(
  fun current_toplevel() :: Toplevel,
  fun current_toplevel(ns :: Toplevel) :: #!void
){

 A @tech{parameter} for the current toplevel.

}
