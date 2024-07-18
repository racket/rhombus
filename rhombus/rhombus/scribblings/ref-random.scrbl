#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/random open)

@title{Random Number Generation}

@docmodule(rhombus/random)

@doc(
  annot.macro 'Random'
  fun Random() :: Random
  fun Random(state :: RandomState) :: Random
){

 Annotation and constructors for a pseudo-random number generator (PRNG).

}

@doc(
  method (prng :: Random).random()
    :: Real.in(0 ~exclusive, 1 ~exclusive)
  method (prng :: Random).random(n :: PosInt)
    :: Int.in(0, n ~exclusive)
  method (prng :: Random).random(start :: Int, end :: Int)
    :: Int.in(start, end ~exclusive)
){

 Steps @rhombus(prng) to obtain a number.

 Using @rhombus(math.random) is the same as using
 @rhombus(Random.current())'s @rhombus(Random.random) method.

}

@doc(
  property
  | (prng :: Random).state :: RandomState
  | (prng :: Random).state := (s :: RandomState)
){

 A property for the state of @rhombus(prng).

}

@doc(
  Parameter.def Random.current :: Random = Random()
){

 A @tech{context parameter} for the pseudo-random number generator that is used by
 @rhombus(math.random).

}

@doc(
  annot.macro 'RandomState'
){

 Satisfied by an array of 6 values where the first three values are
 integers in the range @rhombus(0) to @rhombus(4294967086), inclusive,
 and the last three integers are in the range @rhombus(0) to
 @rhombus(4294944442), inclusive.

}
