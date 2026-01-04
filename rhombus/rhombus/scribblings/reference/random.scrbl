#lang rhombus/scribble/manual
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
    :: Int.in(0 .. n)
  method (prng :: Random).random(start :: Int, end :: Int)
    :: Int.in(start .. end)
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

 Satisfied by an array of 6 integers where the first three integers
 are @rhombus(Int.in(0 ..= 4294967086), ~annot) and the last three
 integers are @rhombus(Int.in(0 ..= 4294944442), ~annot). At least
 one of the first three integers must be non-zero and at least one
 of the last three integers must also be non-zero.

}

@doc(
  fun shuffle(lst :: List, rnd :: Random = Random.current())
    :: List
){

 Returns a list with the same elements as @rhombus(lst), but in a random
 order, where @rhombus(rnd) is used for randomization.

@examples(
  ~hidden:
    import rhombus/random open
    // make result deterministic
    Random.current(Random(Array(1, 2, 3, 4, 5, 6)))
  ~repl:
    shuffle([1, 2, 3, 4])
)

}
