#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Booleans}

@doc(
  annot.macro 'Boolean'
){

  Matches @rhombus(#true) or @rhombus(#false).

}

@doc(
  annot.macro 'False'
){

  Matches only @rhombus(#false).

}

@doc(
  expr.macro '$expr || $expr'
){

 Produces the value of the first @rhombus(expr) if it is
 non-@rhombus(#false), otherwise produces the value(s) of the second
 @rhombus(expr).

 The second @rhombus(expr) is evaluated in tail position with respect to
 the @rhombus(||) form.

}

@doc(
  bind.macro '$left_bind || $right_bind'
){

 Matches if either @rhombus(left_bind) or @rhombus(right_bind) matches.
 No identifiers are bound after a successful match, however.

@examples(
  fun check_shape(v):
    match v
    | [x] || [x, y, z]: #true
    | ~else: #false
  check_shape([1])
  check_shape([1, 2, 3])
  check_shape([1, 2])
)

}

@doc(
  annot.macro '$left_annot || $right_annot'
){

 Creates an annotation that accepts a value satisfying either
 @rhombus(left_annot) or @rhombus(right_annot). The static information
 implied by the annotation is the intersection of information for
 @rhombus(left_annot) and @rhombus(right_annot).

 The annotations are checked in other. Either or both of
 @rhombus(left_annot) and @rhombus(right_annot) can be a @tech{converter
  annotation}, in which case the conversion result of the first satisified
 annotation is used.

@examples(
  1 is_a (String || Int)
  1 is_a (Boolean || Int)
)

}

@doc(
  reducer.macro '||'
){

 A @tech{reducer} that stops an iteration as soon as a non-@rhombus(#false)
 value is produced for an element and returns that value, otherwise
 returns @rhombus(#false).

@examples(
  for ||:
    each i: 0..10
    (i == 5) && to_string(i)
  for ||:
    each i: 0..10
    i == 10
)

}


@doc(
  expr.macro '$expr && $expr'
){

 Produces @rhombus(#false) if the the value of the first @rhombus(expr)
 is @rhombus(#false), otherwise produces the value(s) of the second
 @rhombus(expr).

 The second @rhombus(expr) is evaluated in tail position with respect to
 the @rhombus(&&) form.

}

@doc(
  bind.macro '$left_bind && $right_bind'
){

 Matches when both @rhombus(left_bind) and @rhombus(right_bind) match.
 All identifiers from bindings are available after the match, and
 static information from @rhombus(left_bind) is propagated to
 @rhombus(right_bind) (but not the other way around).

@examples(
  class Posn(x, y)
  fun three_xs(v):
    match v
    | [a, b, c] && [Posn(x, _), ...]:
        [x, ...]
    | ~else: #false
  three_xs([Posn(1, 2), Posn(3, 4), Posn(5, 6)])
  three_xs([Posn(1, 2), Posn(3, 4)])
  three_xs([Posn(1, 2), Posn(3, 4), "no"])
)

}


@doc(
  annot.macro '$left_annot && $right_annot'
){

 Creates an annotation that accepts a value satisfying both
 @rhombus(left_annot) and @rhombus(right_annot).

 When @rhombus(left_annot) and @rhombus(right_annot) are
 @tech{predicate annotations}, the static information
 implied by the annotation is the union of information for
 @rhombus(left_annot) and @rhombus(right_annot), where information
 from @rhombus(right_annot) takes precedence in cases where both
 supply values for the same static-information key.

 If @rhombus(left_annot) or @rhombus(right_annot) is a
 @tech{converter annotation}, the @rhombus(left_annot) conversion
 is applied first, and its result is the input to @rhombus(right_annot),
 and the result of @rhombus(right_annot) is the result for the
 for the overall annotation created by @rhombus(&&, ~annot).
 When the overall annotation is used only for matching, the conversion
 part of @rhombus(right_annot) is skipped, but the conversion part of
 @rhombus(left_annot) must be performed.
 
@examples(
  1 is_a (String && Int)
  Pair.cons(1, "hello") is_a (Pair.of(Int, Any) && Pair.of(Any, String))
  1 :: (converting(fun (n): n+1) && converting(fun (n): -n))
)

}


@doc(
  reducer.macro '&&'
){

 A @tech{reducer} that stops an iteration as soon as a @rhombus(#false) value
 is produced for an element and otherwise returns the result of the last
 iteration.

@examples(
  for &&:
    each i: 0..10
    i == 5
  for &&:
    each i: 0..10
    (i < 10) && to_string(i)
)

}



@doc(
  operator (! v) :: Boolean
){

 Returns @rhombus(#true) if @rhombus(v) is @rhombus(#false),
 @rhombus(#false) otherwise.


@examples(
  !#false
  !#true
  !"false"
)

}
