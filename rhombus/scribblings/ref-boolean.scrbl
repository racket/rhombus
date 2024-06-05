#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Booleans}

@doc(
  annot.macro 'Boolean'
){

  Matches @rhombus(#true) or @rhombus(#false).

}

@doc(
  annot.macro 'False'
  annot.macro 'True'
){

  Matches only @rhombus(#false) and values other than @rhombus(#false), respectively.

@examples(
  #false is_a False
  #false is_a True
  42 is_a True
)

}


@doc(
  ~nonterminal:
    left_expr: block expr
    right_expr: block expr
    left_repet: block repet
    right_repet: block repet
  expr.macro '$left_expr || $right_expr'
  repet.macro '$left_repet || $right_repet'
){

 Produces the value of @rhombus(left_expr) if it is
 non-@rhombus(#false), otherwise produces the value(s) of
 @rhombus(right_expr). The @rhombus(right_expr) is evaluated in tail
 position with respect to the @rhombus(||) form, if evaluated at all.

 The @rhombus(||) form can also serve as @tech{repetitions}.

}

@doc(
  ~nonterminal:
    left_bind: def bind ~defn
    right_bind: def bind ~defn
  bind.macro '$left_bind || $right_bind'
){

 Matches if either @rhombus(left_bind) or @rhombus(right_bind) matches.
 No identifiers are bound after a successful match, however. In other
 words, @rhombus(left_bind) and @rhombus(right_bind) are used only in
 matching mode, and implied conversions might be skipped.

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
  ~nonterminal:
    left_annot: :: annot
    right_annot: :: annot
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
  reducer.macro 'any'
){

 A @tech{reducer} that stops an iteration as soon as a non-@rhombus(#false)
 value is produced for an element and returns that value, otherwise
 returns @rhombus(#false).

@examples(
  for any (i: 0..10):
    i == 5 && to_string(i)
  for any (i: 0..10):
    i == 10
)

}


@doc(
  ~nonterminal:
    left_expr: block expr
    right_expr: block expr
    left_repet: block repet
    right_repet: block repet
  expr.macro '$left_expr && $right_expr'
  repet.macro '$left_repet && $right_repet'
){

 Produces @rhombus(#false) if the value of @rhombus(left_expr) is
 @rhombus(#false), otherwise produces the value(s) of
 @rhombus(right_expr). The @rhombus(right_expr) is evaluated in tail
 position with respect to the @rhombus(&&) form, if evaluated at all.

 The @rhombus(&&) form can also serve as @tech{repetitions}.

}

@doc(
  ~nonterminal:
    left_bind: def bind ~defn
    right_bind: def bind ~defn
  bind.macro '$left_bind && $right_bind'
){

 Matches when both @rhombus(left_bind) and @rhombus(right_bind) match.
 All identifiers from bindings are available after the match, and
 static information from @rhombus(left_bind) is propagated to
 @rhombus(right_bind) (but not the other way around).

 See @rhombus(where, ~bind) for a different kind of ``and'' binding that
 allows the right-hand side to refer to bindings from the left-hand side.

@examples(
  class Posn(x, y)
  fun three_xs(v):
    match v
    | [_, _, _] && [Posn(x, _), ...]: [x, ...]
    | ~else: #false
  three_xs([Posn(1, 2), Posn(3, 4), Posn(5, 6)])
  three_xs([Posn(1, 2), Posn(3, 4)])
  three_xs([Posn(1, 2), Posn(3, 4), "no"])
)

}


@doc(
  ~nonterminal:
    left_annot: :: annot
    right_annot: :: annot
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
  Pair(1, "hello") is_a (Pair.of(Int, Any) && Pair.of(Any, String))
  1 :: (converting(fun (n): n+1) && converting(fun (n): -n))
)

}


@doc(
  reducer.macro 'all'
){

 A @tech{reducer} that stops an iteration as soon as a @rhombus(#false) value
 is produced for an element and otherwise returns the result of the last
 iteration.

@examples(
  for all (i: 0..10):
    i == 5
  for all (i: 0..10):
    i < 10 && to_string(i)
)

}


@doc(
  operator (! (v :: Any)) :: Boolean
){

 Returns @rhombus(#true) if @rhombus(v) is @rhombus(#false),
 @rhombus(#false) otherwise.

@examples(
  !#false
  !#true
  !"false"
)

}

@doc(
  bind.macro '! $bind'
){

 Matches if @rhombus(bind) does not match. Because @rhombus(bind) does
 not match, no identifiers are bound.

@examples(
  fun
  | is_two_list(![x, y]): #false
  | is_two_list(_): #true
  is_two_list([1])
  is_two_list([1, 2])
  is_two_list([1, 2, 3])
)

}

@doc(
  annot.macro '! $annot'
){

 Creates an annotation that accepts a value not satisfying
 @rhombus(annot). Because @rhombus(annot) is not satisfied, no
 conversion is performed.

@examples(
  [1, 2, 3] is_a !List
  PairList[1, 2, 3] is_a !List
)

}
