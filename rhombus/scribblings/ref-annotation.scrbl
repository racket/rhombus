#lang scribble/rhombus/manual
@(import: "common.rhm" open
          "nonterminal.rhm" open)

@title{Annotations}

@doc(
  expr.macro '$expr :: $annot'
){

 Checks that the value of @rhombus(expr) satisifies
 @rhombus(annot), and returns the value if so.

@examples(
  [1, 2, 3] :: List
)

}

@doc(
  bind.macro '$bind :: $annot'
){

 Binds the same as @rhombus(bind), but first checks that the value to
 be bound satisfies @rhombus(annot).

@examples(
  def x :: List: [1, 2, 3]
)

}

@doc(
  annot.macro 'Any'
  annot.macro 'Any.of($expr, ...)'
){

 The @rhombus(Any, ~annot) annotation matches any value. An
 @rhombus(Any.of, ~annot) annotaton matches any value that is equal (in
 the sense of @rhombus(==)) to one of the @rhombus(expr) results.

@examples(
  "hello" is_a Any
  "hello" is_a Any.of("hello", "goodbye")
  "hola" is_a Any.of("hello", "goodbye")
)

}

@doc(
  expr.macro '$expr :~ $annot'
){

 Associates static information to the overall expression the same as
 @rhombus(::), but performs no run-time check on the value of
 @rhombus(expr).

@examples(
  [1, 2, 3] :~ List
  "oops" :~ List
)

}

@doc(
  bind.macro '$bin :~ $annot'
){

 Associates static information to @rhombus(bind) the same as
 @rhombus(::, ~bind), but performs no run-time check.

@examples(
  def x :~ List: [1, 2, 3]
  def x :~ List: "oops"
)

}


@doc(
  expr.macro '$expr is_a $annot'
){

 Produces @rhombus(#true) if the value of @rhombus(expr)
 satisfies @rhombus(annot), @rhombus(#false) otherwise.

@examples(
  [1, 2, 3] is_a List
  "oops" is_a List
)

}

@doc(
  annot.macro 'matching($bind)'
){

 Converts @rhombus(bind) into an annotation. Variables bound in
 @rhombus($bind) are not made visible, but the annotation
 corresponds to the set of values for which @rhombus(bind) would
 match.

@examples(
  def x :: matching([_, 10]): [9, 10]
  ~error: def y :: matching([_, 10]): [9, 11]
)

}

@doc(
  annot.macro 'Maybe($annot)'
){

 An annotation that is satisfied by either @rhombus(#false) or a value
 that satisfies @rhombus(annot).

@examples(
  #false :: Maybe(String)
  "string" :: Maybe(String)
  ~error:
    #true :: Maybe(String)
)

}
