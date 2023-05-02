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

 If @rhombus(annot) is a @tech{converter annotation}, the conversion is
 applied before matching the converted value against @rhombus(bind). This
 dependency implies that the conversion in @rhombus(annot) cannot be
 delayed, and must be performed as part of the matching process (before
 committing to the match).

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
 @rhombus(expr). The @rhombus(annot) must specify a @tech{predicate
  annotation}.

@examples(
  [1, 2, 3] :~ List
  "oops" :~ List
)

}

@doc(
  bind.macro '$bin :~ $annot'
){

 Associates static information to @rhombus(bind) the same as
 @rhombus(::, ~bind), but performs no run-time check. The @rhombus(annot)
 must specify a @tech{predicate annotation}.

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

 If @rhombus(annot) is a @tech{converter annotation}, only the matching
 component of the annotation is used, and the converting part is not
 used. See also @secref("annotation-macro-protocol").

@examples(
  [1, 2, 3] is_a List
  "oops" is_a List
)

}

@doc(
  annot.macro 'matching($bind)'
){

 Converts @rhombus(bind) into an annotation. Variables bound in
 @rhombus($bind) are not made visible, but the annotation corresponds to
 the set of values for which @rhombus(bind) would match. Since no results
 are made visible, @rhombus(bind) is used only in matching mode, and
 implied conversions might be skipped.

@examples(
  def x :: matching([_, 10]): [9, 10]
  ~error: def y :: matching([_, 10]): [9, 11]
)

}

@doc(
  annot.macro 'converting(fun ($bind) $maybe_res_annot:
                            $body
                            ...)'
  grammar maybe_res_annot:
    #,(@rhombus(::, ~bind)) $annot
    #,(@rhombus(:~, ~bind)) $annot
    #,(epsilon)
){

 Produces a @tech{converter annotation} by pairing @rhombus(bind) with
 @rhombus(body). The annotation matches when @rhombus(bind) matches, but
 the value produced by the annotation is determined by the @rhombus(body)
 sequence, which can refer to variables bound by @rhombus(bind).

 When @rhombus(annot) is provided, then it's static information is
 propoagted to the new converter annotation. If @rhombus(annot) is
 supplied with @rhombus(::, ~bind), then the result of the @rhombus(body)
 sequence is checked against @rhombus(annot).

 See also @secref("annotation-macro-protocol").

@examples(
  def x :: converting(fun (x :: Int): x + 1): 11
  ~error: def x :: converting(fun (x :: Int): x + 1): "eleven"
)

}

@doc(
  annot.macro 'Maybe($annot)'
){

 Equivalent to @rhombus(False || annot): an annotation that is
 satisfied by either @rhombus(#false) or a value that satisfies
 @rhombus(annot). If @rhombus(annot) is a @tech{converter annotation},
 its conversion applies to a non-@rhombus(#false) value.

@examples(
  #false :: Maybe(String)
  "string" :: Maybe(String)
  ~error:
    #true :: Maybe(String)
)

}
