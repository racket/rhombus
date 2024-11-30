#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Annotations}

@doc(
  expr.macro '$expr :: $annot'
){

 Checks that the value of @rhombus(expr) satisfies
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

 If @rhombus(annot) is a @tech(~doc: guide_doc){converter annotation}, the conversion is
 applied before matching the converted value against @rhombus(bind). This
 dependency implies that the conversion in @rhombus(annot) cannot be
 delayed, and must be performed as part of the matching process (before
 committing to the match).

@examples(
  def x :: List = [1, 2, 3]
)

}

@doc(
  annot.macro 'Any'
  annot.macro 'Any.of($expr, ...)'
  annot.macro 'Any.to_boolean'
  annot.macro 'None'
){

 The @rhombus(Any, ~annot) annotation matches any value. An
 @rhombus(Any.of, ~annot) annotation matches any value that is equal (in
 the sense of @rhombus(==)) to one of the @rhombus(expr) results. The
 @rhombus(Any.to_boolean, ~annot) annotation matches any value and
 converts non-@rhombus(#false) value to @rhombus(#true).

 The @rhombus(None, ~annot) annotation matches no values, or in other
 words, is equivalent to @rhombus(Any.of(), ~annot). It is useful for
 asserting that something never returns, such as a function that
 always throws.

@examples(
  "hello" is_a Any
  "hello" is_a Any.of("hello", "goodbye")
  "hola" is_a Any.of("hello", "goodbye")
  "hello" :: Any.to_boolean
  #false :: Any.to_boolean
  "will not match" is_a None
  "will not match" is_a Any.of()
)

}

@doc(
  expr.macro '$expr :~ $annot'
){

 Associates static information to the overall expression the same as
 @rhombus(::), but performs no run-time check on the value of
 @rhombus(expr). The @rhombus(annot) must specify a @tech(~doc: guide_doc){predicate
  annotation}.

@examples(
  [1, 2, 3] :~ List
  "oops" :~ List
)

}

@doc(
  bind.macro '$bind :~ $annot'
){

 Associates static information to @rhombus(bind) the same as
 @rhombus(::, ~bind), but performs no run-time check. The @rhombus(annot)
 must specify a @tech(~doc: guide_doc){predicate annotation}.

@examples(
  def x :~ List = [1, 2, 3]
  def x :~ List = "oops"
)

}


@doc(
  expr.macro '$expr is_a $annot'
){

 Produces @rhombus(#true) if the value of @rhombus(expr)
 satisfies @rhombus(annot), @rhombus(#false) otherwise.

 If @rhombus(annot) is a @tech(~doc: guide_doc){converter annotation}, only the matching
 component of the annotation is used, and the converting part is not
 used. See also @secref(~doc: guide_doc, "annotation-macro-protocol").

@examples(
  [1, 2, 3] is_a List
  "oops" is_a List
)

}


@doc(
  bind.macro '$bind described_as ($term ...)'
  bind.macro '$bind described_as $term ...'
){

 Equivalent to @rhombus(bind), but when a binding match fails in a way
 that triggers an error message (as opposed to moving on to a different
 binding pattern), the message describes the expected annotation as
 @rhombus(term ...). The @rhombus(term ...) sequence is not parsed, so it
 can be any sequence of terms, but the first @rhombus(term) can be parenthesized
 only if the @rhombus(term ...) sequence is parenthesized.

@examples(
  ~error:
    def (x :: Int) described_as An Integer = "oops"
)

}


@doc(
  annot.macro 'matching($bind)'
){

 Converts @rhombus(bind) into an annotation. Variables bound in
 @rhombus(bind) are not made visible, but the annotation corresponds to
 the set of values for which @rhombus(bind) would match. Since no results
 are made visible, @rhombus(bind) is used only in matching mode, and
 implied conversions might be skipped.

@examples(
  def x :: matching([_, 10]) = [9, 10]
  ~error:
    def y :: matching([_, 10]) = [9, 11]
)

}

@doc(
  ~nonterminal:
    pred_expr: block expr
  annot.macro 'satisfying(pred_expr)'
){

 Produces a @tech(~doc: guide_doc){predicate annotation} using the resulting function
 from @rhombus(pred_expr).

@examples(
  ~defn:
    fun is_multiple_of(n):
      fun (v):
        v is_a Int && v mod n == 0
  ~repl:
    15 :: (satisfying(is_multiple_of(3))
             && satisfying(is_multiple_of(5)))
  ~defn:
    fun
    | is_list_with_one(lst :: List):
        lst.has_element(1)
    | is_list_with_one(_):
        #false
  ~repl:
    [1, 2, 3] :: satisfying(is_list_with_one)
    ~error:
      Array(1, 2, 3) :: satisfying(is_list_with_one)
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

 Produces a @tech(~doc: guide_doc){converter annotation} by pairing @rhombus(bind) with
 @rhombus(body). The annotation matches when @rhombus(bind) matches, but
 the value produced by the annotation is determined by the @rhombus(body)
 sequence, which can refer to variables bound by @rhombus(bind).

 When @rhombus(annot) is provided, then its static information is
 propagated to the new converter annotation. If @rhombus(annot) is
 supplied with @rhombus(::, ~bind), then the result of the @rhombus(body)
 sequence is checked against @rhombus(annot).

 See also @secref(~doc: guide_doc, "annotation-macro-protocol").

@examples(
  def x :: converting(fun (x :: Int): x + 1) = 11
  ~error:
    def x :: converting(fun (x :: Int): x + 1) = "eleven"
)

}

@doc(
  annot.macro 'maybe($annot)'
){

 Equivalent to @rhombus(#,(@rhombus(False, ~annot)) #,(@rhombus(||, ~annot)) annot), which is an annotation that is
 satisfied by either @rhombus(#false) or a value that satisfies
 @rhombus(annot). If @rhombus(annot) is a @tech(~doc: guide_doc){converter annotation},
 its conversion applies to a non-@rhombus(#false) value.

 See also @rhombus(definitely).

@examples(
  #false :: maybe(String)
  "string" :: maybe(String)
  ~error:
    #true :: maybe(String)
)

}

@doc(
  expr.macro '$expr !!'
  bind.macro '$bind !!'
){

 An an expression, @rhombus(expr!!) ensures that the result of
 @rhombus(expr) is not @rhombus(#false) by throwing an exception if the
 result is @rhombus(#false). If @rhombus(expr) has static information
 from @rhombus(maybe(#,(@nontermref(annot))), ~annot), then the overall
 @rhombus(!!) expression gets the static information of
 @nontermref(annot).

 As a binding, @rhombus(bind!!) matches non-@rhombus(#false) values that
 match @rhombus(bind). Similar to the @rhombus(!!) expression form, when
 static information for the input to @rhombus(bind!!) is
 @rhombus(maybe(#,(@nontermref(annot))), ~annot), then @rhombus(bind)
 more specifically starts with the static information of
 @nontermref(annot).

 See also @rhombus(?)

@examples(
  ~repl:
    ~error:
      definitely(#false)
    ~error:
      #false!!
  ~defn:
    fun len(str :: maybe(String)):
      use_static
      match str
      | s!!: s.length()
      | ~else: 0
  ~repl:
    len("apple")
    len(#false)
    ~error:
      len(1)
)

}


@doc(
  expr.macro '$expr ? $infix_op_and_tail'
){

 If @rhombus(expr) produces @rhombus(#false), then the result of a
 @rhombus(?) expression is @rhombus(#false). Otherwise,
 @rhombus(infix_op_and_tail) is expected to continue with an infix
 operator, such as @rhombus(.), and the result of @rhombus(expr) is used
 as the left argument to that operator.

 When @rhombus(expr) has static information from
 @rhombus(maybe(#,(@nontermref(annot))), ~annot), then the argument to
 the infix operator has static information of @nontermref(annot). If the
 result in the non-@rhombus(#false) case has static information like
 @nontermref(annot), then the overall @rhombus(?) expression has static
 information like @rhombus(maybe(#,(@nontermref(annot))), ~annot).

@examples(
  ~defn:
    fun len(str :: maybe(String)):
      use_static
      str?.length() || 0
  ~repl:
    len("apple")
    len(#false)
)

}
