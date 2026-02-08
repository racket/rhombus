#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Annotations}

@doc(
  expr.macro '$expr :: $annot'
  repet.macro '$repet :: $annot'
){

 Checks that the value of @rhombus(expr) satisfies @rhombus(annot),
 and returns the value if so. The @rhombus(expr) is @emph{not} in
 @tail_position with respect to the @rhombus(::) expression.

 If @rhombus(annot) is a @tech(~doc: model_doc){converter annotation},
 the converted value is returned.

@examples(
  [1, 2, 3] :: List
  PairList[1, 2, 3] :: Listable.to_list
)

}

@doc(
  bind.macro '$bind :: $annot'
){

 Binds the same as @rhombus(bind), but first checks that the value to be
 bound satisfies @rhombus(annot). If @rhombus(bind) is an
 @tech(~doc: model_doc){immediate binding} and @rhombus(annot) is a
 @tech(~doc:model_doc){always-satisfied annotation}, then
 @rhombus(bind :: annot, ~bind) is also an immediate binding.

 If @rhombus(annot) is a @tech(~doc: model_doc){converter annotation}, the conversion is
 applied before matching the converted value against @rhombus(bind). This
 dependency implies that the conversion in @rhombus(annot) cannot be
 delayed, and must be performed as part of the matching process (before
 committing to the match).

@examples(
  def x :: List = [1, 2, 3]
  def [x, y, z] :: Listable.to_list = PairList[1, 2, 3]
)

}

@doc(
  expr.macro '$expr :~ $annot'
  repet.macro '$repet :~ $annot'
){

 Associates static information to the overall expression the same as
 @rhombus(::), but performs no run-time check on the value of
 @rhombus(expr). The @rhombus(annot) must specify a @tech(~doc: model_doc){predicate
  annotation}. The @rhombus(expr) is in @tail_position with respect to
 the @rhombus(::) expression.

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
 must specify a @tech(~doc: model_doc){predicate annotation}. If
 @rhombus(bind) is an @tech(~doc: model_doc){immediate binding}, then
 @rhombus(bind :~ annot, ~bind) is also an immediate binding.
 See also @rhombus(assuming, ~annot).

@examples(
  def x :~ List = [1, 2, 3]
  def x :~ List = "oops"
)

}

@doc(
  annot.macro 'Any'
  annot.macro 'Any.of($expr, ...)'
  annot.macro 'Any.to_boolean'
  annot.macro 'None'
){

 The @rhombus(Any, ~annot) annotation matches any value, so it
 is a @tech(~doc: model_doc){always-satisfied annotation}. An
 @rhombus(Any.of, ~annot) annotation matches any value that is equal (in
 the sense of @rhombus(==)) to one of the @rhombus(expr) results. The
 @rhombus(Any.to_boolean, ~annot) annotation matches any value and
 converts non-@rhombus(#false) value to @rhombus(#true).

 The @rhombus(None, ~annot) annotation matches no values, or in other
 words, is equivalent to @rhombus(Any.of(), ~annot). It is useful for
 asserting that something never returns, such as a function that always
 throws an exception. The annotation
 @rhombus(None || #,(@rhombus(ann, ~var)), ~annot) is equivalent to
 @rhombus(ann, ~var), since any value that satisfies the annotation must
 satisfy @rhombus(ann, ~var). In principle, @rhombus(None, ~annot) would
 imply any other annotation and provide all static information; instead,
 as a pratical compromise, @rhombus(None, ~annot) provides no oher static
 information. As a further practical choice along those lines, the annotation
 @rhombus(None && #,(@rhombus(ann, ~var)), ~annot) implies all the static
 information of @rhombus(ann, ~var), while still behaving like
 @rhombus(None, ~annot) for further combinations via @rhombus(||, ~annot).

 See @secref(~doc: guide_doc, "annotation-satisfying") for information
 about the time that @rhombus(expr) is evaluated.

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
  ~nonterminal:
    arg_id: block id
    class_name: namespace id_name ~defn
    field_id: block id
  annot.macro 'Any.like($arg_id)'
  annot.macro 'Any.like_element($arg_id)'
  annot.macro 'Any.like_key($arg_id)'
  annot.macro 'Any.like_value($arg_id)'
  annot.macro 'Any.like_first($arg_id)'
  annot.macro 'Any.like_rest($arg_id)'
  annot.macro 'Any.like_result($arg_id)'
  annot.macro 'Any.like_field($class_name . $field_id($arg_id))'
){

 Annotation constructors for use in a context where named arguments are
 available---especially in the result annotation position of
 @rhombus(fun, ~defn). Annotations created by @rhombus(Any.like, ~annot)
 and related forms do not imply any run-time checks, but they propagate
 static information from actual argument expressions in a specific
 function call to that specific call's result.

 In the case of a method, @rhombus(this) can be used as an
 @rhombus(arg_id) to refer to the actual target object. When an
 @rhombus(arg_id) refers to a repetition, then static information for
 actual arguments mapped to the repetition are combined with
 @rhombus(statinfo_meta.or). When an @rhombus(arg_id) refers to a splice
 or keyword splice, then corresponding arguments are similarly combined
 with @rhombus(statinfo_meta.or) to get the element or value static
 information within the splice variable as a list or map.

 An @rhombus(Any.like, ~annot) annotation propagates static information
 directly from an argument.

@examples(
  ~defn:
    fun twice(v) :: List.of(Any.like(v)):
      [v, v]
    fun rev(v, ...) :: List.of(Any.like(v)):
      [v, ...].reverse()
  ~repl:
    use_static
    def lst = twice("apple")
    :
      lst[0].length() // method found statically
    :
      rev("a", "bb", "ccc")[0].length() // ditto
)

 An @rhombus(Any.like_element, ~annot) annotation propagates static
 information corresponding to @tech{sequence} or @tech{indexable} values
 within an argument.

@examples(
  ~defn:
    fun pick(choices :: List) :: Any.like_element(choices):
      choices[math.random(choices.length())]
  ~repl:
    use_static
    pick(["a", "b"]).length()
)

 The @rhombus(Any.like_key, ~annot) or @rhombus(Any.like_value, ~annot)
 annotation constructors are normally used for @tech{maps}. They are
 similar to @rhombus(Any.like_element, ~annot), but for a @tech{sequence}
 that produces two values, where @rhombus(Any.like_key, ~annot)
 corresponds to the first value and @rhombus(Any.like_value, ~annot) the
 second.

@examples(
  ~defn:
    fun listize(m :: Map) :: List.of(Pair.of(Any.like_key(m),
                                             Any.like_value(m))):
      for List ((k, v) in m):
        Pair(k, v)
  ~repl:
    use_static
    def lst = listize({ "a": [1, 2, 3], "b": [0, 0] })
    lst[0].first.length()
    lst[1].rest.reverse()
)

 The @rhombus(Any.like_first, ~annot) or @rhombus(Any.like_rest, ~annot) annotations
 are similar to @rhombus(Any.like_element, ~annot), but for the components of
 @tech{pairs}.

@examples(
  ~defn:
    fun pick_part(pr :: Pair) :: Any.like_first(pr) || Any.like_rest(pr):
      if math.random(2) == 0
      | pr.first
      | pr.rest
  ~repl:
    use_static
    pick_part(Pair("apples", "banana")).length()
)

 The @rhombus(Any.like_result, ~annot) annotation is
 similar to @rhombus(Any.like_element, ~annot), but for the result of
 a @tech{function}.

@examples(
  ~defn:
    fun apply_2(f, a, b) :: Any.like_result(f):
      f(a, b)
  ~repl:
    use_static
    apply_2(String.append, "a", "b").length()
)

 An @rhombus(Any.like_field(class_name.field_id(arg_id)), ~annot)
 annotation propagates static information corresponding to the field
 named by @rhombus(field_id) in the class named by @rhombus(class_name),
 where @rhombus(arg_id) is an object that is an instance of
 @rhombus(class_name).

@examples(
  ~defn:
    class Posn(x :: Int || Flonum, y :: Int || Flonum):
      // if both are Int or both are Flonum, result is the same
      method dist() :: (Any.like_field(Posn.x(this))
                          || Any.like_field(Posn.y(this))):
        x + y
  ~repl:
    :
      3.0 + Posn(1.0, 2.0).dist() // uses Flonum arithmetic
    :
      3.0 + Posn(1, 2.0).dist() // uses generic arithmetic
)

}


@doc(
  expr.macro '$expr is_a $annot'
  repet.macro '$repet is_a $annot'
  non_target:
    expr.macro '$expr !is_a $annot'
  non_target:
    repet.macro '$repet !#,(@rhombus(is_a, ~repet)) $annot'
  operator_order:
    ~order: equivalence
){

 Produces @rhombus(#true) if the value of @rhombus(expr)
 satisfies @rhombus(annot), @rhombus(#false) otherwise.
 The operator combination @rhombus(!is_a) inverts the test. Either
 form works as a @tech{repetition} given a repetition to test.

 If @rhombus(annot) is a @tech(~doc: model_doc){converter annotation}, only the matching
 component of the annotation is used, and the converting part is not
 used. See also @secref(~doc: guide_doc, "annotation-convert").

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

 Note that when @rhombus(bind) involves a conversion (that is not
 performed), the static information associated with
 @rhombus(matching(bind), ~annot) corresponds to the conversion input,
 not its output. For example,
 @rhombus(matching(_ :: String.to_int), ~annot) will have the static
 information of @rhombus(String, ~annot), not @rhombus(Int, ~annot).
 Similarly, @rhombus(matching(_ :: MutableList.later_of(String)), ~annot)
 will have the static information of @rhombus(MutableList, ~annot), but
 no information about list elements.

 When @rhombus(bind) is an @tech(~doc: model_doc){immediate binding},
 then @rhombus(matching(bind), ~annot) is an
 @tech(~doc:model_doc){always-satisfied annotation}.

}

@doc(
  ~nonterminal:
    pred_expr: block expr
  annot.macro 'satisfying(pred_expr)'
){

 Produces a @tech(~doc: model_doc){predicate annotation} using the resulting function
 from @rhombus(pred_expr).

 See @secref(~doc: guide_doc, "annotation-satisfying") for information
 about the time that @rhombus(pred_expr) is evaluated.

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
        lst.contains(1)
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
  grammar maybe_res_annot
  | #,(@rhombus(::, ~bind)) $annot
  | #,(@rhombus(:~, ~bind)) $annot
  | #,(epsilon)
){

 Produces a @tech(~doc: model_doc){converter annotation} by pairing @rhombus(bind) with
 @rhombus(body). The annotation matches when @rhombus(bind) matches, but
 the value produced by the annotation is determined by the @rhombus(body)
 sequence, which can refer to variables bound by @rhombus(bind).

 When @rhombus(annot) is provided, then its static information is
 propagated to the new converter annotation. If @rhombus(annot) is
 supplied with @rhombus(::, ~bind), then the result of the @rhombus(body)
 sequence is checked against @rhombus(annot).

 See also @secref(~doc: guide_doc, "annotation-convert").

@examples(
  def x :: converting(fun (x :: Int): x + 1) = 11
  ~error:
    def x :: converting(fun (x :: Int): x + 1) = "eleven"
)

}

@doc(
  annot.macro 'assuming($annot)'
){

 An @tech(~doc: model_doc){always-satisfied annotation} with the same
 static information as @rhombus(annot), which must be a
 @tech(~doc: model_doc){predicate annotation}. This form is equivalent to
 @rhombus(matching(_ :~ annot), ~annot).

@examples(
  use_static
  def strs :: assuming(List.of(String)) = ["a", #'oops, "bcde"]
  strs[2].length()
  ~error:
    strs[1].length()
)

}

@doc(
  annot.macro 'maybe($annot)'
){

 Equivalent to @rhombus(#,(@rhombus(False, ~annot)) #,(@rhombus(||, ~annot)) annot), which is an annotation that is
 satisfied by either @rhombus(#false) or a value that satisfies
 @rhombus(annot). If @rhombus(annot) is a @tech(~doc: model_doc){converter annotation},
 its conversion applies to a non-@rhombus(#false) value.

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
  repet.macro '$repet !!'
  operator_order:
    ~order: logical_negation
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

@examples(
  ~repl:
    "apple"!!
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
  expr.macro '$expr !!.'
  repet.macro '$repet !!.'
  operator_order:
    ~order: member_access
){

 The @rhombus(!!.) operator is equivalent to @rhombus(!!) followed by
 @rhombus(.), but without a space in between the operators.

@examples(
  ~defn:
    fun len(str :: maybe(String)):
      use_static
      str!!.length() || 0
  ~repl:
    len("apple")
    ~error:
      len(#false)
)

}


@doc(
  expr.macro '$expr ?. $id ($arg, ...)'
  expr.macro '$expr ?. $id'
  repet.macro '$repet ?. $id ($arg, ...)'
  repet.macro '$repet ?. $id'
  operator_order:
    ~order: member_access
){

 If @rhombus(expr) produces @rhombus(#false), then the result of a
 @rhombus(?.) expression is @rhombus(#false). Otherwise, @rhombus(?.) is
 like @rhombus(.) for a field access or method call.

 When @rhombus(expr) has static information from
 @rhombus(maybe(#,(@nontermref(annot))), ~annot), then the argument to
 @rhombus(.) has static information of @nontermref(annot). If the result
 of the @rhombus(.) form in the non-@rhombus(#false) case has static
 information like @nontermref(annot), then the overall @rhombus(?.)
 expression has static information like
 @rhombus(maybe(#,(@nontermref(annot))), ~annot).

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
