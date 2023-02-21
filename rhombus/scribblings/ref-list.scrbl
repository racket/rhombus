#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(def dots: @rhombus(..., ~bind))
@(def dots_expr: @rhombus(...))

@title{Lists}

@deftech{Lists} can be constructed using the syntax
@rhombus([#,(@rhombus(expr, ~var)), ...]), which creates list containing the values of the
@rhombus(expr, ~var)s as elements. More precisely, a use of square
brackets without a preceding expression implicitly uses the
@rhombus(#%brackets) form, which (despite its name) is normally bound to
construct a list.

A list works with map-referencing @brackets to access a list
element by position (in time proportional to the position) via
@rhombus(#%ref). A list also works with the @rhombus(++) operator
to append lists. A list can be used as @tech{sequence}, in which case
it supplies its elements in order.

@dispatch_table(
  "list"
  @rhombus(List)
  [lst.length(), List.length(lst)]
  [lst.first, List.first(lst)]
  [lst.rest, List.rest(lst)]
  [lst.reverse(), List.reverse(lst)]
  [lst.map(func), List.map(lst, func)]
)

@doc(
  fun List(v :: Any, ...) :: List
  expr.macro '#%brackets [$expr_or_splice, ...]'
  repet.macro '#%brackets [$repet_or_splice, ...]'

  grammar expr_or_splice:
    $expr
    $repetition #,(@litchar{,}) $ellipses
    & $list_expr

  grammar ellipses:
    $ellipsis
    $ellipses #,(@litchar{,}) ellipsis

  grammar ellipsis:
    #,(dots_expr)

){

 Constructs a list of the given @rhombus(v)s values or results of the
 @rhombus(expr_or_splice)s. A @rhombus(&) or @dots_expr form
 can appear within @rhombus([]) to splice a @tech{repetition} or existing list
 into the constructed list, the same as in a function call (see
 @rhombus(#%call)). List constructions can also serve as
 repetitions, where @rhombus(repet_or_splice) is like
 @rhombus(expr_or_splice), but with repetitions in place of expressions.

 @see_implicit(@rhombus(#%brackets), @rhombus([]), "expression")

@examples(
  def lst = List(1, 2, 3)
  lst
  lst[0]
  lst ++ [4, 5]
  #%brackets [1, 2, 3]
)

}

@doc(
  bind.macro 'List($binding, ...)'
  bind.macro 'List($binding, ..., $rest)'
  bind.macro '#%brackets [$binding, ...]'
  bind.macro '#%brackets [$binding, ..., $rest]'
  grammar rest:
    $repetition_binding #,(@litchar{,}) $ellipsis
    & $list_binding
  grammar ellipsis:
    #,(dots)
){

 Matches a list with as many elements as @rhombus(binding)s, or if
 @rhombus(rest) is included, at least as many elements as
 @rhombus(binding)s, where the @rhombus(rest) (if present) matches the
 rest of the list.

 @see_implicit(@rhombus(#%brackets, ~bind), @rhombus([]), "binding")

@examples(
  def List(1, x, y): [1, 2, 3]
  y
  def [1, also_x, also_y]: [1, 2, 3]
  also_y
  def List(1, & xs): [1, 2, 3]
  xs
  def List(1, x, ...): [1, 2, 3]
  [x, ...]
)

}

@doc(
  annot.macro 'List'
  annot.macro 'List.of($annotation)'
){

 Matches any list in the form without @rhombus(of). The @rhombus(of)
 variant matches a list whose elements satisfy @rhombus(annotation).

}

@doc(
  annot.macro 'NonemptyList'
  annot.macro 'NonemptyList.of($annotation)'
){

 Like @rhombus(List, ~annot) as an annotation, but matches only non-empty
 lists.

@examples(
  [1] :: NonemptyList
  ~error: [] :: NonemptyList
)

}

@doc(
  reducer.macro 'List'
){

 A @tech{reducer} used with @rhombus(for), accumulates each result of a
 @rhombus(for) body into a result list.

}

@doc(
  fun List.cons(elem :: Any, lst :: List) :: List
){

 Creates a list like @rhombus(lst), but with @rhombus(elem) added to
 the front.

@examples(
  List.cons(1, [2, 3])
)

}

@doc(
  bind.macro 'List.cons($elem_binding, $list_binding)'
){

 Matches a non-empty list where @rhombus(elem_binding) matches the
 first element of the list and @rhombus(list_binding) matches the
 rest of the list.

@examples(
  def List.cons(x, y): [1, 2, 3]
  x
  y
)

}


@doc(
  def List.empty :: []
  bind.macro 'List.empty'
){

  A name and pattern for the empty list.

}


@doc(
  fun List.first(lst :: NonemptyList)
){

 Returns the first element of @rhombus(lst).

@examples(
  List.first(["a", "b", "c"])
)

}

@doc(
  fun List.rest(lst :: NonemptyList) :: List
){

 Returns a list like @rhombus(lst), but without its first element.

@examples(
  List.rest(["a", "b", "c"])
)

}

@doc(
  fun List.length(lst :: List) :: Int
){

 Returns the number of items in @rhombus(lst).

@examples(
  List.length([1, 4, 8])
  List.length([])
  [1, 4, 8].length
)

}


@doc(
  fun List.reverse(lst :: List) :: List
){

 Returns a list with the same items as @rhombus(lst). but in reversed
 order.

@examples(
  List.reverse([1, 4, 8])
  [1, 4, 8].reverse
)

}

@doc(
  fun List.map(args :: List, f :: Function) :: List,
){

 Like @rhombus(Function.map), but with a single list of arguments first,
 with the function supplied second.

@examples(
  List.map([1, 2, 3], fun (x): x + 1)
  [1, 2, 3].map(fun (x): x + 1)
)

}



@doc(
  fun List.iota(n :: NonnegInt) :: List.of(NonnegInt)
){

 Returns a list containing the integers 0 to @rhombus(n) (exclusive) in
 order.

@examples(
  List.iota(3)
  List.iota(0)
)

}
