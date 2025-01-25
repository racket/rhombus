#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Indexables}

An @deftech{indexable} value is one that supports @brackets afterward
to extract an element at the index within @brackets. @tech{Maps},
@tech{lists}, @tech{mutable lists}, @tech{pair lists}, @tech{arrays},
@tech{strings}, and @tech{byte strings} are all indexable, as are
instances of classes that implement @rhombus(Indexable, ~class).

@doc(
  ~nonterminal:
    at_expr: block expr
    at_repet: block repet
    rhs_expr: block expr
  expr.macro '$expr #%index [$at_expr]'
  expr.macro '$expr #%index [$at_expr] $assign_op $rhs_expr'
  repet.macro '$repet #%index [$at_repet]'
  grammar assign_op:
    :=
    $other_assign_op
){

 Without an @rhombus(assign_op), accesses an element of an
 @tech{indexable} object, such as a map, list, mutable list, pair
 list, array, string, byte string, or @rhombus(Indexable, ~class)
 object. The element is accessed from the value produced by
 @rhombus(expr) at the index or key produced by @rhombus(at_expr). The
 access form also works as a @tech{repetition} given repetitions for a
 collection and an index.

 With an @rhombus(assign_op), for a mutable indexable object (such as
 a mutable list, mutable array, mutable map, mutable byte string, or
 @rhombus(MutableIndexable, ~class) object), the index element is
 assigned to the value based on the operator and
 @rhombus(rhs_expr). The expression result is @rhombus(#void) in the
 case of @rhombus(:=) as @rhombus(assign_op).

 See also @rhombus(use_static).

 @see_implicit(@rhombus(#%index), @brackets, "expression or repetition", ~is_infix: #true)

@examples(
  {"a": 1, "b": 2}["a"]
  {"a": 1, "b": 2} #%index ["a"]
)

}


@doc(
  interface Indexable
){

@provided_interface_and_other_annotation_only()

 An interface that a class can implement (publicly or privately) to make
 instances of the class work with @rhombus(#%index). As an annotation,
 @rhombus(Indexable, ~annot) matches all @tech{indexable} objects, not
 just instances of classes that publicly implement the
 @rhombus(Indexable, ~class) interface.

 The interface has a single abstract method:

@itemlist(

 @item{@rhombus(#,(@rhombus(get, ~datum))(#,(@rhombus(index, ~var))))
  --- the @rhombus(index, ~var) value is the second argument to @rhombus(#%index),
  which is normally written within @brackets. The result of the
  @rhombus(get) method is the result of the index form.}

)

@examples(
  ~defn:
    class Interleaved(lst1, lst2):
      private implements Indexable
      private override method get(index):
        if (index mod 2) == 0
        | lst1[index div 2]
        | lst2[index div 2]
  ~repl:
    def lsts = Interleaved([1, 2, 3], [-1, -2, -3])
    lsts[2]
    lsts[3]
)

}

@doc(
  interface MutableIndexable:
    extends Indexable
){

@provided_interface_and_other_annotation_only()

 An interface that extends @rhombus(Indexable, ~class) to add support
 for assignment with @rhombus(#%index). As an annotation,
 @rhombus(MutableIndexable, ~annot) matches all mutable @tech{indexable}
 objects.

 The interface has one additional abstract method:

@itemlist(

 @item{@rhombus(#,(@rhombus(set, ~datum))(#,(@rhombus(index, ~var)), #,(@rhombus(val, ~var))))
  --- takes an @rhombus(index, ~var) and new @rhombus(val, ~var), which are the second and
  third arguments to @rhombus(#%index). Those arguments are normally written
  within @brackets and after an assignment operator like @rhombus(:=),
  respectively. The result must be @rhombus(#void).}

)
@examples(
  ~defn:
    class InterleavedArray(arr1, arr2):
      private implements MutableIndexable
      private override method get(index):
        if (index mod 2) == 0
        | arr1[index div 2]
        | arr2[index div 2]
      private override method set(index, val):
        if (index mod 2) == 0
        | arr1[index div 2] := val
        | arr2[index div 2] := val
  ~repl:
    def lsts = InterleavedArray(Array(1, 2, 3), Array(-1, -2, -3))
    lsts[2] := 20
    lsts
)

}
