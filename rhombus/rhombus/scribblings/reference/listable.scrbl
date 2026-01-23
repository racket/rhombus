#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Listables}

A @deftech{listable} value can be converted to a @tech{list} using a
@rhombus(to_list) operation such as @rhombus(Listable.to_list). Lists
are themselves listable, where @rhombus(List.to_list) just returns the
same list. @tech{Mutable lists}, @tech{pair lists}, @tech{arrays},
listable @tech{ranges}, and instances of classes that implement
@rhombus(Listable, ~class) are also listable.

List-splicing contexts generally allow listables, including in the
construction of a list. As a result, if @nontermref(expr) produces a
listable, another way to convert it to a list is
@rhombus([& #,(nontermref(expr))]), while
@rhombus(PairList[& #,(nontermref(expr))]) would convert it to a
@tech{pair list}.

Typically, a listable object is also @tech{sequence} and directly
@tech{indexable}, but listable does not imply indexable or working as a
sequence.

@doc(
  interface Listable
){

 An interface that a class can implement (publicly or privately) to make
 instances of the class convertable to a @tech{list}. As an annotation,
 @rhombus(Listable, ~annot) matches all @tech{listable} objects, not
 just instances of classes that publicly implement the
 @rhombus(Listable, ~class) interface.

 The interface has a single abstract method:

@itemlist(

 @item{@rhombus(#,(@rhombus(to_list, ~datum))()) --- produces a
  @tech{list} with the same elements as the object in the same order.}

)

@examples(
  ~defn:
    class Posn(x, y):
      private implements Listable
      private override method to_list():
        [x, y]
  ~repl:
    def l :: Listable = Posn(1, 2)
    l.to_list()
)

}


@doc(
  method (v :: Listable).to_list() :: List.of(Any.like_element(v))
){

 Converts any @tech{listable} value to a list.

@examples(
  Listable.to_list([1, 2, 3])
  Listable.to_list(PairList[1, 2, 3])
  Listable.to_list(Array(1, 2, 3))
)

}


@doc(
  annot.macro 'Listable.expect_of($annot)'
){

 An annotation like @rhombus(Listable, ~annot), with static information
 indicating that elements have the static information of @rhombus(annot).
 The extracted elements are not checked or converted, however, and
 @rhombus(annot) is used only for its static information.

}


@doc(
  annot.macro 'Listable.to_list'
){

 A @tech(~doc: guide_doc){converter annotation} that is satisfied by any @tech{listable}
 value and converts it to a @rhombus(List).

@examples(
  ~repl:
    def sizes :: Listable.to_list = Array("small", "medium", "large")
    sizes
  ~defn:
    fun avg(ns :: (Listable.to_list && NonemptyList.of(Number))):
      math.sum(& ns) / ns.length()
  ~repl:
    avg([1, 2, 3])
    avg(Array(10, 20, 30, 40))
    ~error:
      avg(Array("small", "medium", "large"))
)

}
