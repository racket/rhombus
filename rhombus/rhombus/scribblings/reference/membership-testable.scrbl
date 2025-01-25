#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Membership Tests}

A @deftech{membership test} uses the @rhombus(in) operator to check
whether an element is included in the value. @tech{Maps},
@tech{lists}, @tech{mutable lists}, @tech{pair lists}, @tech{arrays},
@tech{sets}, and @tech{ranges} all support membership tests, as do
instances of classes that implement @rhombus(MembershipTestable, ~class).

@doc(
  ~nonterminal:
    elem_expr: block expr
    elem_repet: block repet
  expr.macro '$elem_expr in $expr'
  repet.macro '$elem_repet in $repet'
  non_target:
    expr.macro '$elem_expr !in $expr'
  non_target:
    repet.macro '$elem_repet !#,(@rhombus(in, ~repet)) $repet'
  operator_order:
    ~order: equivalence
){

 Checks whether the result of @rhombus(elem_expr) is a member of the
 result of @rhombus(expr), where the latter is a value such as a map,
 list, mutable list, pair list, array, set, range, or
 @rhombus(MembershipTestable, ~class) object that support
 @tech{membership tests}. The result is @rhombus(#true) for
 @rhombus(in) if the element is present and @rhombus(#false) otherwise.
 The operator combination @rhombus(!in) inverts the result relative to
 @rhombus(in).

 The @rhombus(in) operator is also recognized by @rhombus(for) and
 @rhombus(each, ~for_clause) as part of the syntax of iteration.

 The @rhombus(use_static) declaration constrains @rhombus(in) to work
 only when the right-hand argument has static information indicating
 that it satisfies @rhombus(MembershipTestable, ~annot).

@examples(
  "a" in {"a", "b"}
  "c" !in {"a", "b"}
  "a" in {"a": 1, "b": 2}
  "a" in ["a", "b"]
  1 in {"a": 1, "b": 2}
)

}


@doc(
  interface MembershipTestable
){

 An interface that a class can implement (publicly or privately) to make
 instances of the class work with @rhombus(in). As an annotation,
 @rhombus(MembershipTestable, ~annot) matches all objects that support
 @tech{membership tests}, not just instances of classes that publicly
 implement the @rhombus(MembershipTestable, ~class) interface.

 The interface has a single abstract method:

@itemlist(

 @item{@rhombus(#,(@rhombus(contains, ~datum))(#,(@rhombus(val, ~var))))
  --- checks for membership of @rhombus(val, ~var), which is normally
  provided to the left of @rhombus(in). The result of the
  @rhombus(contains) method must be a @rhombus(Boolean) value, and it
  is the result of the @rhombus(in) form.}

)

@examples(
  ~defn:
    class Either(lst1, lst2):
      private implements MembershipTestable
      private override method contains(v):
        v in lst1 || v in lst2
  ~repl:
    def lsts = Either([1, 2, 3], [-1, -2, -3])
    2 in lsts
    4 !in lsts
)

}
