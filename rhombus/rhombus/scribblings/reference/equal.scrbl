#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Equality}

See also @secref("Equatables").

@doc(
  operator ((v1 :: Any) == (v2 :: Any)) :: Boolean:
    ~order: equivalence
  key_comp.def '=='
){

 The @rhombus(==) operator reports whether @rhombus(v1) and @rhombus(v2) are equal, which includes
 recursively comparing elements of compound data structures. Two numbers
 are @rhombus(==) only if they are both exact or both inexact. Two mutable
 values are @rhombus(==) only if they the same object (i.e., mutating one
 has the same effect as mutating the other). A class can customize the
 equality comparison for its instances by implementing the
 @rhombus(Equatable, ~class) interface.

 The @rhombus(==, ~key_comp) @tech(~doc: meta_doc){map configuration} can be used with forms like
 the @rhombus(Map.by) constructor or @rhombus(Map.by, ~annot)
 annotation constructor to specify the default equality and hashing
 functions for map keys. The @rhombus(Map.by(==), ~annot) annotation, for
 example, matches only maps that use the default equality and hashing
 functions.

@examples(
  "apple" == "apple"
  [1, 2, 3] == 1
  [1, "apple", {"alice": 97}] == [1, "apple", {"alice": 97}]
  1 == 1.0
)

}

@doc(
  operator ((v1 :: Any) === (v2 :: Any)) :: Boolean:
    ~order: equivalence
  key_comp.def '==='
){

 The @rhombus(===) operator reports whether @rhombus(v1) and @rhombus(v2) are the same object.
 Being the @emph{same} is weakly defined, but only @rhombus(==) values
 can possibly be the same object, and mutable values are the same only if
 modifying one has the same effect as modifying the other. Interned
 values like symbols are @rhombus(===) when they are @rhombus(==).

 The @rhombus(===, ~key_comp) @tech(~doc: meta_doc){map configuration} can be used with forms
 like the @rhombus(Map.by) constructor or @rhombus(Map.by, ~annot)
 annotation constructor to specify key equality with @rhombus(===) and a
 corresponding hashing function.

@examples(
  #'apple === #'apple
  #'apple === #'banana
)

}

@doc(
  operator ((x :: Number) .= (y :: Number)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Number) .!= (y :: Number)) :: Boolean:
    ~order: order_comparison
){

 Reports whether @rhombus(x) and @rhombus(y) are numerically equal or
 unequal, where inexact numbers are effectively coerced to exact for
 comparisons to exact numbers. The value @rhombus(#nan) is not
 @rhombus(.=) to itself (but @rhombus(#nan) is @rhombus(==) to itself).

 These comparisons are specialized like @rhombus(+) for arguments with
 @rhombus(Flonum, ~annot) static information.

@examples(
  1 .= 1
  1 .= 2
  1.0 .= 1
  1 .!= 2
  1 .!= 1.0
)

}

@doc(
  operator ((v1 :: Any) != (v2 :: Any)) :: Boolean:
    ~order: equivalence
){

 Equivalent to @rhombus(!(v1 == v2)).

@examples(
  "apple" != "apple"
)

}


@doc(
  operator ((v1 :: Any) is_now (v2 :: Any)) :: Boolean:
    ~order: equivalence
  non_target:
    expr.macro '$expr !is_now $expr'
  non_target:
    repet.macro '$repet !#,(@rhombus(is_now, ~repet)) $repet'
  key_comp.def 'is_now'
){

 Reports whether @rhombus(v1) and @rhombus(v2) are equivalent @emph{now}
 in the sense that mutable fields of objects have the same values. The
 operator combination @rhombus(!is_now) inverts the test.

 Mutable and immutable strings, byte vectors, and arrays are considered
 the same if they have the same elements, even if one is mutable and the
 other is immutable. However, a mutable map or set is never considered
 equivalent to an immutable map or set, even if they have the same
 content.

 The @rhombus(is_now, ~key_comp) @tech(~doc: meta_doc){map configuration} can be used with forms
 like the @rhombus(Map.by) constructor or @rhombus(Map.by, ~annot)
 annotation constructor to specify key equality with @rhombus(is_now) and a
 corresponding hashing function. Beware, however, that mutating a key after
 it is mapped will tend to make the map entry inaccessible.

@examples(
  ~repl:
    #"apple" is_now Bytes.copy(#"apple")
    #"apple" == Bytes.copy(#"apple")
  ~defn:
    class Posn(mutable x, mutable y)
  ~repl:
    Posn(1, 2) is_now Posn(1, 2)
    Posn(1, 2) == Posn(1, 2)
)

}

@doc(
  operator ((v1 :: Any) is_same_number_or_object (v2 :: Any))
    :: Boolean:
      ~order: equivalence
  key_comp.def 'is_same_number_or_object'
){

 The @rhombus(is_same_number_or_object) operator checks whether two values
 are either equal via @rhombus(===) or are two numbers that are
 @rhombus(==). This comparison is primarily intended for comparing
 numbers, and especially via @rhombus(is_same_number_or_object, ~key_comp) as a
 @tech(~doc: meta_doc){map configuration} for equating map keys, where
 @rhombus(is_same_number_or_object, ~key_comp) can be slightly more efficient
 than @rhombus(==).

}

@doc(
  expr.macro '='
){

 The @rhombus(=) operator is not bound as an expression or binding
 operator. It is used as a syntactic delimiter by various forms, such as
 in @rhombus(fun) when specifying the default value for an optional
 argument.

}
