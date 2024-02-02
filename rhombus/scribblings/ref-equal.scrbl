#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Equality}

@doc(
  operator ((v1 :: Any) == (v2 :: Any)) :: Boolean
){

 Reports whether @rhombus(v1) and @rhombus(v2) are equal, which includes
 recursively comparing elements of compound data structures. Two numbers
 are @rhombus(==) only if they are both exact or both inexact. Two mutable
 values are @rhombus(==) only if they the same object (i.e., mutating one
 has the same effect as mutating the other).

@examples(
  "apple" == "apple"
  [1, 2, 3] == 1
  [1, "apple", {"alice": 97}] == [1, "apple", {"alice": 97}]
  1 == 1.0
)

}

@doc(
  operator ((v1 :: Any) === (v2 :: Any)) :: Boolean
){

 Reports whether @rhombus(v1) and @rhombus(v2) are the same object.
 Being the @emph{same} is weakly defined, but only @rhombus(==) values
 can possibly be the same object, and mutable values are the same only if
 modifying one has the same effect as modifying the other. Interned
 values like symbols are @rhombus(===) when they are @rhombus(==).

@examples(
  #'apple === #'apple
  #'apple === #'banana
)

}

@doc(
  operator ((x :: Number) .= (y :: Number)) :: Boolean
){

 Reports whether @rhombus(x) and @rhombus(y) are numerically equal,
 where inexact numbers are effectively coerced to exact for
 comparisons to exact numbers. The value @rhombus(#nan) is not
 @rhombus(.=) to itself (but @rhombus(#nan) is @rhombus(==) to
 itself).

@examples(
  1 .= 1
  1 .= 2
  1.0 .= 1
)

}

@doc(
  operator ((v1 :: Any) != (v2 :: Any)) :: Boolean
){

 Equvalent to @rhombus(!(v1 == v2)).

@examples(
  "apple" != "apple"
)

}


@doc(
  operator ((v1 :: Any) is_now (v2 :: Any)) :: Boolean
){

 Reports whether @rhombus(v1) and @rhombus(v2) are equivalent @emph{now}
 in the sense that mutable fields of objects have the same values.

 Mutable and immutable strings, byte vectors, and arrays are considered
 the same if they have the same elements, even if one is mutable and the
 other is immutable. However, a mutable map or set is never considered
 equivalent to an immutable map or set, even if they have the same
 content.

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
  expr.macro '='
){

 The @rhombus(=) operator is not bound as an expression or binding
 operator. It is used as a syntactic delimiter by various forms, such as
 in @rhombus(fun) when specifying the default value for an optional
 argument.

}


@doc(
  interface Equatable
){

@provided_interface_and_namespace_only()

 An interface that a class can implement (publicly or privately) to
 customize the way its objects are compared and hashed. The interface has
 two methods:

@itemlist(

 @item{@rhombus(#,(@rhombus(equals, ~datum))(#,(@rhombus(other, ~var)), #,(@rhombus(recur, ~var))))
  --- takes another object that is an instance of the same class (or a
  subclass), and returns a non-@rhombus(#false) value if the objects
  are equal, @rhombus(#false) otherwise.
  Use the given @rhombus(recur, ~var) function to compare components of
  the two objects, instead of using @rhombus(==) on the components.}

 @item{@rhombus(#,(@rhombus(hash_code, ~datum))(#,(@rhombus(recur, ~var))))
  --- returns a hash code for the object, which is an exact integer. Use
  @rhombus(recur) to compute hash code for components of the object, and
  use the functions @rhombus(Equatable.hash_code_combine) and
  @rhombus(Equatable.hash_code_combine_unordered) to combine component
  hash codes.}

)

 A class gets a default equality implementation that recurs to compare
 fields only if the class has no mutable fields, and that matches
 @rhombus(===) when the class has mutable fields. Normally,
 @rhombus(Equatable) should be implemented privately, so that the methods
 do not have to check what class @rhombus(other, ~var) instantiates or
 whether @rhombus(recur, ~var) is a function.

@examples(
  ~defn:
    class Posn(x, y):
      private field cache = #false
      method dist():
        cache || (block:
                    def v = math.sqrt(x*x + y*y)
                    cache := v
                    v)
      // since `cache` is mutable, the default equality
      // comparison would only make `===` objects equal
      private implements Equatable
      private override equals(other :: Posn, recur):
        recur(x, other.x) && recur(y, other.y)
      private override hash_code(recur):
        Equatable.hash_code_combine(recur(x), recur(y))
  ~repl:
    Posn(3, 4) == Posn(3, 4)
)

}

@doc(
  fun Equatable.hash(v :: Any) :: Int
){

 Returns a hash code for @rhombus(v) that is consistent with
 @rhombus(==).

}

@doc(
  fun Equatable.identity_hash(v :: Any) :: Int
){

 Returns a hash code for @rhombus(v) that is consistent with
 @rhombus(===).

}

@doc(
  fun Equatable.hash_code_combine(hc :: Int, ...)
    :: Int
  fun Equatable.hash_code_combine_unordered(hc :: Int, ...)
    :: Int
){

 Combines hash codes to produce a new one. Information is generally
 lost in the combination, but this combining function mixes integers
 in a suitable way to produce good results for hashing.

 See @rhombus(Equatable, ~class) for an example.

}
