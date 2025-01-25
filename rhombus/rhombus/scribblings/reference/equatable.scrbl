#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "Equatables"){Equatables}

Any value is @deftech{equatable}. Implementing the @rhombus(Equatable, ~class)
interface customizes the way that instances of a class are compared for
@rhombus(==) and hashed for maps and sets that use @rhombus(==, ~key_comp).

@doc(
  interface Equatable
){

@provided_interface_and_namespace_only()

 An interface that a class can implement (publicly or privately) to
 customize the way its objects are compared for @rhombus(==) and hashed
 for maps and sets that use @rhombus(==, ~key_comp). The interface has
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
