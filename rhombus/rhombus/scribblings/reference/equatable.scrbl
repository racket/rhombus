#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "Equatables"){Equatables}

Any value is @deftech{equatable}. Implementing the @rhombus(Equatable, ~class)
interface customizes the way that instances of a class are compared for
@rhombus(==) or @rhombus(is_now) and hashed for maps and sets that use
@rhombus(==, ~key_comp) or @rhombus(is_now, ~key_comp).

@doc(
  interface Equatable
){

@provided_interface_and_namespace_only()

 An interface that a class can implement (publicly or privately) to
 customize the way its objects are compared for @rhombus(==) or @rhombus(is_now) and hashed
 for maps and sets that use @rhombus(==, ~key_comp) or @rhombus(is_now, ~key_comp). The interface has
 two methods that must be overridden for @rhombus(==), and two methods for @rhombus(is_now)
 whose default implementations use the methods for @rhombus(==):

@itemlist(

 @item{@rhombus(#,(@rhombus(equals, ~datum))(#,(@rhombus(other, ~var)), #,(@rhombus(recur, ~var)))):
  Takes another object that is an instance of the same class (or a
  subclass), and returns a non-@rhombus(#false) value if the objects
  are equal in the sense of @rhombus(==), @rhombus(#false) otherwise.
  Use the given @rhombus(recur, ~var) function to compare components of
  the two objects, instead of using @rhombus(==) on the components.

  If a class has mutable fields whose state is externally observable,
  take care not to treat objects as @rhombus(==) if they should be
  equivalent only by @rhombus(is_now). Override both the
  @rhombus(equals, ~datum) and @rhombus(now_equals, ~datum) to distinguish
  @rhombus(==) and @rhombus(is_now) equality.

  If two objects are equal in the sense of @rhombus(===), then they are
  automatically also equal by @rhombus(==). An @rhombus(equals, ~datum)
  method implementation is never called on an obejct with itself as
  @rhombus(other, ~var).}

 @item{@rhombus(#,(@rhombus(hash_code, ~datum))(#,(@rhombus(recur, ~var)))):
  Returns a hash code for the object, which is an exact integer. The
  hash code should be consistent with equality via @rhombus(==). Use
  @rhombus(recur) to compute hash code for components of the object, and
  use the functions @rhombus(Equatable.hash_code_combine) and
  @rhombus(Equatable.hash_code_combine_unordered) to combine component
  hash codes.

  If a class has mutable fields, the hash code produced by
  @rhombus(hash_code, ~datum) normally should return the same result as
  @rhombus(Equatable.identity_hash). Override
  @rhombus(now_hash_code, ~datum) to implement hashing consistent with
  @rhombus(is_now).}

 @item{@rhombus(#,(@rhombus(now_equals, ~datum))(#,(@rhombus(other, ~var)), #,(@rhombus(recur, ~var)))):
  Like @rhombus(equals, ~datum), but for equality in the sense of @rhombus(is_now).
  The default implementation of this method calls the @rhombus(equals, ~datum) method;
  it passes along the given @rhombus(recur, ~var) function, which means that
  @rhombus(is_now) will used for nested comparisons (assuming that the called
  @rhombus(equals, ~datum) method uses @rhombus(recur, ~var) as it should).}

 @item{@rhombus(#,(@rhombus(now_hash_code, ~datum))(#,(@rhombus(recur, ~var)))):
  Like @rhombus(hash_code, ~datum), but for equality in the sense of @rhombus(is_now).
  The default implementation of this method calls the @rhombus(hash_code, ~datum) method;
  it passes along the given @rhombus(recur, ~var) function, which means that
  nested hashing will create @rhombus(is_now)-compatible results (assuming that the called
  @rhombus(hash_code, ~datum) method uses @rhombus(recur, ~var) as it should).}

)

 A class gets a default equality implementation that recurs to compare
 fields for @rhombus(==) only if the class has no mutable fields.
 When the class has mutable fields, is default equality implementation
 for @rhombus(==) uses @rhombus(===) comparisons. The default equality
 implementation always recurs to fields for @rhombus(is_now) comparisons.

 The @rhombus(Equatable, ~class) interface normally should be implemented
 @rhombus(private, ~class_clause)ly, so that the methods do not have to
 check what class @rhombus(other, ~var) instantiates or whether
 @rhombus(recur, ~var) is a function.

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
  fun Equatable.now_hash(v :: Any) :: Int
){

 Returns a hash code for @rhombus(v) that is consistent with
 @rhombus(is_now).

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
