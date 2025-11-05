#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Comparables}

A @deftech{comparable} value is one that supports @rhombus(<),
@rhombus(<=), @rhombus(>=),@rhombus(>=), @rhombus(compares_equal), and
@rhombus(compares_unequal). Real numbers, @tech{characters},
@tech{strings}, @tech{byte strings}, @tech{symbols}, @tech{keywords},
and @tech{cross-platform paths} are all comparable, as are instances of classes that
implement @rhombus(Comparable, ~class).

@doc(
  operator ((v1 :: Comparable) compare_to (v2 :: Comparable)) :: Int:
    ~order: order_comparison
  operator ((v1 :: Comparable) < (v2 :: Comparable)) :: Boolean:
    ~order: order_comparison
  operator ((v1 :: Comparable) > (v2 :: Comparable)) :: Boolean:
    ~order: order_comparison
  operator ((v1 :: Comparable) <= (v2 :: Comparable)) :: Boolean:
    ~order: order_comparison
  operator ((v1 :: Comparable) >= (v2 :: Comparable)) :: Boolean:
    ~order: order_comparison
  operator ((v1 :: Comparable) compares_equal (v2 :: Comparable))
    :: Boolean:
      ~order: order_comparison
  operator ((v1 :: Comparable) compares_unequal (v2 :: Comparable))
    :: Boolean:
      ~order: order_comparison
){

 Compares @rhombus(v1) and @rhombus(v2), which uses a primitive
 comparison operation for real numbers, characters, strings, byte strings,
 symbols, keywords, and cross-platform paths, or it calls the relevant
 method for a @rhombus(Comparable, ~class) instance.

 See also @rhombus(.<), @rhombus(.>), @rhombus(.<=), @rhombus(.>=),
 @rhombus(.=), and @rhombus(.!=), which are specific to numbers.

 The difference between @rhombus(compares_equal) and @rhombus(==) is
 that the former uses @rhombus(Comparable, ~class) while the latter uses
 @rhombus(Equatable, ~class). The results tend to be the same, but they
 are different in the case of numbers: two numbers are @rhombus(==) only
 when they have the same exactness, but @rhombus(compares_equal)
 corresponds to @rhombus(.=).

 The @rhombus(use_static) declaration constrains @rhombus(<), etc., to
 work only when the left-hand argument or right-hand argument has static
 information indicating that it satisfies @rhombus(Comparable, ~annot).
 If both expressions provide static information, the
 @rhombus(Comparable, ~annot) specifications must be compatible: both
 identifying the same operation, or one specifying the generic
 @rhombus(Comparable, ~class) operation.

 The comparison implementations for @rhombus(Number, ~annot) arguments
 are specialized like @rhombus(+) for arguments with
 @rhombus(Flonum, ~annot) static information. That specialization does
 not mean that a @rhombus(Number, ~annot) argument combined with a
 @rhombus(Flonum, ~annot) argument will trigger an ambiguous-comparison
 error in static mode, however.

@examples(
  ~repl:
    1 < 2
    "apple" <= "banana"
    #'banana >= #'apple
    #'~banana > #'~apple
  ~repl:
    use_static
    ~error:
      1 < "apple"
)

}



@doc(
  interface Comparable
){

@provided_interface_and_other_annotation_only()

 An interface that a class can implement (publicly or privately) to make
 instances of the class work with @rhombus(<), @rhombus(>), etc. As an
 annotation, @rhombus(Comparable, ~annot) matches all @tech{comparable}
 objects, not just instances of classes that publicly implement the
 @rhombus(Comparable, ~class) interface.

 The interface has one abstract method, @rhombus(compare_to, ~datum),
 and six methods that use @rhombus(compare_to, ~datum) by default but can
 be individually overridden:

@itemlist(

 @item{@rhombus(#,(@rhombus(compare_to, ~datum))(#,(@rhombus(other, ~var))))
  --- the @rhombus(other, ~var) value is the right-hand argument to a
  comparison, and it is always an instance of the same class or a subclass
  that inherits the same @rhombus(compare_to, ~datum) implementation. The
  result must be an integer: negative if the object is less than the
  @rhombus(other, ~var), positive if the object is greater than
  @rhombus(other, ~var), and zero if they are equal.}

 @item{@rhombus(#,(@rhombus(less, ~datum))(#,(@rhombus(other, ~var))))
  --- takes the same kind of argument as @rhombus(compare_to, ~datum), but
  returns a boolean to indicate whether the object is less than
  @rhombus(other, ~var), and with the further constraint that the two
  objects have the same @rhombus(less, ~datum) implementation. This is the
  method called to implement @rhombus(<), but the default implementation
  of this method uses @rhombus(compare_to, ~datum).}

 @item{@rhombus(#,(@rhombus(less_or_equal, ~datum))(#,(@rhombus(other, ~var))))
  --- like @rhombus(less, ~datum), but for @rhombus(<=).}

 @item{@rhombus(#,(@rhombus(greater, ~datum))(#,(@rhombus(other, ~var))))
  --- like @rhombus(less, ~datum), but for @rhombus(>).}

 @item{@rhombus(#,(@rhombus(greater_or_equal, ~datum))(#,(@rhombus(other, ~var))))
  --- like @rhombus(less, ~datum), but for @rhombus(>=).}

 @item{@rhombus(#,(@rhombus(compares_equal, ~datum))(#,(@rhombus(other, ~var))))
  --- like @rhombus(less, ~datum), but for @rhombus(compares_equal).}

 @item{@rhombus(#,(@rhombus(compares_unequal, ~datum))(#,(@rhombus(other, ~var))))
  --- like @rhombus(less, ~datum), but for @rhombus(compares_unequal).}

)

@examples(
  ~defn:
    class Posn(x, y):
      private implements Comparable
      private override method compare_to(other :: Posn):
        let delta = x - other.x
        if delta == 0
        | y - other.y
        | delta
  ~repl:
    Posn(1, 2) < Posn(2, 1)
    Posn(1, 2) < Posn(1, 3)
    Posn(1, 2) < Posn(1, 0)
)

}
