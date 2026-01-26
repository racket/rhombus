#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      rhombus/wrapper)

@title(~tag: "wrapper"){Rhombus Objects and Racket Handles}

@docmodule(rhombus/wrapper)

The @rhombusmodname(rhombus/wrapper) library provides support for a
common Rhombus library pattern where a Rhombus class wraps a Racket
representation. The wrapper class defines methods and properties for a
Rhombus-like view on the underlying Racket object. Typically, the
Rhombus class includes a @rhombus(handle, ~datum) property that returns
the wrapper Racket representation, while a @rhombus(from_handle, ~datum)
function creates a Rhombus wrapper object given a Racket representation.

Here's a sketch of a typical class, which has a private
@rhombus(_handle, ~datum) field, a constructor to create a Racket-level
object through an imported @rhombus(#{make-fish}) function, and a
@rhombus(from_handle, ~datum) method that uses an imported
@rhombus(#{fish?}) function to check whether the handle is
acceptable---throwing an exception with @rhombus(wrapper.error) if not.
Note that the placement of the @rhombus(from_handle) function at the end
of the @rhombus(Fish) class declaration is important for allowing the
result annotation @rhombus(:~ Fish).

@rhombusblock(
  class Fish(private _handle):
    internal _Fish
    export:
      from_handle

    constructor (....):
      super(#{make-fish}(....))

    property handle: _handle

    fun from_handle(handle) :~ Fish:
      ~who: who
      unless #{fish?}(handle)
      | wrapper.error(~who: who, ~what: "fish", handle)
      _Fish(handle)
)

@doc(
  fun wrapper.error(
    ~who: who :: error.Who,
    ~what: what :: maybe(String) = #false,
    handle
  ) :: None
){

 Throws an exception indicating that @rhombus(handle) is not an
 acceptable handle.

 Typically, there is no exposed predicate or annotation at the Rhombus
 layer to recognize acceptable handles, because the relevant Racket-level
 predicate is explained or implied by documentation in a way that does
 not create a direct cross reference (and therefore a documentation
 dependency). The thrown exception is nevertheless an instance of
 @rhombus(Exn.Fail.Annot, ~class) to

}
