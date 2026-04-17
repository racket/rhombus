#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Weak References}

A @deftech{weak box} is an object with a single value field, which can
be accessed from a box @rhombus(wb, ~var) as
@rhombus(#,(@rhombus(wb, ~var)).value), but
@rhombus(#,(@rhombus(wb, ~var)).value) can asynchronously change to
@rhombus(#false) if the memory manager determines that the value is not
otherwise @tech(~doc: model_doc){reachable}.

An @deftech{ephemeron} is similarly an object with a value field, but
also a key, where both a key and value are supplied when the ephemeron
is constructed. The value in an ephemeron @rhombus(eph, ~var) can be
accessed as @rhombus(#,(@rhombus(eph, ~var)).value), but
@rhombus(#,(@rhombus(eph, ~var)).value) can asynchronously change to
@rhombus(#false) if the memory manager determines that the @emph{key}
(as opposed to the value) is not otherwise
@tech(~doc: model_doc){reachable}. Crucially, an ephemeron's reference
to a key does not keep the key reachable, even if that key is reachable
via the ephemeron's value; that is, the key and value can be
simultaneously determined to be unreachable.

A @rhombus(WeakMutableMap) or @rhombus(WeakMutableSet) retains elements
weakly in the same sense as a @tech{weak box}.

@doc(
  annot.macro 'WeakBox'  
  fun WeakBox(val :: Any) :: WeakBox
  property (wb :: WeakBox).value :: Any
){

 The @rhombus(WeakBox, ~annot) annotation matches any @tech{weak box}.

 The @rhombus(WeakBox) constructor creates a weak box whose initial
 value is @rhombus(val), but which can be replaced by the memory manager
 at any time by @rhombus(#false) if @rhombus(val) becomes otherwise
 un@tech(~doc: model_doc){reachable}.

 The @rhombus(WeakBox.value) property accesses the current value from a
 weak box.

}

@doc(
  annot.macro 'Ephemeron'  
  fun Ephemeron(key :: Any, val :: Any) :: Ephemeron
  property (eph :: Ephemeron).value :: Any
){

 The @rhombus(Ephemeron, ~annot) annotation matches any @tech{ephemeron}.

 The @rhombus(Ephemeron) constructor creates an ephemeron initially
 referencing the given @rhombus(key) value is @rhombus(val). The memory
 manager can replace the ephemeron's @rhombus(val) any time by @rhombus(#false)
 if @rhombus(key) becomes otherwise un@tech(~doc: model_doc){reachable}, but the
 ephemeron's own reference to @rhombus(key) (directly or via
 @rhombus(val)) does not make the key reachable.

 The @rhombus(Ephemeron.value) property accesses the current value from
 an ephemeron.

}
