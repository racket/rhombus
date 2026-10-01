#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      ffi/finalize
      draw
      rhombus/memory)

@(rhombus_typeset '_m1': '@rhombus(m1, ~var)')
@(rhombus_typeset '_m2': '@rhombus(m2, ~var)')
@(rhombus_typeset '_v': '@rhombus(v, ~var)')

@title(~tag: "gcable-pointers"){Rules for Referencing GC-Managed Objects}

As noted in @secref("tutorial-memory"), Rhombus's memory manager relies
on garbage collection to reclaim memory of objects that are no longer
reachable from other objects. @defterm{Object} here means any
representation that is allocated in memory, not just a @rhombus(class)
instance.

@deftech{Tracing} is the garbage collector's process of traversing
objects to find references to allocated objects. The garbage collector
never traces memory that it does not manage itself, including stack
frames and registers in use by foreign functions. When tracing, the
garbage collector may also move an object that remains allocated so
that it resides at a new address in memory; the garbage collector
updates references to the moved object accordingly, but it cannot
update a reference that it does not trace. These properties of
Rhombus's memory management mean that care is needed when passing
pointers that originate in Rhombus to foreign functions.

@section{GCable Pointers}

A @deftech{gcable pointer} object represents a reference to memory
that is managed by Rhombus's garbage collector. That memory (which is
itself an allocated object separate from the pointer object), will
remain allocated only as long as some traceable reference exists to
the allocated memory, typically in the form of a gcable
pointer.@margin_note{The phrase @defterm{gcable pointer object} is
arguably a misnomer, since @defterm{gcable} is a property of the
object referenced by the pointer, not the pointer object itself (which
is always managed by the garbage collector). The more accurate phrase
@defterm{reference-to-gcable-object pointer object} is too long.}
Allocating memory through @rhombus(new) or @rhombus(malloc) with any allocation
mode other than @rhombus(~manual) returns a gcable pointer.

More precisely, a gcable pointer @emph{claims} to reference
garbage-collected memory. Conversion of a pointer from C does not try
to infer whether the pointer refers to memory that is managed by
Rhombus's garbage collector. Instead, allocation via @rhombus(new)
or @rhombus(malloc) or conversion of a pointer with a type like
@rhombus_t(ptr_t/gcable) creates a gcable pointer object that claims to
reference garbage-collected memory, while conversion with a type like
@rhombus_t(ptr_t) or allocation with @rhombus(~manual) creates a
non-gcable pointer object that makes no such claim.

@section{Conversion Between GCable and Non-GCable Pointers}

Providing a gcable pointer as an argument to a C function effectively
converts it to a non-gcable reference. Thus, for a @tech{foreign
callout} argument or a @tech{foreign callback} result, @rhombus_t(ptr_t)
and @rhombus_t(ptr_t/gcable) are equivalent. For a callout result or
callback argument, however, @rhombus_t(ptr_t/gcable) claims that the
result or argument is a reference to Rhombus-managed memory, and a
gcable pointer is created to represent the result or argument.

A gcable pointer may be safety converted to a non-gcable pointer as
long as the garbage collector will not run while the non-gcable
pointer is used. In particular, garbage collection is prevented during
a foreign callout unless the @rhombus(~collect_safe) option is used or
the callout triggers a Rhombus-implemented callback.

Allocation with @rhombus(~gcable_immobile) or
@rhombus(~gcable_traced_immobile) produces a gcable pointer that may
be safety converted to a non-gcable pointer, as long as the referenced
allocated object remains referenced (e.g., through a retained gcable
pointer) so that it is not deallocated. A non-gcable pointer can be
safely converted to a gcable pointer when it is known to refer to an
immobile object managed by the garbage collector. A reference to
memory not managed by Rhombus also can be converted to a gcable pointer,
but with care: as long as the gcable pointer object itself is
allocated, the referenced memory must remain allocated outside of
Rhombus's management (so that the reference memory is not taken over by
Rhombus's memory manager).

Conversion of a pointer via @rhombus(cast) preserves its status as
a gcable pointer: the result of @rhombus(cast) is a gcable pointer
if and only if its argument is a gcable pointer. For the rare case
that a conversion between a gcable pointer and non-gcable pointer
makes sense, use @rhombus(ptr_to_gcable_ptr) or
@rhombus(gcable_ptr_to_ptr).

@section{Tracing and GCable Pointers}

A gcable pointer continues to refer to a garbage-collectable object if
the referenced object is moved by Rhombus's memory manager. A gcable
pointer also prevents the object that it references from being
deallocated (i.e., collected). A non-gcable pointer that actually
refers to a garbage-collectable object can lose track of the
referenced object if it moves, and the non-gcable pointer does not
prevent the referenced object from being deallocated.

Memory allocated in @rhombus(~gcable_traced) or
@rhombus(~gcable_traced_immobile) mode can contain references to
object objects managed by the garbage collector. Such a reference
might be installed with @rhombus(mem *_m1 := _m2), for
example, where @rhombus(_m1) refers to memory allocated with
@rhombus(~gcable_traced) and @rhombus(_m2) is a gcable pointer. Since
@rhombus(_m1) is traced, it prevents the object referenced by
@rhombus(_m2) from being deallocated, and the reference is updated if
that object is moved by Rhombus's garbage collector. Such references
must be installed using @rhombus(_m2) itself as a gcable pointer so
that the reference can be tracked properly. Installing a reference to
@rhombus(_m2) into memory not managed by Rhombus's garbage collector
makes sense only when @rhombus(_m2) is immobile and otherwise retained.

@section{Pointers to Rhombus Values}

Rhombus objects such as pairs, strings, and large numbers are allocated
and managed by Rhombus's garbage collector, but the representations of
those objects are generally not recognized by foreign functions, so
there is little meaning to obtaining a pointer reference to those
objects.

The exceptions are @tech(~doc: ref_doc){byte strings} and
@tech(~doc: ref_doc){flonum arrays}, which are an array of @rhombus_t(uint8_t)s and
array of @rhombus_t(double_t)s, respectively. A pointer to that content can be
passed using @rhombus_t(bytes_ptr_t) or @rhombus_t(flonum_array_ptr_t), and an
expression such as @rhombus(cast ~from (bytes_ptr_t) ~to (ptr_t) _v) or
@rhombus(cast ~from (flonum_array_ptr_t) ~to (double_t*) _v) can
obtain a reference to byte string or flonum array content as a pointer
object.

@section{Pointer Arithmetic}

Pointer arithmetic via @rhombus(cast) with an
offset works for gcable pointers as well as non-gcable pointers. A
gcable pointer with a non-zero offset internally retains its reference
to the beginning of the referenced object, and that reference persists
through further @rhombus(cast) operations.
Any conversion of a gcable pointer (with a non-zero offset) to a
different representation, however loses the reference to the start of
the object. Passing the pointer as type @rhombus_t(ptr_t) in a foreign
call counts as a conversion, for example, as does installing the
pointer into a traced allocated object with @rhombus(mem .... := ....).

A traced reference into the middle of an allocated object is invalid
at a point where garbage collection is enabled---not just allowing the
referenced object to be deallocated, perhaps, but potentially crashing
or corrupting the Rhombus process if the converted reference is traced
(e.g., because it was converted back into a gcable pointer or because
it was into a traced object via @rhombus(mem .... := ....). That restriction
applies even if the referenced object is immobile, but it applies only
to traced references; if a non-gcable pointer is into the middle of an
immobile object that is otherwise retained, the non-gcable pointer
remains valid. A garbage collection cannot happen between the time of
a conversion for a callout and the callout's invocation, and it cannot
happen during the post-conversion (of inputs) portion of forms and
functions like @rhombus(mem),
@rhombus(memcpy), @rhombus(memmove), and @rhombus(memset).
