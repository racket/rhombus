#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      ffi/finalize)

@title(~tag: "finalize"){Finalization}

@docmodule(ffi/finalize)

@doc(
  fun finalize.register_finalizer(
    obj :: Any,
    finalizer :: Function.of_arity(1)
  ) :: Void
){

 Registers a finalizer procedure @rhombus(finalizer) for the given
 @rhombus(obj), which can be any Rhombus object. The @rhombus(finalizer)
 procedure is called with @rhombus(obj) as its argument sometime after
 the point where no safe Rhombus code can acess @rhombus(obj).

 More precisely, the finalizer is registered with a ``late'' will
 executor that makes wills ready for a value only after all weak
 references (such as in a weak box) for the value have been cleared,
 which implies that the value is unreachable and no normal will executor
 has a will ready for the value. The finalizer is invoked when the will
 for @rhombus(obj) becomes ready in the ``late'' will executor, which
 means that the value is unreachable (even from wills, and even from
 itself) by safe code.

 The finalizer is invoked in a thread that is in charge of triggering
 will executors for @rhombus(finalize.register_finalizer). The given
 finalizer procedure should generally not rely on the environment of the
 triggering thread, and it must not use any parameters or call any
 parameter functions, except that relying on a default logger and/or
 calling @rhombus(Logger.current) is allowed.

 Finalizers are mostly intended to be used with @tech{pointer} objects
 (for freeing unused memory that is not under GC control), but it can be
 used with any Rhombus object---even ones that have nothing to do with
 foreign code. Note, however, that the finalizer is registered for the
 Racket object that represents the pointer. If you intend to free a
 pointer object, then you must be careful to not register finalizers for
 two pointers that point to the same address. Also, be careful to not
 make the finalizer a closure that holds on to the object. Finally,
 beware that the finalizer is not guaranteed to be run when Rhombus
 exits.

}

@doc(
  fun finalize.allocator(
    dealloc :: maybe(Function.of_arity(1)),
    ~merely_uninterruptible: merely_uninterruptible :: Any.to_boolean = #false
  ) :: Function.assume_of((proc :: maybe(Function)) -> Any.like(proc))
){

 Returns a function @rhombus(make_allocator, ~var). The generated
 @rhombus(make_allocator, ~var) function, in turn, takes an argument
 @rhombus(f, ~var) and returns an @rhombus(allocator, ~var) procedure that
 behaves like @rhombus(f, ~var): it accepts the same arguments, and it
 produces the same result. Each result @rhombus(v, ~var) from the
 @rhombus(allocator, ~var), if not @rhombus(#false), is registered with a
 finalizer that calls @rhombus(dealloc) on @rhombus(v, ~var)---unless the
 call has been canceled by applying a deallocator (produced via
 @rhombus(finalize.deallocator)) to @rhombus(v, ~var), in which case any
 existing @rhombus(dealloc) registered for @rhombus(v, ~var) is canceled.
 If and only if @rhombus(f, ~var) is @rhombus(#false),
 @rhombus(foreign.allocator(dealloc)(alloc)) produces @rhombus(#false).

 The resulting @rhombus(allocator) calls @rhombus(f, ~var) in
 @tech{atomic mode}, unless @rhombus(mereley_uninterruptible) is true, in
 which case @tech{uninterruptible mode} is used. The result from
 @rhombus(f, ~var) is received and registered in atomic or
 uninterruptible mode, so that the result is reliably deallocated as long
 as no exception is raised.

 The @rhombus(dealloc) procedure will be called in atomic mode, and it
 must obey the same constraints as a finalizer procedure provided to
 @rhombus(finalize.register_finalizer). The @rhombus(dealloc) procedure
 itself need not be a deallocator produced via
 @rhombus(finalize.deallocator). Along the same lines, if a deallocator
 is called explicitly on a @rhombus(v, ~var) produced by
 @rhombus(allocator, ~var), it need not be the same as that
 @rhombus(allocator, ~var)'s @rhombus(dealloc, ~var).

 When a non-main place exits, after all custodian-shutdown actions, for
 every @rhombus(dealloc) still registered via an
 @rhombus(allocator, ~var), the value to deallocate is treated as
 immediately unreachable. At that point, @rhombus(dealloc, ~var)
 functions are called in reverse order of their registrations. Note that
 references in a @rhombus(dealloc) function’s closure do not prevent
 running a @rhombus(dealloc) function for any other value. If
 deallocation needs to proceed in an order different than reverse of
 allocation, use a retainer (via @rhombus(finalize.retainer)) to insert a
 new deallocation action that will run earlier.

}

@doc(
  fun finalize.deallocator(
    get_arg :: Function.of_arity(1) = PairList.first,
    ~merely_uninterruptible: merely_uninterruptible :: Any.to_boolean = #false
  ) :~ Function.assume_of((proc :: Function) -> Any.like(proc))
){

 Returns a function @rhombus(make_deallocator, ~var). The generated
 @rhombus(make_deallocator, ~var) function, in turn, takes an argument
 @rhombus(f, ~var) and produces a @rhombus(deallocator, ~var) procedure
 that behaves like @rhombus(f): it accepts the same arguments and returns
 the same result. The @rhombus(deallocator, ~var) function calls
 @rhombus(dealloc) in @tech{atomic mode} or @tech{uninterruptible mode},
 and for one of its arguments, the it cancels the most recent remaining
 deallocator registered by an allocator or retainer.

 The optional @rhombus(get_arg) procedure determines which of
 @rhombus(f, ~var)’s arguments correspond to the released object;
 @rhombus(get_arg) receives a @rhombus(PairList, ~annot) of arguments
 passed to @rhombus(deallocator), so the default @rhombus(PairList.first)
 selects the first one. Note that @rhombus(get_arg) can only choose one
 of the by-position arguments to @rhombus(deallocator, ~var), although
 the @rhombus(deallocator, ~var) function will require and accept the
 same keyword arguments as @rhombus(f, ~var), if any.

}
