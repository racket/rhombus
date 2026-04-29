#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/will)

@title(~tag: "will-executor"){Will Executors}

@docmodule(rhombus/will)

A @deftech{will executor} support finalization by maintaining a
collection of @defterm{will} functions associated to Rhombus objects. When
an object becomes unreachable, its will are ready to be executed. See
@secref(~doc: model_doc, "will-model") for more information.

A will executor can be used as a @tech{synchronizable event}. A will
executor is ready for synchronization when
@rhombus(will.Executor.execute) would not block, and the synchronization
result is the executor itself.

@doc(
  class will.Executor()
){

 Represents a @tech{will executor}.

}

@doc(
  method (executor :: will.Executor).register(
    v :: Any,
    v_will :: (Any) -> ~any
  ) :: Void
){

 Registers the value @rhombus(v) with the function @rhombus(v_will) in
 @rhombus(executor). When @rhombus(v) is proven nreachable (see
 @secref(~doc: model_doc, "gc-model")), then the function
 @rhombus(v_will) is ready to be called with @rhombus(v) as its argument
 via @rhombus(will.Executor.execute) or
 @rhombus(will.Executor.maybe_execute).

 The @rhombus(v_will) argument is strongly referenced by
 @rhombus(executor) until it is executed, which is why @rhombus(v) is
 supplied back to @rhombus(v_will) instead of expecting @rhombus(v) to be
 in @rhombus(v_will)'s closure.

 See @rhombus(will.Executor.execute) for an example.

}

@doc(
  method (executor :: will.Executor).execute()
  method (executor :: will.Executor).maybe_execute(
    ~none: none :: Any = #false
  )
){

 Invokes the will function for a single otherwise-unreachable value
 registered with @rhombus(executor). The values returned by the will
 procedure are the result of the @rhombus(will.Executor.execute) or
 @rhombus(will.Executor.maybe_execute) call.

 If no will is ready for immediate execution, @rhombus(will.Executor.execute)
 blocks until one is ready, while @rhombus(will.Executor.maybe_execute)
 returns @rhombus(none) immediately.

 The following example reflects a relatively simple pattern where will
 functions can run at any time, so a thread is created to run wills as
 soon as they become available.

@rhombusblock(
  block:
    def exor = will.Executor()
    def th:
      thread:
        while #true:
          exor.execute()
    def mutable box_to_track = Box(#false)
    exor.register(box_to_track,
                  fun (bx):
                    println("a-box is now garbage"))
    memory.gc()
    box_to_track := #false
    memory.gc()
    // printing is likely at this point
    Evt.system_idle.sync()
    #void
)

}
