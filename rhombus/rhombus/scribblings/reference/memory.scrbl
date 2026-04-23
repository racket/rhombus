#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/memory)

@title(~tag: "memory"){Memory Management}

@docmodule(~use_sources: lib("rhombus/memory.rhm"),
           rhombus/memory)

@(~version_at_least "8.14.0.4")

@doc(
  fun memory.gc() :: Void
  fun memory.minor_gc() :: Void
  fun memory.incremental_gc() :: Void
){

 Forces an immediate garbage collection:

@itemlist(

 @item{@rhombus(memory.gc) forces a major collection that inspects all generations.}

 @item{@rhombus(memory.minor_gc) function forces only a minor collection.}

 @item{@rhombus(memory.incremental_gc) function may perform a minor collection,
  but also requests incremental collection for future automatic
  collections. The request expires if it is not renewed frequently.}

)

}

@doc(
  fun memory.current_use() :: Int
  fun memory.cumulative_use() :: Int
){

 Returns the amount of memory allocated:

@itemlist(

 @item{@rhombus(memory.current_use) reports the number of bytes occupied
  by all currently allocated objects, not counting overhead, but including
  objects that might be reclaimed immediately by a garbage collection.}

 @item{@rhombus(memory.cumulative_use) reports the total number of bytes
  that have been allocated since the process started, including bytes that
  have been subsequently reclaimed by garbage collection.}

)

}

@doc(
  fun memory.order_acquire() :: Void
  fun memory.order_release() :: Void
){

 Those operations implement a machine-level memory fence on platforms
 where one is needed for synchronization. The
 @rhombus(memory.order_acquire) operation ensures at least a load–load
 and load–store fence at the machine level, and the
 @rhombus(memory.order_release) operation ensures at least a store–store
 and store–load fence at the machine level.

 See also @secref(~doc: model_doc, "memory-model").

}
