#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/runtime)

@title(~tag: "memory"){Memory Management}

@docmodule(~use_sources: lib("rhombus/runtime.rhm"),
           rhombus/runtime)

@doc(
  fun runtime.gc() :: Void
  fun runtime.minor_gc() :: Void
  fun runtime.incremental_gc() :: Void
){

 Forces an immediate garbage collection:

@itemlist(

 @item{@rhombus(runtime.gc) forces a major collection that inspects all generations.}

 @item{@rhombus(runtime.minor_gc) function forces only a minor collection.}

 @item{@rhombus(runtime.incremental_gc) function may perform a minor collection,
  but also requests incremental collection for future automatic
  collections. The request expires if it is not renewed frequently.}

)

}

@doc(
  fun runtime.current_memory_use() :: Int
  fun runtime.cumulative_memory_use() :: Int
){

 Returns the amount of memory allocated:

@itemlist(

 @item{@rhombus(runtime.current_memory_use) reports the number of bytes occupied
  by all currently allocated objects, not counting overhead, but including
  objects that might be reclaimed immediately by a garbage collection.}

 @item{@rhombus(runtime.cumulative_memory_use) reports the total number of bytes
  that have been allocated since the process started, including bytes that
  have been subsequently reclaimed by garbage collection.}

)

}
