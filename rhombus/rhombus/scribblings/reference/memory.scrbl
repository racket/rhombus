#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/memory)

@title(~tag: "memory"){Memory Management}

@docmodule(~use_sources: lib("rhombus/memory.rhm"),
           rhombus/memory)

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
