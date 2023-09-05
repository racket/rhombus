Generic Immutable Lists
=======================

This document proposes a design for generic immutable lists in Rhombus, as well as a plan for implementing them.

Motivation
----------

Lists come in many flavors, with many different performance characteristics. Forces programmers to interact with each of these flavors with different APIs leads to complexity, confusion, and inconsistencies. It hampers expressive power, as algorithms that operate on lists must tie themselves to specific implementations. Performance also suffers, as a lack of support for the most optimal list implementation for a task can lead programmers to choose suboptimal implementations instead. A generic interface for list processing can solve some of these problems.

Out of scope
------------

This design document focuses solely on immutable lists, and leaves open the design of mutable lists as well as other collections. Discussion on these topics can be found in #201 and #221.

Summary
-------

A `List` interface is provided from a `rhombus/list` module alongside various implementations of it. The basic structure of the interface is this:

```
interface List:
    method get(index)
    method set(index, element)
    method add(element) # Adds to the end of the list
    method insert(index, element)
    method remove(element) # Removes first occurrence of element from list
    method removeAt(index)
    method sublist(start, end) # Returns a view
```

Various other methods that can be derived from these may be included, but this is the core interface that list implementations must supply. The following implementations will be provided:

- `ConsList`, which is what the current Rhombus `List` implementation is. This is primarily intended for use in the Rhombus implementation itself, to avoid dependencies on collection implementations. The existing `List` implementation will be renamed to `ConsList`.
- `PersistentList`, an immutable list backed by an RRB-tree that supports efficient reads and (functional) writes.
- `ArrayList`, an immutable list backed by a Racket vector. This is also roughly what the current Rhombus `Array` implementation is, with the exception that a Rhombus `Array` can be mutable or immutable. The `ArrayList` `List` implementation is solely for lists backed by immutable arrays.
- `EmptyList` and `SingletonList`, specialized implementations for empty lists and single-element lists that avoid the overheads of trees and arrays.

Rather than ask users to carefully consider their use case and select the appropriate implementation every time a list is needed, the `List` interface will provide mechanisms to intelligently select a desirable implementation:

- A `List.builder()` function for creating a *list builder*, a write-only object which allows mutably constructing a list with its `add()` method. Calling `build()` on a builder produces an immutable list backed by an `ArrayList`. Using builders avoids the overhead of the tree structures in `PersistentList` entirely. Insertion into the builder can avoid allocation too by using the standard approach of maintaining an internal mutable array that is doubled in size as elements are inserted. The builder can also decide whether to use `EmptyList` or `SingletonList` based on the number of inserted elements at the time `build()` is called.
- Whenever the update methods on an `ArrayList`, `EmptyList`, or `SingletonList` are called, the underlying list will be copied into a `PersistentList`. This allows efficient functional updates on the lists created by a list builder with an amortized constant cost.
- The bracketed list construction notation `[a, b, c]` will be equivalent to `List.builder().add(a).add(b).add(c).build()`.

These choices are based on the following assumptions:

- Most lists are created, then read and iterated. Interleaved reads and updates to a list are rare.
- When interleaved reads and writes *are* necessary, the initial starting list is usually small, frequently the empty list or a singleton list.
- Iterating and accessing flat arrays is faster than cons lists and RRB trees.

RRB Trees
---------

TODO

Sublist Views
-------------

TODO

Indexing Notation
-----------------

TODO

Module Organization
-------------------

TODO

Batch Processing Methods
------------------------

TODO

Sequence Interface
------------------

TODO

Examples
--------

TODO

Implementation Plan
-------------------

TODO
