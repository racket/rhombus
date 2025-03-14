# Ranges

## Summary
This proposal discusses ranges, discrete or continuous, with or
without inclusive or exclusive upper/lower bounds.  Specifically, when
we speak of ranges, we refer to ranges as *objects* with a reasonable
interface.

## Motivation
A *range*, or an *interval*, represents a contiguous sequence of
values.  The use of ranges is widely common in programming due to its
very general idea—whenever a (partial) order is available, or in
Rhombus terms, a class implements `Comparable`, we can speak of
ranges.  As an example, the sub-list/string/… operation takes an
(optional) lower bound and an (optional) upper bound, which can as
well be represented by a range of non-negative integers.  This is not
only intentionally better than two ad hoc parameters `start` and
`end`, but also practically better as one no longer has to explicitly
verify that `start <= end`.

We can also speak of *discrete* ranges, which can be enumerated,
stepped, etc.  This is the notion of ranges that is more familiar to
most people: discrete ranges can be used as sequences by stepping from
the starting point to the ending point.  The above example, in fact,
is better expressed with discrete ranges, because the bounds must be
indexes, i.e., interpreted as natural numbers.

## Current Status

### Racket
Racket has no ranges in our sense.  The sequence constructor
`in-range` actually produces a sequence of (real!) numbers, with an
optional step.  In other words, Racket

```racket
(in-range start end step)
```

is somewhat like Haskell

```haskell
unfoldr
  (\x -> if (x < end) then Just (x, x + step) else Nothing)
  start
```

or, somewhat like C (except the lack of generic numerics)

```c
for (int i = start; i < end; i += step) {
    ....
}
```

*when `step` is non-negative* (use `>` instead when `step` is
negative).  The function `range` is just like `in-range`, but produces
a list.

There are also sequence constructors `in-inclusive-range`, which
includes the `end` (by using `<=`/`>=`), and `in-naturals`, which is
infinite, but curiously only applies to non-negative integers, i.e.,
naturals.

### Rhombus
Rhombus does what Racket does, but `start .. end` (`in-range`), `start
..= end` (`in-inclusive-range`), and `start ..` (`in-naturals`) only
allow (any) integers, and do not support custom `step`s.  Moreover,
`start .. end` and `start ..= end` are `Listable`.

## Prior Art

### Rust
Rust provides ranges of any type that implements `PartialOrd` (like
Rhombus `Comparable`).  Ranges are separated into six types:

- `Range`, which is half-open (`start .. end`);
- `RangeFrom`, which is bounded inclusively below (`start ..`);
- `RangeFull`, which is unbounded (`..`);
- `RangeInclusive`, which is closed (`start ..= end`);
- `RangeTo`, which is bounded exclusively above (`.. end`);
- `RangeToInclusive`, which is bounded inclusively above (`..= end`).

The syntax itself seems reasonable to implement in Rhombus, but the
semantics is more problematic.  Among these types, `Range`,
`RangeFrom`, and `RangeInclusive` can serve as `Iterator`s (like
Rhombus `Sequenceable`s), given that the item type implements `Step`
(no Rhombus equivalent, so far; let’s say this is `Steppable`).  This
is because an `Iterator` requires a starting point and a way to
proceed through steps.

Custom steps are not provided by ranges themselves, but are available
for all `Iterator`s through the `Iterator::step_by` method.

(Subtlety: The mentioned range types are `Iterator`s, as opposed to
`IntoIterator`s, because `Iterator::next` can mutate the `start`
field.  In Rhombus, ranges will more likely be `Sequenceable`s.)

Range types also implement `SliceIndex`, which can in turn implement
the sub-list/string/… operation.

### Haskell
Haskell has no ranges in our sense (indeed, Haskell favors a different
organization principle).  However, two Haskell type classes are
particularly relevant.

The `Enum` type class identifies types that can be enumerated in the
sense of mapping from and to natural numbers; in other words, `toEnum`
and `fromEnum` comprise a minimal complete definition.  Therefore,
`succ`essor and `pred`ecessor methods are provided, as well as methods
backing range-like notations.  Specifically,

- `[n ..]` is `enumFrom n`;
- `[n, n' ..]` is `enumFromThen n n'`;
- `[n .. m]` is `enumFromTo n m`; and
- `[n, n' .. m]` is `enumFromThenTo n n' m`.

The `Ix` type class identifies types that can serve as “indexes,”
therefore closer to ranges in our sense.  Specifically, a range is
represented as `(a, a)` where `Ix a`, and laws ensure that the methods
preserve this fact.

### Guava
Guava provides `Range<C extends Comparable>` class, which has nine (!)
constructors according to

- + Whether there is a lower bound;
  + If so, whether that is open;
- + Whether there is an upper bound;
  + If so, whether that is open.

`BoundType`s are represented by an `Enum` with two variants.

Additionally, when given a `DiscreteDomain<C>` (i.e., `Steppable`),
the `Range<C>` can be viewed as `ContiguousSet<C>`, therefore
`Iterable<C>`.

### Elixir
Elixir provides integer ranges.  The interesting part is the
notations: `first .. last` and `first .. last // step`, the latter
being a mixfix notation.

### Ruby
Ruby provides `Range`s of any object with a `<=>` operator (i.e.,
`Comparable`).  Moreover, a `succ` method (i.e., `Steppable`) is
required to treat the range as a sequence.

### Python
Python provides `range`s of any object that implements the
`__index__()` special method, which converts to integers; in other
words, only discrete ranges.

Alternatively, Python provides `slice`s (with no obligation!), which
back the slice notations `seq[:]`, `seq[start:]`, `seq[:end]`, and
`seq[start:end:step]`.

### Summary
Generally, two interfaces seem to be relevant for generic ranges:

- `Comparable`, with which the very basic inclusion operation can be
  defined;
- `Steppable`, with which the notion of discrete ranges can be
  represented.

In principle, a total of nine types of ranges can be defined, but the
usual expression problem applies.

Ranges can be used for many purposes.  Some use ranges for slicing;
some use ranges as sequences; still some use ranges in data
structures.

## Proposed Design
It’s not very clear how to make ranges generic while still efficient,
so let’s confine ourselves to integer ranges, for now.

Ranges will have nine types, with the following notations (*edited on
14th May*):

- ~~`Range.from_below(start, end)`~~
  `Range.from_to(start, end)`, which is closed–open `start .. end`;
- ~~`Range.from_to(start, end)`~~
  `Range.from_to_inclusive(start, end)`, which is closed–closed
  `start ..= end`;
- `Range.from(start)`, which is closed–unbounded `start ..`;
- ~~`Range.above_below(start, end)`~~
  `Range.from_exclusive_to(start, end)`, which is open–open
  `start <..< end`;
- ~~`Range.above_to(start, end)`~~
  `Range.from_exclusive_to_inclusive(start, end)`, which is
  open–closed `start <..= end`;
- ~~`Range.above(start)`~~
  `Range.from_exclusive(start)`, which is open–unbounded `start <..<`;
- ~~`Range.below(end)`~~
  `Range.to(end)`, which is unbounded–open `.. end`;
- ~~`Range.to(end)`~~
  `Range.to_inclusive(end)`, which is unbounded–closed `..= end`;
- `Range.full()`, which is unbounded–unbounded `..`.

~~The terms `from`–`to` vs. `above`–`below` for inclusive
vs. exclusive are inspired by Common Lisp `loop`~~
The name `Range.from_to` is chosen for the common case `..`, and
other names build on it; the operators `<..<` (cannot end in `.`) and
`<..=` have no precedent, but resemble `..=` in a way.  For
simplicity, prefer `..` over `..=` over `<..<`/`<..=` for unbounded
sequences.

The expression forms will have corresponding binding forms, as
expected.  Three annotations will be available:

- `Range`, satisfied by all range types;
- `SequenceRange`, satisfied by `Range.from`, `Range.from_below`, and
  `Range.from_to` (that implement `Sequenceable`);
- `ListRange`, satisfied by `Range.from_below` and `Range.from_to`
  (that implement `Listable`).

Specifically, range types that have a “starting point” implement
`Sequenceable`, and ones that have an “ending point” implement
`Listable`.

These methods seem reasonable for ranges, borrowed from Rebellion
(*edited on 9th May*):

- ~~`range.contains(v)`: checks whether `range` contains `v`;~~
- `range.has_element(i)`: checks whether `range` has `i` in its range;
- ~~`range.encloses(other)`: checks whether `range` encloses `other`;~~
- `range.encloses(other, ...)`: checks whether `range` and `other`s
  are in an enclosing order;
- `range.is_connected(other)`: checks whether `range` is connected to
  `other`;
- `range.overlaps(other)`: checks whether `range` overlaps with
  `other`;
- ~~`range.span(other)`: returns the range that spans `range` and
  `other`~~;
- `range.span(other, ...)`: returns the range that spans `range` and
  `other`s;
- `range.gap(other)`: returns the gap between `range` and `other`, if
  any;
- ~~`range.intersection(other)`: returns the intersection of `range` and
  `other`.~~
- `range.intersect(other, ...)`: returns the intersection of `range`
  and `other`s, if any.

Four deconstructors are available on ranges (*added on 3rd May*):

- `range.start()`: returns the starting point, which is an integer or
  `#neginf`;
- `range.end()`: returns the ending point, which is an integer or
  `#inf`;
- `range.includes_start()`: returns `#true` if `range` includes the
  starting point, `#false` otherwise;
- `range.includes_end()`: returns `#true` if `range` includes the
  ending point, `#false` otherwise.

These methods are available on `SequenceRange`, additionally:

- `range.to_sequence()`: returns a sequence, like `in-range` with a
  step of 1;
- `range.step_by(step)`: returns a sequence, like `in-range` with a
  custom `step`.

The name `step_by` is borrowed from Rust, but restricted to ranges,
for now.  This allows a naïve optimization strategy.

These methods are available on `ListRange`, additionally:

- `range.to_list()`: returns a list.

## Future Possibilities
Ranges can be made generic, but it’s not clear how to do that.  Does
anyone have an idea that can work well in the context of Rhombus?

The interface `Steppable` seems rather limited in functionality.
Indeed, Rust puts the corresponding interface under `std::iter`
module, which indicates that they also think such interface is
intended for (generic) iterators.  Should we implement that?

The method `Range.step_by` points to a more general version for all
sequences, and there are more such methods that can be defined.  How
should we do so?  As sequence combinators (however those will work)?
As `Sequenceable` methods?
