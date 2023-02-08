Partial Order
==========================

Summary
-------

A generic interface for partial orders.

Motivation
----------

I have two main motivations:
 - Ordering for sorted data structures.
 - Customizable equal-now-like behavior within Rhombus.

Note that most sorted data structures expect a total order,
not a partial order.
But a key type that implements a partial order can still be
useful if only a subset of keys are allowed which are all
comparable to each other: for example `Real` numbers only
have a partial order because NaN exists, but if they're
free to accept that and just error when they see NaN is
incomparable, they can still be very useful.

Design Options
--------------

### Option A: Comparison only, any resemblance to equality is entirely coincedental

Data definitions that want to implement partial orders only
need to implement one method: `partial_compare`.
Implementations of `partial_compare` are not expected to be
compatible with with `Equatable`, `==`, or Racket's `equal?`
in any way, and there is no hash code associated with this.

#### Interface to be implemented by data definitions privately

A `PartialOrdering` is a `Real` number:
 - Negative represents `<` less than
 - Zero represents `=` equal to
 - Positive represents `>` greater than
 - NaN represents incomparable

The interface `PartialOrder` is intended to be implemented
privately with the `partial_compare` method:

```
method partial_compare(self :: Self, other :: Imp, recur :: (Any, Any) -> PartialOrdering) :: PartialOrdering
```

The method implementation may assume that `other`'s method
implementation comes from the same type.

It is expected that mutable data structures can implement
`partial_compare` in a way that depends on mutable state,
and so ordering might change after mutation.

#### Operations to be used publicly

An `Ordering` is a comparable real number, excluding NaN:
 - Negative represents `<` less than
 - Zero represents `=` equal to
 - Positive represents `>` greater than

Functions:
 * `fun partial_compare(a, b) :: PartialOrdering`
   Follows type-specific `partial_compare` method implementations
   when the method implementations come from the same type.
   Produces NaN when they come from different types.
   When the types don't implement `PartialOrder`, it can
   use `equal?/recur` for a product order.
 * `fun compare(a, b) :: Ordering`
   Errors when `partial_compare` would produce NaN,
   otherwise produces the `Ordering` it would produce.

Operators:
 * `operator (a =~ b) :: Boolean` produces true when `partial_compare` produces zero, false when it produces something else (including NaN)
 * `operator (a !=~ b) :: Boolean` produces true when `partial_compare` produces something other than zero (including NaN), false on zero
 * `operator (a <~ b) :: Boolean` produces true when `compare` produces negative, false on positive or zero, and errors on errors (including errors when `partial_compare` produces NaN)
 * `operator (a >~ b) :: Boolean` produces true when `compare` produces positive, false on negative or zero, and errors on errors (including errors when `partial_compare` produces NaN)
 * `operator (a <=~ b) :: Boolean` produces true when `compare` produces negative or zero, false on positive, and errors on errors (including errors when `partial_compare` produces NaN)
 * `operator (a >=~ b) :: Boolean` produces true when `compare` produces positive or zero, false on negative, and errors on errors (including errors when `partial_compare` produces NaN)

### Option B: Comparison with hash, connected with equal-now

Data definitions that want to implement partial orders need
two methods: `partial_compare` and `compare_hash_code`.
Partial orders interact with Racket's `equal?/recur`
through product orders (not lexicographic orders).

#### Interface to be implemented by data definitions privately

The interface `PartialOrder` is intended to be implemented
privately with these methods:

```
method partial_compare(self :: Self, other :: Imp, recur :: (Any, Any) -> PartialOrdering) :: PartialOrdering

method compare_hash_code(self :: Self, recur :: (Any) -> Integer) :: Integer
```

The `partial_compare` method may assume that `other`'s
method implementation comes from the same type.

It is expected that mutable data structures can implement
partial orders in a way that depends on their mutable state,
and so ordering might change after mutation.

The `compare_hash_code` method can error when an element
isn't comparable to anything, not even itself (such as NaN).

#### Operations to be used publicly

Functions:
 * All the same functions from Option A: Comparison only
 * `fun compare_hash_code(a) :: Integer`
   Follows type-specific `compare_hash_code` method
   implementations, behaves compatibly with `.=` on  real
   numbers, and on other types, follows hash code methods
   from their equality implementations.
 * Racket's `equal?` and `equal-hash-code` can use these
   methods as well.

Operators:
 * All the same operators from Option A: Comparison only

### Option C: Comparison with key, connected with equal-now via hash-proc recur

Data definitions that want to implement partial orders can
either just implement the `compare_key` method, or
implement both `compare_key` and `partial_compare`.
Partial orders interact with Racket's `equal?/recur`
through product orders (not lexicographic orders).

#### Interface to be implemented by data definitions privately

The interface `PartialOrder` is intended to be implemented
privately with these methods:

```
method compare_key(self :: Self) :: Any

method partial_compare(self :: Self, other :: Imp, recur :: (Any, Any) -> PartialOrdering) :: PartialOrdering
```

With the `partial_compare` method optional.

The `partial_compare` method may assume that `other`'s
method implementation comes from the same type.

It is expected that mutable data structures can implement
partial orders in a way that depends on their mutable state,
and so ordering might change after mutation.

The `compare_key` method can error when an element isn't
comparable to anything, not even itself (such as NaN).

#### Operations to be used publicly

Functions:
 * All the same functions from Option B: Comparison with hash

Operators:
 * All the same operators from Option B: Comparison with hash

Evaluation and Tradeoffs
------------------------

Option A is relatively simple and easier to implement than
Option B or C.

Option B and C provide more functionality with hash codes
and support for Racket's `equal?`.
But options B and C would probably require building this
into the core of Racket so that they can access `hash-proc`
methods with their recur arguments.

Between B and C, B is simpler to implement but slightly
harder for users.
While C is slightly trickier to implement but easier for
users.

Prior art and References
------------------------

The `compare_key` method in Option C is inspired by
[countvajhula's proposal](https://github.com/racket/rhombus-prototype/tree/master/design/equality)
to use a `key` method to define equality.

The use of negative/zero/positive numbers for less/equal/greater is inspired by 2 sources:
 * Gerbil Scheme's
   [Red-Black Trees](https://cons.io/reference/misc.html#red-black-trees)
   and their `cmp` argument.
 * Alexis King's
   [`alexis/util/comparator`](https://docs.racket-lang.org/alexis-util/Untyped_Utilities.html#%28part._.Comparison_.Predicate_.Generation%29)
   module, `define-comparison-predicates` macro.

The operater name `=~` is inspired by [Pyret's equal-now](https://www.pyret.org/docs/latest/equality.html).

Drawbacks and Unresolved questions
----------------------------------

Yet another equality predicate `=~`?
When there are already several of them, adding another
might be considered confusing.

Contributors
------------

Alex Knauth
