RRFI [Draft]: Two-level Universal Scheme for Equality
=====================================================

.. contents:: :depth: 1

Proposal
--------

Summary
~~~~~~~

Model equality in the language on two levels -- the lower one consisting of "key" (e.g. built-in) types related by a primitive equality relation, and the higher level, of arbitrary values that may be mapped to the key types by means of key functions.

This allows the language to provide a universal equality relation that presents a simple and extensible interface.

Substance
~~~~~~~~~

The proposed scheme has the following components:

1. A sufficiently diverse set of built-in types that form the initial "key" types.
2. A primitive, sufficiently "fine" equality relation on the key types.
3. A means to extend the key types to include user-defined types.
4. A means to indicate key functions that map arbitrary values to key types for primitive comparison.

For (1), we will assume that the set of built-in types found in most programming languages are sufficient.

For (2), ``egal?`` (also known as ``equal-always?``) seems an appropriate choice.

For (3), a standard generic dispatch mechanism (such as generic interfaces in Racket) is suggested.

For (4), key functions are proposed as the standard way in which equality between arbitrary values should be defined -- either in providing a definition at the type level so it applies to all values of a particular (user-defined) type, or on an ad hoc basis anywhere that we want to provide a coarser definition of equality than = itself, for instance in APIs that employ a notion of equality.

We'll look at each of these in turn, but first, some terminology will help.

Terminology
-----------

In this scheme, at a high level, there are three sets of interest, ``K``, ``K+`` and ``T``, and there are functions that map between them. Let's define these.

* **The set of all types, T**: This is the set of all types either defined or definable in a language. The elements of this set are *types* rather than values.
* **Key type**: A built-in type provided by the language. The set of key types will be denoted ``K``.
* **Extended key types**: The set of key types augmented with user-defined types that have defined key functions. We will denote this set by ``K+``.
* **Extent of a type**: For a type ``t ∈ T``, the "extent" of the type is the set of all values that are instances of that type. We will denote this ``ε(t)``.
* **Key function**: A unary, single-valued function ``χ`` mapping a value of any type to a value in ``ε(K+)``, i.e. ``χ : ε(T) → ε(K+)``. Key functions either (1) extend the equality predicate to new types, or (2) specialize the equality predicate to a coarser definition.
* **Key chain**: The sequence of key functions mapping any type to ``K``, i.e. ``χ₁, χ₂, ..., χn`` such that ``χn . ... . χ₁ : T → K``. The members or "links" of a key chain are *types*, for instance, a key chain may look something like (Teacher, Person, Vector). But for convenience, we will also use the term to refer to the corresponding structure on individual values.
* **Immediate representative**: A value ``r ∈ ε(K+)`` that is the result of mapping an arbitrary value ``v ∈ ε(T)`` under a key function. We say that ``r`` is the immediate representative of ``v``.
* **Ground value**: Any value ``v ∈ ε(K)``, i.e. an instance of a key type.
* **Ground representative**: A ground value ``r ∈ ε(K)`` that is the result of mapping an arbitrary value ``v ∈ ε(T)`` under its key chain. We say that ``r`` is the ground representative of ``v``.

Components
----------

Diversity of Primitive Key Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The foundational intuition of the approach proposed here comes from a standard mathematical `result on equivalence relations <https://encyclopediaofmath.org/wiki/Kernel_of_a_function>`__, that every equivalence relation on a set has a corresponding function that expresses it. More precisely:

*For any equivalence relation == on a set E and a primitive notion of equivalence =, there exists a set M and a function f : E → M such that x == y is equivalent to f(x) = f(y).*

This is an abstract mathematical result that doesn't really tell us what the nature of the set M might be, or how the function f could be constructed[1], or the conditions under which an arbitrary set M' could serve as M here.

We can make this a little more tangible by observing that in programming languages, notions of equality partition along type boundaries. That is, values of the same type need not be equal, but values of disjoint types are always unequal. This gives rise to a more concrete conception of the set M – that it is composed of values of different types which each have their own notion of equality. We call these types over M "key types," and the equivalence relation = defined across the entire set M is composed from the equality relations over individual key types. A function f maps types on E to types on M, and is called a "key function."

For example, a key function could map a user-defined Event type with attributes "place" and "time" to a representative vector containing the place and time values, so that two events are considered equal if they have the same place and time, by virtue of the existing (primitive) notion of vector equality (presuming that vectors are a key type).

Empirically, the built-in types in most languages (such as Racket) appear to suffice for the needs of equality, and we will be making that assumption here. To substantiate this assumption, we motivate how these common built-in types represent a late stage in the richness of categories of types available to use as representatives:

* If the language has no innately comparable types to use as keys, then key functions don't have anything useful to return at all, and they can't be used to compare anything.
* If the language's only key types have small finite size like boolean? and fixnum?, then no key function will be able to compare bignums, strings, vectors, or other data of arbitrary size.
* If its key types have arbitrary size but are always serializable, then no key function will be able to compare opaque values according to information those values keep encapsulated, unless the key function is implemented as a generic method having local access to that information (although a comparator method would be an alternative here).

A language like Racket has types of arbitrary size including hashes, lists, strings, and numbers, and empirically, these can be composed to express a variety of comparisons, to the extent that no counter-example to a notion of equality being definable using key functions alone on these built-in types is known.

[1] The standard proof simply exhibits such a function by mapping from E to M via the quotient set E/R (i.e. the set of the equivalence classes presupposed on E) -- it does not say how this function could be computed.

Primitive Equality Predicate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``egal?`` (also known as ``equal-always?``) seems an appropriate choice as it is billed as "the finest equality predicate [a language should provide.]." See [1] for an implementation for Racket.

[1] https://github.com/racket/racket/pull/4076

Extending the Key Types
~~~~~~~~~~~~~~~~~~~~~~~

This could be done using a generic interface resembling the existing ``gen:equal+hash``. Instead of specifying a binary comparator together with hash functions, we suggest employing the key function approach here as it improves on the comparator approach in a number of ways.

1. With a binary comparator, there are no correctness guarantees regarding the definition of equality provided for a particular type. Indeed, the definition could violate reflexivity (via ``a != a``), symmetry (via ``a = b`` but ``b != a``), or transitivity (via ``a = b`` and ``b = c`` but ``a != c``), the three properties commonly taken to define valid equivalence relations. This could lead to surprising behavior as equality is broadly assumed to have these properties. With a key function, reflexivity, symmetry, and transitivity are implicitly guaranteed.

2. A binary comparator is a familiar way to define equality, with both objects available for manual comparison. Though it is familiar, it is not as simple as one might expect. In addition to the two input arguments, the user must also manage another binary predicate to be used for any recursive comparisons that may be necessary. In contrast, a key function involves a single argument, and requires the user to select a representative value from the key types. This may be a less familiar way to define equality, but it is also objectively simpler, and does not require that the user manage a predicate for recursive comparison.

3. The Racket implementation of ``gen:equal+hash`` employs double hashing, a standard technique to resolve hash collisions. This means that users must implement not one but two hash functions, exposing the hashing mechanism to the user who may be ill-equipped to grapple with the technicalities of writing good hash functions. Not to mention, there is once again the need for users to manage hash procedures used in recursive hash computation. In contrast, with key functions, the hashing mechanism is completely hidden from the user.

4. With a binary comparator, there are no guarantees about the soundness of the hash functions, nor even about the consistency of the equality predicate with the hash value. That is, that equal values should hash to the same values cannot be guaranteed. With key functions, as user-defined types are always ultimately compared as built-in types, the hash functions used are written by the language developers and not the user – i.e. they would have guarantees of soundness to the extent that they are written by domain experts in an environment of broad scrutiny, rather than everyday users in isolation.

A generic interface specifying a key function would resemble:

::

  (define-generics comparable
    (key comparable))

That is, users would need to implement a single unary method, ``key``. By doing so, they gain all of the machinery of hashing and recursive comparisons for free, by virtue of delegating to an existing solution among the key types. Since the key never changes for an instance, it would be amenable to optimization via memoization.

The existing ``gen:equal+hash`` interface is reproduced in Appendix A, for comparison.

Note that once a definition of equality has been provided for a user-defined type, that type joins the key types. Ultimately, values in the language are compared for equality via key functions that may form arbitrarily long chains ("key chains") that terminate in the primitive key types.

Default Equality for User-defined Types
```````````````````````````````````````

In case ``gen:comparable`` is not implemented, then there are two options to consider:

A. Assume a default key function that maps to a vector containing the fields of the type in a particular order. This would mean that every new type that isn't "opaque" becomes a key type.

B. Assume that the new type is not comparable, so that attempting ``a = b`` is an error by default, assuming either ``a`` or ``b`` is an incomparable type of this kind.

Options
```````

For extending key types to include user-defined types via ``gen:comparable``, options are:

A. Use a key method, exclusively

B. Use a (comparator, hash, hash2) interface, exclusively

C. Support the user providing either a key method or (comparator, hash, hash2) implementation (but not both).

This proposal recommends (A) here due to the various benefits pointed out above, and also recommends against (C) for the following additional reasons:

1. Supporting both would be worthwhile out of necessity, but if it is not necessary, then the (comparator, hash, hash2) interface represents complexity -- both for writers as well as readers of the language. It also increases the size of the language and the consequent burden on maintainers.

2. If a comparator is truly needed in some cases, unless these cases are common, it may be worth considering an alternate "special case" channel rather than bloat the primary interface in order to support fringe cases.

Ad Hoc Extension of Equality
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While the key types may always be extended to new user-defined types, often (and perhaps more commonly), users need a definition of equality on the fly, either for a custom type or even a key type.

Such definitions of equality could either be temporary extensions of the key types to encapsulate instances of new types that do not have a definition of equality (i.e. where ``gen:comparable`` isn't implemented), or simply a specialization of the equality relation to a coarser version of itself (e.g. comparing strings in a case-insensitive way, for which the key function ``string-upcase`` or ``string-downcase`` may be used).

One way to think about this is that each key type represents a definition of equality, and there is also a global definition of equality (e.g. ``egal?``) that delegates to each of these in a disjoint way. But in practice we may desire a *different* definition for any one of these key types than the default one. This may be a coarser definition (e.g. case insensitive comparison, for strings), or even one that leverages the definition of equality on *other* types (for instance, comparing two strings by their *length*). It is for these (very common) cases that we need to provide the ability to customize the definition of equality in any setting where a notion of equality is presumed.

The way to customize the definition of equality in such cases is the same as usual, i.e. a key function – any unary, single-valued function mapping to a key type. The practical implications are that all APIs provided built-in by the language or even those authored by third parties should support a key argument if their user-facing purpose leverages a notion of equality.

Implementation
--------------

Hashing Scheme
~~~~~~~~~~~~~~

Requirements
````````````

Given an equality relation ``=``, a hash function ``h`` should be such that:

``a = b ⇒ h(a) = h(b)``

Conversely, ``not(h(a) = h(b)) ⇒ not(a = b)``

Additionally, desirable qualities of the hashing scheme include uniformity, efficiency, and more – e.g. see `Hash function <https://en.wikipedia.org/wiki/Hash_function>`__.

Keychain-based Scheme
`````````````````````

While equality comparisons are type-specific, an optimal hashing scheme would possess uniformity across types.

A naive scheme would be to define the hash of a value to be identical to that of its ground representative. With this approach, only ground values (i.e. instances of key types) would have unique hash values – for a value of any other type, its hash would coincide with that of a ground value by definition. This is still a valid hash function (since equal values have the same hashes), but it may lead to performance issues in dictionaries or sets containing keys of multiple types.

Another approach is to accumulate a type-specific "discriminant" at each link in the key chain with which to augment the hash computed on the ground value. Since the path taken through type space represented by the key chain uniquely identifies a type, and the chosen representative uniquely identifies a value within that type, this path together with the ground representative could be used to generate a hash value with global uniformity.

Indeed, the ground representative and the links in the key chain when taken together (e.g. in a tuple) constitute a unique representative in the language for the original value – a globally unique or lossless identifier (TODO: maybe only the source type together with the ground representative are needed – not sure if the keychain adds any additional uniqueness). Therefore, the properties of this global hash function (such as uniformity and efficiency) reduce to those of the hash function on key types, since the implementation itself is a simple facade on the primitive hashing scheme. For instance, double-hashing and any other techniques would not need to be extended beyond the key types.

Hashes for immutable values do not change and are therefore amenable to memoization. Consequently, for values that have already been compared for equality at least once (with any value, not necessarily the other participant in the present comparison), computing the representative would be a constant time operation. See `Optimizations`_ for more.

Numeric Comparison
~~~~~~~~~~~~~~~~~~

Mixing Types
````````````

``egal?`` treats numbers with different representations as being of different types, and therefore considers them unequal even if they are numerically equal. For instance, ``(egal? 1 1.0)`` is false. There are a few options here:

A. Accept ``egal?``'s handling of numbers, and in cases where numeric equality is desired, employ a key function that maps the numbers to a rich type where they may be compared numerically without losing information (as would happen in the case of a conversion to a floating point representation). The following key function is one example:

::

  (-> number? (or/c #f number?))
  (define (number->maybe-equal?-key x)
    (define (real->maybe-equal?-key x)
      (cond
        [(nan? x) #f]
        [(= +inf.0 x) +inf.0]
        [(= -inf.0 x) -inf.0]
        [else (inexact->exact x)]))
    (for/first ([r (in-value (real->maybe-equal?-key (real-part x)))]
                #:when r
                [i (in-value (real->maybe-equal?-key (real-part x)))]
                #:when i)
      (make-rectangular r i)))

B. Augment ``egal?`` so that the primitive equality relation ``=`` is ``egal?`` in all cases except for numbers. For numbers, they should be compared as reals (or complex numbers), that is, in a way that treats numbers with different representations as being of the *same* type, viz. reals or a similar "rich," lossless type, even though they may actually be a specific subtype such as rational or inexact. For inexact numbers, essentially, they should be converted to exact numbers prior to the comparison, similar to Racket's existing way of comparing exact and inexact numbers. In other words, the behavior for numbers with this option is probably identical with Racket's existing numeric ``=``.

Between these two options, there are reasons to favor the former, since choosing our primitive equality relation ``=`` so that ``1 = 1.0`` is true would mean that ``1 == 1.0`` must hold for any ``==`` that we define in the language, if we are to consider only "well-defined specializations of equality" (see `Design Constraints`_), and that means that we could never consider numbers with floating point representations as distinct from numerically equal numbers with exact representations. If this is what we want, then (B) would be fine. But if we do want to support distinguishing these, then only (A) would preserve "well-definedness."

A third option that may be a reasonable compromise would be to introduce a distinct numeric equality operator that is defined in terms of a key function on top of the primitive equality predicate. For instance, ``.=`` could mean numeric equality, employing the semantics of option (A).

Special Cases
`````````````

``+nan.0`` should be excluded from numeric comparison via ``=``. It could either be treated as a distinct key type, so that NaN = NaN is true, or it could orphaned entirely from equality comparison.

``0.0`` and ``-0.0`` should be treated as ``0``, exactly (according to the IEEE standard). Since floating point numbers are to be converted to exact representations prior to comparison, this should be handled in a matter of course.

``+inf.0`` and ``-inf.0`` should be equal to themselves but nothing else.

Once again, it's likely that these special cases coincide with the handling in Racket's numeric ``=``.

Optimizations
~~~~~~~~~~~~~

There are three broad aspects of the proposed scheme that could be optimized.

1. Computing the ground representative.
2. Computing the hash.
3. Computing the result of equality comparison.

Of these, the first two are aspects of the two-level scheme, while the third is an aspect of the primitive equality relation (i.e. the implementation of ``egal?``).

Some criteria on which performance of these could be judged are collected in Appendix C.

TODO: These all need a lot more detail and vetting.

1. Memoization

As ground representatives and hashes don't change (for immutable values), both of these may be cached after being computed once. Doing so makes this scheme equivalent in performance to the built-in equality predicate and hashing scheme (after the initial computation).

Note that memoization is not an option with a binary comparator instead of a key function. With comparators, memoizing the results of equality comparisons for ``n`` values would need ``n!`` cached results before equality comparison on the set is always constant time. In contrast, with key functions, ``n`` cached values would suffice to compute representatives in constant time, following by ``k!`` cached values on the set of key types K to ensure contant time comparisons as well – where K is a smaller set than the set of all types T and also allows reuse of the same memoized value across many different types (since, for example, a ``teacher`` type and a ``student`` type may both map to the same ground vector if these types happen to have the same field names). Although it seems likely that memoization on binary comparisons is not feasible in either case.

A `weak hashtable <https://docs.racket-lang.org/reference/eval-model.html#%28tech._weak._reference%29>`__ could probably be used for the purposes of memoization.

2. In ad hoc extension (i.e. ``#:key`` argument), ways to avoid expensive (key) mapping in the worst case, e.g. pre-checks before performing the key mapping.

3. Store efficient binary comparators "on" the most commonly used ad hoc keys (e.g. ``string-upcase``) -- note, this is relevant for ad hoc key mapping, not for key types where memoization is possible.

4. For mutable values, memoization is not applicable since the representative as well as (consequently) the hash may change. Options here include (1) avoiding mapping altogether (in both ad hoc as well as key type cases) and just use reference equality (i.e. egal's usual handling here, and key function is just ignored), or (2) respect the key function and live with worse performance for mutable values, and of course, forbid the use of such values in cases where deterministic hashing is assumed (e.g. dictionaries and sets – though, this could be handled at the data structure level so that sets always insert immutable copies of mutable values, and check for membership on a synthesized immutable copy of mutable input).

The latter option here would be more user friendly while still remaining consistent with ``egal?`` and hashing considerations. Mutable values would have poorer performance but that is a tradeoff made by the user.

5. Optimize the most common case of user-defined types being compared as vector-of-field-values (TODO: how?).

6. Cooperating key functions (nee comparators) that leverage optimizations based on nominal typing to potentially avoid more than one pass over the data.

7. Instead of always constructing the representative (e.g. a vector) at once, in some cases generate it lazily, one comparable component at a time. That way, equality comparison would not usually need to traverse the entire data structure before reaching a negative result (but still would, for a positive result). (TODO: this is not immediately compatible with memoization (1) and hash prechecking (9). It may take some creativity to be able to simultaneously leverage this in tandem with e.g. memoization. For instance, maybe partially generated representatives could be memoized).

8. For chained key functions, instead of generating the intermediate representations each time, employ a transducer so that only the final representation is constructed. This can also be combined with lazy generation of the key so that the transduced key computation is done lazily and generates only one representation.

9. Since hash values can be memoized but comparison results cannot (see (1)), and since numeric comparison is efficient (as opposed to, say, linear or log-linear comparison of collection-like structures), always compare the hash values first, and if they are equal, proceed with the equality comparison. Otherwise immediately return false, since we have the guarantee that equal values have equal hashes (which we don't have with the comparator-based implementation of gen:equal+hash – see `Extending the Key Types`_).

Design Constraints
------------------

Well-defined Specializations of Equality
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a primitive equality relation ``=``, ``==`` is a well-defined specialization of ``=`` (which we can denote ``σ(==, =)``) when the following conditions hold:

1. ``a = b ⇒ a == b``
2. ``not(a == b) ⇒ not(a = b)``

Every notion of equality supported by the language must satisfy these relations. This would have implications in particular for the definition of numeric equality.

Equality and Order
~~~~~~~~~~~~~~~~~~

Notions of equality should generally be consistent with notions of order, but these needn't be ensured by the language beyond the design phase. That is, where applicable, these invariants should be upheld by the language on built-in types, but they needn't be ensured through programming checks such as contracts on user-defined types.

Wherever we expect the following relations to hold, they should be ensured by the language by design (rather than by contract):

1. ``a = b ⇔ a ≤ b and b ≤ a``
2. ``a = b ⇒ not(a < b) and not(a > b)``
3. ``a ≤ b ⇔ a < b or a = b``

For instance, all three of these should hold for numbers. But e.g. some of them do not hold for the subset relation over sets.

Arguably, the third relation above should always be assumed to hold, even over user-defined types. Though, again, whether this is ensured by the language is another matter (and doing so would incur performance overhead).

Extras
------

Additional equality-related infrastructure may be built around the core to provide efficient and useful features in connection with the determination of equality.

TODO: Decidable equality

TODO: The difference between two values: ``(diff a b)``

TODO: anything else?

Prior Art
---------

* `Generic Relations <https://docs.racket-lang.org/relation/index.html>`_
* `Interconfection <https://docs.racket-lang.org/interconfection/index.html>`_
* `Rebellion <https://docs.racket-lang.org/rebellion/index.html>`_

Sample Implementation
---------------------

The Racket collection `relation/equivalence <https://docs.racket-lang.org/relation/Equivalence_Relations.html>`__ exhibits some of the behavior in this proposal, but also differs in other respects (the present proposal takes precedence where they differ).

Contributors
------------

* Ross "Nia" Angle
* Jack Firth
* Matthew Flatt
* Sid Kasivajhula
* Alex Knauth
* Sorawee Porncharoenwase
* Jens Axel Søgaard
* (among others -- see the references below)

References
----------

See the discussions at `What do we do about equality? <https://github.com/racket/rhombus-prototype/issues/16>`_, `Generic order relations <https://github.com/racket/rhombus-prototype/issues/214>`_, and `Rhombus bi-weekly virtual meeting <https://github.com/racket/rhombus-prototype/discussions/180>`_ for more context. Also see the predecessor of this document: `RRFI [Draft]: Equality and Order Relations Interface <https://gist.github.com/countvajhula/bf4041e4ae5e2feb7ad4b9631e2cf734>`_. In addition to relevant context, this document also contains a full listing of Racket APIs affected by this proposal.

Appendix A: Problems Addressed
------------------------------

This proposal is a response to several problems with the current ways of handling equality.

Surplus of Equivalence Predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Scheme standard, which Racket implements, has four equivalence predicates: ``eq?``, ``eqv?``, ``=``, and ``equal?``, and as a result it falls to the user to decide which one is appropriate to use in each situation. ``egal?`` / ``equal-always?`` is an improvement here as it replaces use of ``eq?`` and ``equal?`` in many cases. (TODO: specific examples where ``eq?`` or ``equal?`` are avoided). And with deeply integrated key functions, this single relation could suffice in every case.

Complexity of extending equality to user-defined types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Extending equality to user-defined types is accomplished by implementing three methods on the ``gen:equal+hash`` generic interface. As discussed earlier, this `isn't as straightforward as it could be <Extending the Key Types>`_. The interface is shown below.

::

  (define-generics equal+hash
    (equal-proc a b equal?-recur)
    (hash-proc a hash-recur)
    (hash2-proc a hash2-recur))

Lack of Unity in Ad Hoc Equality Extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In cases where APIs might support customizing the definition of equality, interfaces in the Racket ecosystem sometimes expect a key function, sometimes a binary comparator, sometimes they don't support customization, and at other times there are parallel sets of interfaces corresponding to the different equality predicates. The proposal suggests that a key function would work in every case and should be adopted as the standard to streamline APIs.

Appendix B: Reasoning Under Differing Notions of Equality
---------------------------------------------------------

A key function constitutes a definition of equality. As two given data structures may employ different key functions, we'd need to decide on what happens when they interact. This situation isn't unique to a key function world, as Racket already does have multiple notions of equality that happen to be built-in (i.e. ``eq?``, ``equal?``, etc.) and which occasionally interact. Their interaction does not appear to be modeled in Racket, as the handling is not consistent across interfaces.

For instance, currently, ``hash-union`` can union across different equality relations, e.g. ``hash`` and ``hasheq``. On the other hand, ``set-union`` does not allow this.

Proposed handling, either:

A. *Union* could be defined as "equal under any key" and *intersection* could be defined as "equal under all keys."
B. Or we don't allow it.

Appendix C: Case Breakdown for Performance Analysis of Equality Comparison
--------------------------------------------------------------------------

In designing for, or gauging, the performance of equality comparison on arbitrary values with and without an ad hoc key function being specified, it may be useful to employ these cases and see how performance is affected when, for inputs large and small:

1. The values are actually equal
2. The values are very similar but still different
3. The values are dramatically different but of the same type
4. The values are of different types

With the ad hoc key function and the input size multipliers, this is a 16-row grid, which could either (1) have 3 columns containing average case, best case, and worst case algorithmic performance, or (2) have benchmark results, or (3) both.
