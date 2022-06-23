Two-level Universal Scheme for the Extension of Equality
========================================================

.. sectnum::

.. contents:: :depth: 1

Proposal
--------

Model any notion of equality in the language on two levels -- the lower one consisting of "key" (i.e. built-in) types related by a primitive equality relation, and the higher level, of arbitrary values that may be mapped to the key types by means of key functions.

This allows the language to provide a simple and robust means of extending the equality relation to arbitrary equivalence relations while preserving the foundational properties of equality.

Why?
----

Simplicity
~~~~~~~~~~

1. The Scheme standard, which Racket implements, has four equivalence predicates: ``eq?``, ``eqv?``, ``=``, and ``equal?``. The present proposal provides a way to safely and easily extend equality predicates to maximize their applicability, and consequently minimizes the number of such relations that would be needed in a language.

2. The existing way of extending equality to user-defined types in Racket requires the user to specify a binary comparison predicate as well as implement part of the hash computation that will be used as a distinguishing key in data structures. There are also practical complications introduced in having to manage references for recursive equality and hash computation. In contrast, the present proposal requires the user to implement a single unary function, by doing which they gain all of the equality as well as hashing functionality.

3. Existing equality-based APIs and data structures in the Racket ecosystem present many different means of customization to users -- sometimes a key function is expected, sometimes a comparator, sometimes there are parallel sets of interfaces, and other times customization isn't supported. The present proposal would replace all of these with just one way -- that is, the option to specify a key function.

Robustness
~~~~~~~~~~

The existing way of allowing customization of equality via specification of a binary predicate and hashing functions presents a complicated interface to the user, but a more serious cause for concern is that it also compromises the integrity of both the equality relation (in terms of basic properties of reflexivity, symmetry, and transitivity that equality relations exhibit) as well as the hashing scheme, which can no longer be assumed to be sound.

In contrast, the present proposal implicitly preserves guarantees of the essential properties of equality, viz. reflexivity, symmetry, and transitivity. And in addition, the hashing mechanism is completely hidden from the user and thus any soundness and uniformity guarantees ensured by the language designers cannot be violated.

Completeness
~~~~~~~~~~~~

The proposal makes the claim that any notion of equality that could be expressed using a binary comparator can be expressed using a unary "key" function with a sufficiently diverse set of built-in "key" types, so that no expressiveness is lost.

Maintainability
~~~~~~~~~~~~~~~

By unifying the equality interface presented by the language in terms of primitive predicates and key functions, the proposal significantly reduces the number of data structures and APIs that need to be written and maintained.

How?
----

**Note**: If at any point the use of a term isn't clear, refer to the `Glossary`_ to make sure.

The proposed scheme has the following components:

1. A sufficiently diverse set of built-in types that form the initial "key" types.
2. A primitive, sufficiently "fine" equality relation on the key types.
3. A means to extend the key types to include user-defined types.
4. A means to extend the hashing scheme on key types to include user-defined types.
5. A means to map arbitrary values to key types for primitive comparison.

For (1), we will assume that the set of built-in types found in most programming languages are sufficient as the key types, but also suggest some criteria for sufficiency here.

For (2), we propose that *any* primitive equality predicate in the language that needs to support extension leverage the proposed key system. The proposal takes no position on this choice, but employs ``egal?`` for the purposes of illustration.

For (3), a standard generic dispatch mechanism (such as generic interfaces in Racket) is suggested as the means by which equality is defined for user-defined types.

For (4), a thin facade on top of the built-in hashing scheme suffices for user-defined types, without needing to expose it to the user.

For (5), unary, single-valued functions known as "key functions" are proposed as the standard way in which equality is extended to arbitrary values.

We'll look at each of these in turn.

Diversity of Primitive Key Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The foundational intuition of the approach proposed here comes from a standard mathematical `result on equivalence relations <https://encyclopediaofmath.org/wiki/Kernel_of_a_function>`__, that any equivalence relation on a set can be modeled by a function mapping values to another set on which a primitive notion of equivalence is defined. More precisely:

*For any equivalence relation == on a set E, there exists a set M and a function f : E → M such that, assuming a primitive notion of equivalence = on M, x == y is logically equivalent to f(x) = f(y).*

This is an abstract mathematical result that doesn't really tell us what the nature of the set ``M`` might be, or how the function ``f`` could be constructed[1], or the conditions under which an arbitrary set ``M'`` could serve as ``M`` here.

In programming languages, we can make this a little more tangible by observing that notions of equality partition along type boundaries. That is, values of the same type need not be equal, but values of disjoint types are implicitly unequal. In a sense, types recognized by the language are a manifestation of the primitive notion of equivalence on the set ``M`` that we seek to reveal.

This gives rise to a more concrete conception of the set ``M`` and the relation ``=`` – that ``M`` is composed of values that fall into different types, where each type has its own notion of equality. We call these types over ``M`` "key types," and observe that the equivalence relation ``=`` defined across the entire set ``M`` is composed from the equality relations over individual key types. A function ``f`` maps values in ``E`` (containing any value constructible in the programming language) to values in ``M`` that are instances of key types, and is called a "key function."

For example, a key function could map a user-defined Event type with attributes "place" and "time" to a representative vector containing the place and time values, so that two events are considered equal if they have the same place and time, by virtue of the existing (primitive) notion of vector equality (presuming that vectors are a key type).

Empirically, the built-in types in most languages (such as Racket) appear to suffice for the needs of equality for arbitrary user-defined types, and we will be making that assumption here. To substantiate this assumption, we motivate how these common built-in types represent a late stage in the richness of categories of types available to use as representatives:

* If the language has no innately comparable types to use as keys, then key functions don't have anything useful to return at all, and they can't be used to compare anything.
* If the language's only key types have small finite size like boolean? and fixnum?, then no key function will be able to compare bignums, strings, vectors, or other data of arbitrary size.
* If its key types have arbitrary size but are always serializable, then no key function will be able to compare opaque values according to information those values keep encapsulated.

A language like Racket has types of arbitrary size including hashes, lists, strings, and numbers, and empirically, these can be composed to express a variety of comparisons. Additionally, extensibility via generic interfaces allows key functions to be implemented as generic methods having local access to information for opaque types, allowing those to be compared as well.

Appendix C provides a straightforward construction of a function producing a unique ground representative for many common types.

[1] The standard proof simply exhibits such a function by mapping from E to M via the quotient set E/R (i.e. the set of the equivalence classes presupposed on E) -- it does not say how this function could be computed.

Primitive Equality Predicate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The present proposal is a way to extend *any* equality predicate. It leaves open the questions of:

1. How many primitive predicates to employ, and which ones?

2. How many of the primitive predicates, and which ones, should be made extensible via this scheme?

We will use ``egal?`` (also known as ``equal-always?``) for illustration purposes, as it is billed as "the finest equality predicate [a language should provide.]," and may be a good choice if a single primitive predicate is desired. See [1] for an implementation for Racket.

[1] https://github.com/racket/racket/pull/4076

Extending the Key Types
~~~~~~~~~~~~~~~~~~~~~~~

Basic Approach
``````````````

This could be done using a generic interface resembling the existing ``gen:equal+hash``. Instead of specifying a binary comparator together with hash functions, we suggest employing the key function approach here as it improves on the comparator approach in a number of ways.

1. With a binary comparator, there are no correctness guarantees regarding the definition of equality provided for a particular type. Indeed, the definition could violate reflexivity (via ``a != a``), symmetry (via ``a = b`` but ``b != a``), or transitivity (via ``a = b`` and ``b = c`` but ``a != c``), the three properties commonly taken to define valid equivalence relations. This could lead to surprising behavior as equality is broadly assumed to have these properties. With a key function, reflexivity, symmetry, and transitivity are implicitly guaranteed.

2. A binary comparator is a familiar way to define equality, with both objects available for manual comparison. Though it is familiar, it is not as simple as one might expect. In addition to the two input arguments, the user must also manage another binary predicate to be used for any recursive comparisons that may be necessary. In contrast, a key function involves a single argument, and requires the user to select a representative value from the key types. This may be a less familiar way to define equality, but it is also objectively simpler, and does not require that the user manage a predicate for recursive comparison.

3. The Racket implementation of ``gen:equal+hash`` employs double hashing, a standard technique to resolve hash collisions. This means that users must implement not one but two hash functions, exposing the hashing mechanism to the user who may be ill-equipped to grapple with the technicalities of writing good hash functions. Not to mention, there is once again the need for users to manage hash procedures used in recursive hash computation. In contrast, with key functions, the hashing mechanism is completely hidden from the user.

4. With a binary comparator, there are no guarantees about the soundness of the hash functions, nor even about the consistency of the equality predicate with the hash value. That is, that equal values should hash to the same values cannot be guaranteed. With key functions, as user-defined types are always ultimately compared as built-in types, the hash functions used are written by the language developers and not the user – i.e. they would have guarantees of soundness to the extent that they are written by domain experts in an environment of broad scrutiny, rather than everyday users in isolation.

A generic interface specifying a key function would resemble:

::

  (define-generics comparable
    (key comparable))

That is, users would need to implement a single unary method, ``key``. By doing so, they gain all of the machinery of hashing and recursive comparisons for free, by virtue of delegating to an existing solution among the key types.

The existing ``gen:equal+hash`` interface is reproduced below, for comparison.

::

  (define-generics equal+hash
    (equal-proc a b equal?-recur)
    (hash-proc a hash-recur)
    (hash2-proc a hash2-recur))

Note that once a definition of equality has been provided for a user-defined type, that type joins the key types. Ultimately, values in the language are compared for equality via key functions that may form arbitrarily long chains ("key chains") that terminate in the primitive key types.

Default Equality for User-defined Types
```````````````````````````````````````

In case ``gen:comparable`` is not implemented, then there are two options to consider:

A. Assume a default key function that maps to a vector containing the fields of the type in a particular order. This would mean that every new type that isn't "opaque" becomes a key type.

B. Assume that the new type is not comparable, so that attempting ``a = b`` is an error by default, assuming either ``a`` or ``b`` is an incomparable type of this kind.

Recommendation
``````````````

For extending key types to include user-defined types via ``gen:comparable``, options are:

A. Use a key method, exclusively

B. Use a (comparator, hash, hash2) interface, exclusively

C. Support the user providing either a key method or (comparator, hash, hash2) implementation (but not both).

This proposal recommends (A) here due to the various benefits pointed out above, and also recommends against (C) for the following additional reasons:

1. Supporting both would be worthwhile out of some known necessity, but otherwise, the (comparator, hash, hash2) interface represents complexity -- both for writers as well as readers of the language. It also increases the size of the language and the consequent burden on maintainers to support two different ways of doing the same thing, and which come with dramatically different guarantees.

2. If a comparator is truly needed in some cases, unless these cases are common, it may be worth considering an alternate "special case" channel rather than bloat the primary interface in order to support fringe cases.

Hashing Scheme
~~~~~~~~~~~~~~

Requirements
````````````

Given an equality relation ``=``, a hash function ``h`` should satisfy:

``a = b ⇒ h(a) = h(b)``

Conversely, ``not(h(a) = h(b)) ⇒ not(a = b)``

Additional desirable qualities of the hashing scheme include *uniformity* (resulting hash values should occur equally often across all inputs), *efficiency* (in space and time), *diffusion* (differential changes in the input should result in unpredictable changes in the output) and more – e.g. see `Hash function <https://en.wikipedia.org/wiki/Hash_function>`__.

Extending Built-in Hashing to User-Defined Types
````````````````````````````````````````````````

While equality comparisons are done in a type-specific way, an optimal hashing scheme would possess uniformity across types.

A naive scheme would be to define the hash of a value to be identical to that of its ground representative. With this approach, only ground values (i.e. instances of key types) would have hash values that obey all of the properties encoded into the design of the hash function – for a value of any other type, its hash would coincide with that of a ground value by definition. This is still a valid hash function (since equal values have equal hashes), but it may lead to "clustering" (i.e. loss of uniformity) in the computed hash values, which could cause performance issues in dictionaries or sets containing keys of multiple types whose hashes may coincide in this manner.

It would be better if we could find a way to extend the domain of the built-in hashing scheme to include the user-defined types in a way that preserves global uniformity and other desirable properties.

Towards this goal, we observe that for a given value ``v``, assuming we have a type identifier that uniquely identifies its type, and as the chosen ground representative uniquely identifies a value within that type, the pair of these values constitutes a unique representative in the language for ``v`` – a globally unique or lossless identifier.

So one way in which we could extend the built-in hash function ``H`` to a new value ``v`` not in its domain, is to construct the pair made up of the type of the value (which we could signify by ``τ(v)``) + its ground representative (``ρ(v)``), and then define the hash of ``v`` to be the hash of this synthesized value, i.e. ``H'(v) = H((τ(v), ρ(v)))`` (a kind of "macro," as it extends the built-in scheme to new values by defining it in terms of old values). This value can now be computed using the built-in hash function ``H``, as the pair so constructed is a ground value (assuming, of course, that pairs are a key type).

With this approach, the properties of this extended hash function ``H'`` (such as uniformity, efficiency, and diffusion) reduce to those of the built-in hash function ``H`` on key types, since the implementation itself is a simple facade on the primitive hashing scheme. And in particular, double-hashing and any other techniques employed in ``H`` would not need to be extended beyond the key types.

Ad Hoc Extension of Equality
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While the key types may always be extended to new user-defined types, often (and perhaps more commonly), users need a definition of equality on the fly, either for a custom type or even a key type.

Such definitions of equality could either be temporary extensions of the key types to encapsulate instances of new types that do not have a definition of equality (i.e. where ``gen:comparable`` isn't implemented), or simply a specialization of the equality relation to a coarser version of itself (e.g. comparing strings in a case-insensitive way, for which the key function ``string-upcase`` or ``string-downcase`` may be used).

One way to think about this is that each key type represents a definition of equality, and there is also a global definition of equality (e.g. ``egal?``) that delegates to each of these in a disjoint way. But in practice we may desire a *different* definition for any one of these key types than the default one. This may be a coarser definition (e.g. case insensitive comparison, for strings), or even one that leverages the definition of equality on *other* types (for instance, comparing two strings by their *length*)[1]. It is for these (very common) cases that we need to provide the ability to customize the definition of equality in any setting where a notion of equality is presumed.

The way to customize the definition of equality in such cases is the same as usual, i.e. a key function – any unary, single-valued function mapping to a key type. The practical implications are that all APIs provided built-in by the language or even those authored by third parties should support a key argument if their user-facing purpose leverages a notion of equality.

[1] Note that the latter case too is just a "coarser" definition of equality like the former, by the requirement of "well-defined specializations of equality" in the companion document on "Primitive Equality Predicates."

Is It Feasible?
---------------

Performance
~~~~~~~~~~~

There are four broad aspects of the proposed scheme that could be optimized.

1. Computing the ground representative.
2. Computing the hash.
3. Computing the result of equality comparison.
4. Ad hoc equality comparison.

Of these, the first two are aspects of the two-level scheme, while the third is an aspect of the primitive equality predicate (e.g. the implementation of ``egal?``). The fourth is an aspect common to the proposed scheme and existing ways of doing it.

Some criteria on which performance of these could be judged are collected in Appendix B.

Computing the Ground Representative
```````````````````````````````````

1. *Memoization* -- As ground representatives and hashes don't change (for immutable values), both of these may be cached after being computed once. Doing so makes this scheme equivalent in performance to the built-in equality predicate and hashing scheme (after the initial computation).

Note that memoization is not an option with a binary comparator instead of a key function. With comparators, memoizing the results of equality comparisons for ``n`` values would need ``C(n,2)`` (choosing pairs of elements) cached results before equality comparison on the set is always constant time. In contrast, with key functions, ``n`` cached values would suffice to compute representatives in constant time, followed by ``C(k,2)`` cached values on the set of key types K to ensure contant time comparisons as well – where K is a smaller set than the set of all types T and also allows reuse of the same memoized value across many different types (since, for example, a ``teacher`` type and a ``student`` type may both map to the same ground vector if these types happen to have the same field names). Although, it seems likely that memoization on binary comparisons is not feasible in either case.

A `weak hashtable <https://docs.racket-lang.org/reference/eval-model.html#%28tech._weak._reference%29>`__ could probably be used for the purposes of memoization.

For mutable values, memoization is not applicable since the representative as well as (consequently) the hash may change. Options here include (1) avoid mapping altogether (in both ad hoc as well as key type cases) and just use reference equality (i.e. egal's usual handling here, and key function is just ignored), or (2) respect the key function and live with worse performance for mutable values, and of course, forbid the use of such values in cases where deterministic hashing is assumed (e.g. dictionaries and sets – though, this could be handled at the data structure level so that sets always insert immutable copies of mutable values, and check for membership on a synthesized immutable copy of mutable input).

The latter option here would be more user friendly while still remaining consistent with ``egal?`` and hashing considerations. Mutable values would have poorer performance but that is a tradeoff made by the user.

2. *Common representative optimization* -- The most common representative for equality of user-defined types (and the default choice for "transparent" struct types) would be a vector containing the component fields. This case could be optimized, perhaps by exposing an existing such representation present in the implementation of the struct type to avoid a duplicate memoized value, or by modifying the implementation to be vector-based to enable this confluence.

3. *Lazy key computation* -- Instead of always constructing the representative (e.g. a vector) at once, in some (perhaps all) cases generate it lazily, one comparable component at a time. That way, equality comparison would not usually need to traverse the entire data structure before reaching a negative result (but still would, for a positive result).

Note that this optimization is not immediately compatible with memoization (#1). But it may be possible to leverage them together if, for instance, partially generated representatives could be memoized.

Additionally, doing it this way means that the actual ground representative here is not a simple vector (for instance) but a lazily generated sequence, meaning that such a sequence would need to be part of the key types in order for the primitive equality predicate to be able to use it.

4. *No intermediate representations* -- For chained key functions, instead of generating the intermediate representations each time, employ a transducer so that only the final representation is constructed. This could also be combined with lazy generation of the key so that the transduced key computation is done lazily and generates only one representation.

5. *Construction-time metadata* -- As an alternative to memoization (#1), the key may be computed and stored at instance construction time.

Computing the Hash
``````````````````

Memoization (#1), or computation at construction time (#5) are applicable here -- and the same considerations as above apply.

Primitive Predicate
```````````````````

6. *Hash preverification* -- Since hash values can be memoized but comparison results cannot (see #1), and since numeric comparison is efficient (as opposed to, say, linear or log-linear comparison of collection-like structures), always compare the hash values first, and if they are equal, proceed with the equality comparison. Otherwise immediately return false.

This optimization means that every equality comparison returning false is effectively a constant time operation. This may be the *majority of cases*.

This optimization is only possible since the proposed scheme guarantees that equal values have equal hashes -- a guarantee that we do not have with the existing way of extending equality to user-defined types – see `Extending the Key Types`_.

7. *Native comparison of common representatives* -- The binary comparison of vectors (the most common representative for user-defined types, and the default for transparent struct types) under the primitive predicate could have an optimized, perhaps native, implementation.

8. *Meh-less not meh-mo* -- Whenever an equality check returns true, the memoized representatives for each participant in the comparison could be merged, to free memory.

9. *Equivalence class construction* -- Naively, in cases where the equality predicate would return true, the data structures being compared must always be fully traversed. But in certain cases (perhaps cases where the traversal could be assumed to be expensive), values that have already been found to be equal could be stored in hashes containing sets of equal values -- known equivalence classes, essentially -- and then the equality comparison could reduce to checking for membership of all values in a common equivalence class. The equivalence classes could be keyed in the hash by all members. A positive result here allows us to return a positive result for the comparison, while a negative result requires traversal of the data structures, as usual, with a positive result of the equality comparison then resulting in the values being added to a common equivalence class.

This optimization is only possible because the proposed scheme guarantees symmetry and transitivity of equality -- guarantees we do not have with the existing way of extending equality to user-defined types.

Ad Hoc Equality Comparison
``````````````````````````

This applies to cases where a ``#:key`` argument is provided to customize the definition of equality on the fly. Such cases are not unique to the present proposal, as customizing equality in an ad hoc way is a common need already fulfilled in many different ways. By encapsulating the means of customization within the equality interface via the ``key`` argument, we lose the ability of users to provide optimized binary comparators (in cases where this is supported), while gaining the ability to optimize this case in a standard way with wide applicability.

Note that these optimizations are not necessarily relevant for extended key types, where stronger optimizations such as memoization are possible.

10. Store efficient binary comparators "on" the most commonly used ad hoc key functions (e.g. ``string-upcase``).

11. Avoid expensive (key) mapping in the worst case, e.g. employ pre-checks to catch edge cases before performing the key mapping.

Glossary
--------

In the proposed scheme, at a high level, there are three sets of interest, ``K``, ``K+`` and ``T``, and there are functions that map between them. The following terms name different aspects of the scheme.

* **The set of all types, T**: This is the set of all types either defined or definable in a language. The elements of this set are *types* rather than values.
* **Key type**: A built-in type provided by the language, which is in the domain of the primitive equality predicate ``=``. The set of key types will be denoted ``K``. The elements of this set are *types* rather than values.
* **Extended key types**: The set of key types augmented with user-defined types that have defined key functions. We will denote this set by ``K+`` and it is a superset of ``K``.
* **The set of all values, V**: This is the set of all values that could be constructed in the language.
* **Extent of a type**: For a type ``t ∈ T``, the "extent" of the type is the set of all values that are instances of that type. We will denote this ``ε(t)``. For convenience, we may also denote the extent of a set of types using the same notation, and for instance, ``ε(T) = V``. That is, the extent of the set of all types is the set of all values (of any type).
* **Type of a value**: For a value ``v``, we will denote its type as ``τ(v)``. ``τ(v) ∈ T``.
* **Key function, χ**: A unary, single-valued function ``χ`` mapping a value of any type to a value in ``ε(K+)``, i.e. ``χ : ε(T) → ε(K+)``. Key functions either (1) extend the equality predicate to new types, or (2) specialize the equality predicate to a coarser definition.
* **Key chain, ρ**: The sequence of key functions mapping a value in ``ε(K+)`` to a value in ``ε(K)``, i.e. ``χ₁, χ₂, ..., χn`` such that the composed function ``ρ = χn . ... . χ₁ : ε(T) → ε(K)``. In some cases it may be useful to think about the members or "links" of a key chain as *types*, so that for instance, a key chain may look something like (Teacher, Person, Vector). For convenience, we will also use the term to refer to the corresponding structure on individual values.
* **Immediate representative**: A value ``r ∈ ε(K+)`` that is the result of mapping an arbitrary value ``v ∈ ε(T)`` under a key function. We say that ``r`` is the immediate representative of ``v``.
* **Ground value**: Any value ``v ∈ ε(K)``, i.e. an instance of a key type.
* **Ground representative**: A ground value ``k ∈ ε(K)`` that is the result of mapping an arbitrary value ``v ∈ ε(T)`` under its key chain, i.e. ``ρ(v) = k``. We say that ``k`` is the ground representative of ``v``.

Prior Art
---------

* `Generic Relations <https://docs.racket-lang.org/relation/index.html>`_
* `Interconfection <https://docs.racket-lang.org/interconfection/index.html>`_
* `Rebellion <https://docs.racket-lang.org/rebellion/index.html>`_

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

`RRFI [Draft]: Equality and Order Relations Interface <https://gist.github.com/countvajhula/bf4041e4ae5e2feb7ad4b9631e2cf734>`_ -- The predecessor of this document. In addition to relevant context, this document also contains a full listing of Racket APIs affected by this proposal.

Rhombus Discussion: `What do we do about equality? <https://github.com/racket/rhombus-prototype/issues/16>`_

Rhombus Discussion: `Generic order relations <https://github.com/racket/rhombus-prototype/issues/214>`_

Rhombus Discussion: `Rhombus bi-weekly virtual meeting <https://github.com/racket/rhombus-prototype/discussions/180>`_

Appendix A: Reasoning Under Differing Notions of Equality
---------------------------------------------------------

A key function constitutes a definition of equality. As two given data structures may employ different key functions, we'd need to decide on what happens when they interact. This situation isn't unique to a key function world, as Racket already does have multiple notions of equality that happen to be built-in (i.e. ``eq?``, ``equal?``, etc.) and which occasionally interact. Their interaction does not appear to be modeled in Racket, as the handling is not consistent across interfaces.

For instance, currently, ``hash-union`` can union across different equality relations, e.g. ``hash`` and ``hasheq``. On the other hand, ``set-union`` does not allow this.

Proposed handling, either:

A. *Union* could be defined as "equal under any key" and *intersection* could be defined as "equal under all keys."
B. Or we don't allow it.

Appendix B: Case Breakdown for Performance Analysis of Equality Comparison
--------------------------------------------------------------------------

In designing for, or gauging, the performance of equality comparison on arbitrary values with and without an ad hoc key function being specified, it may be useful to employ these cases and see how performance is affected when, for inputs large and small:

1. The values are actually equal
2. The values are very similar but still different
3. The values are dramatically different but of the same type
4. The values are of different types

With the ad hoc key function and the input size multipliers, this is a 16-row grid, which could either (1) have 3 columns containing average case, best case, and worst case algorithmic performance, or (2) have benchmark results, or (3) both.

The same analysis may be done for hash computation, as well.

Appendix C: Regarding the Completeness of the System
----------------------------------------------------

Assume that pairs (and lists) and vectors are key types. Let ``I(v)`` signify a unique identifier in the language for the value ``v``, let ``(...)`` signify a pair or list, let ``tᵢ`` signify a type tag in a disjoint union type, and let ``[vᵢ]`` signify a vector with components ``vᵢ``. Then, for common types, ``I`` may be constructed as:

::

  I(v) = v if v ∈ K
         (T, [I(vᵢ)]) if v is a product type
         (T, tᵢ, I(value(v))) if v is a sum type

It would be worthwhile to generalize this to any other kinds of types that may exist in the language.
