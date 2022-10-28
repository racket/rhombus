Classes, Field, and Methods
===========================

Motivation
----------

The initial `class` form is a simplified variant of Racket's `struct`,
but it's called `class` because the intent is to provide a smoother
path to something like Racket's `class` form. Some things we'd like to
cover in a complete design:

 * A way to specialize the constructor, including a convenient way to
   have keyword arguments for fields. The approach to constructor s
   should be different than in Racket's `class`, where
   constructor-argument handling and the inialization sequence is
   intermingled with the rest of a class body, not to mention being
   disconnected from Racket's more modern approach to keyword
   arguments.

 * Similarly, there should be a way to control the binding pattern,
   annotation predicate, annotation constructor, and printer for a
   class.

 * Support for methods, including inheritance for implementation, but
   also something like properties, traits, or mixins for interface.

 * Macro-extensiblity of the `class` form, where macros are able to
   extend the notation to perform the kinds of transformations that
   might be implemented via annotations in Java or decorators in
   Python.

Summary
-------

The syntax of `class` is

```
class identifier(field_spec, ...):
  class_clause_or_body
  ...

field_spec := maybe_mutable identifier maybe_annotation
            | keyword: maybe_mutable identifier maybe_annotation
            | keyword

maybe_mutable := mutable | ε
maybe_annotation := :: annotation | ε
```

Specifying a keyword for a field causes the constructor, binding
pattern, and annotation to expect a keyword for the corresponding
field instead of a by-position argument or subform.

Predefined class-clause forms:

```
class_clause := extends identifier
             | final
             | nonfinal
             | field identifier maybe_annotation: body; ...
             | constructor (make_identifier): entry_point
             | binding (bind_identifier): entry_point
             | annotation (annot_identifier): entry_point
             | internal identifier
             | authentic
```

An `extends` clause specifies a superclass. The superclass must be
nonfinal.

Each `field` clause adds additional mutable fields to the class, but
the extra fields are not including in the default constructor, etc.

The `constructor`, `binding` and `annotation` clause forms support
customizing those aspects of the class. In each case, the form expects
an identifier afterward that is bound to the representation's
constructor, binding, or annotation that would otherwise apply without
customization.

If `internal` is present, then it binds the associated `identifier` to
the representation in the same context as the `class` definition, and
that `identifier` also serves as a default for `make_identifier`,
`bind_identifier` or `annot_identifier`.

The `authentic` clause is a performance hack that disallows custodians
and impersonators.

Design
------

The block after `class` contains a mixture of class clauses and other
definitions and expressions. Clauses can add extra fields to the
class, add methods, customize the constructor, and so on.

Definitions and expressions that appear a `class` block are evaluated
at *class* creation time (instead of *instance* creation time, like
someone familiar with Racket's `class' might expect). Allowing
definitions here is necessary to support macro extensibility of
`class` using macro-generating macros, which is a common Racket and
Rhombus idiom. It's possible for a form within the block to be
ambiguous, such as either a class clause or an expression. Potential
ambiguity is resolved by ordering parses: each form in the block is
first tried as a class clause, then as a definition, and finally as an
expression (analogous to the way that definitions and then
experessions are tried in a Rhombus module or body blocks generally).

The right-hand side of a `field` declaration is similarly evaluated
once when the class is defined. The resulting constant is used to
initialize the field in each instance. Since these fields must be
mutable to be useful, the `mutable` keyword is not needed or allowed
in a `field` clause.

The `constructor` clause expects an immediate function in its block.
The `entry_point` syntactic category includes `fun` with the same
syntax as its expression form. It also includes a `rule` form for
simple pattern-matching macro transformations, which is useful with
`binding` and `anotation`. (When `constructor` is specified, then
typically `binding` and `annotation` should also be specified.)

A class is `final` unless `nonfinal` or `extends` is present (i.e., by
default, classes do not have subclasses). A `nonfinal` clause is
implied when `extends` is present and `final` is not present (i.e., by
default, a subclass can have surther subclasses).

When a class extends a superclass that has a customized constructor,
then the class must also have a customized constuctor. In that case,
the binding of `make_identifier` is a curried constructor: it expects
the arguments that the superclass wants, and it returns a function to
consume the arguments that the default constructor wants; the result
of calling the second function is the class instance. A customized
constructor is not obligated to call `make_identifier`, but it is
obligated to return an instance of the class. When `make_identifier`
or its result produces an instance of class, it will be an instance of
a subclass if the constructor was call on behalf of the subclass.

Open Issues
-----------

Since it's a common case, probably a `field_spec` should allow a
keyword in place of an identifier with the meaning that the default
constructor expects keyword arguments.

An uncooperative custom constructor for a non-final class might return
an instance of itself or some other subclass when called on behalf of
the constructor for a different subclass.

Other Discussion
----------------


Prior art
---------

R6RS Records, especially constructor protocols.


Contributors
------------

* Jack Firth
* Matthew Flatt
* Alex Knauth
* Sorawee Porncharoenwase

References
----------

