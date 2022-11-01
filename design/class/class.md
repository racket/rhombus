Classes, Field, and Methods
===========================

Motivation
----------

The initial `class` form is a simplified variant of Racket's `struct`,
but it's called `class` because the intent is to provide a smoother
path to something like Racket's `class` form. Some things we'd like to
cover in a complete design:

 * Builtin support for keyword and optional fields.

 * A way to specialize the constructor beyond the built-in keyword and
   optional-argument support. The approach to constructors should be
   different than in Racket's `class`, where constructor-argument
   handling and the inialization sequence is intermingled with the
   rest of a class body, not to mention being disconnected from
   Racket's more modern approach to keyword arguments.

 * A way to control the binding pattern, annotation predicate,
   annotation constructor, dot provider, printer, and equality
   operator for a class.

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

field_spec := maybe_mutable identifier maybe_annot maybe_default
            | keyword: maybe_mutable identifier maybe_annot maybe_default
            | keyword maybe_default

maybe_mutable := mutable | ε
maybe_annot := :: annotation | ε
maybe_default := = expr | ε
```

Specifying a keyword for a field causes the constructor, binding
pattern, and annotation to expect a keyword for the corresponding
field instead of a by-position argument or subform. Specifying a
default-value expression causes the corresponding argument in the
constructor to be optional.<

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
the extra fields are not included in the default constructor, etc.

The `constructor`, `binding` and `annotation` clause forms support
customizing those aspects of the class. In each case, the form expects
an identifier afterward that is bound to the default constructor,
binding, or annotation, or a curried version in the case of
subclassing.

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
someone familiar with Racket's `class` might expect). Allowing
definitions here is necessary to support macro extensibility of
`class` using macro-generating macros, which is a common Racket and
Rhombus idiom. It's possible for a form within the block to be
ambiguous as a class clause or a definition or expression. Potential
ambiguity is resolved by ordering parse attempts: each form in the
block is first tried as a class clause, then as a definition, and
finally as an expression (analogous to the way that definitions and
then experessions are tried in a Rhombus module or body blocks
generally).

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
typically `binding` and `annotation` should also be specified.) The
`entry_point` for `binding` and `annotation` is a meta-time
expression.

A class is `final` unless `nonfinal` or `extends` is present (i.e., by
default, classes do not have subclasses). A `nonfinal` clause is
implied when `extends` is present and `final` is not present (i.e., by
default, a subclass can have further subclasses).

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

Similarly, when a class extends a superclass that has a customized
binding or annotation, then the class must also have a customized
binding or annotation, respectively. A `bind_identifier` or
`annot_identifier` in a subclass is “curried” in the sense that
`bind_identifier` or `annot_identifier.of` expects two terms
afterward: the first corresponds to a term to follow the superclass
binding or annotation form, and the second is a parenthesized sequence
of bindings or annotations correponding to the `field_spec`s of the
subclass. Typically, the term for the superclass form expects
parentheses, but it can have any shape; to work with a subclass
customization, however, it will need a shape that is represented as a
single term.

Examples
--------

Basic:

```
class Posn(x, y)

Posn(1, 2)

val p: Posn(1, 2)
p.x

Posn(1, 2).x

fun dist(p :: Posn):
  return p.x + p.y

val Posn(x, y): Posn(1, 2)
x
```

Keyword arguments:

```
class Posn(~x, ~y)

val p: Posn(~x: 1, ~y: 2)
p.x

val Posn(~y: _, ~x: x): Posn(~x: 1, ~y: 2)
x
```

Default field values:

```
class Posn(x, y, dist = x+y)

val p: Posn(2, 3)
p.dist

val p: Posn(2, 3, 17)
p.dist
```

Keywords distinct from field names, annotations on fields:

```
class Posn(~ex: x :: Integer,
           ~wy: y :: Integer)

val p :: Posn: Posn(~ex: 1, ~wy: 2)
p.x

val Posn(~wy: _, ~ex: x): Posn(~ex: 1, ~wy: 2)
x
```

Subclassing:

```
class Posn(x, y):
  nonfinal

class Posn3D(z):
  extends Posn

val p: Posn3D(1, 2, 3)
p.x
p.z
```

Custom constructor and binding:

```
import rhombus/meta open

class Posn(x, y):
  constructor (make):
    fun (x = 0, y = 0):
      make(x, y)
  binding (bind):
    rule | 'Posn()': 'bind(_, _)'
         | 'Posn($(x :: Group))': 'bind($x, _)'
         | 'Posn($x, $y)': 'bind($x, $y)'

Posn()
Posn(1)
Posn(1, 2)

val Posn(x): Posn(1, 2)
x

val Posn(_, y): Posn(1, 2)
y
```

Using `internal`:

```
import rhombus/meta open

class Posn(x, y):
  internal _Posn
  constructor:
    fun (x = 0, y = 0):
      _Posn(x, y)
  binding:
    rule | 'Posn()': '_Posn(_, _)'
         | 'Posn($(x :: Group))': '_Posn($x, _)'
         | 'Posn($x, $y)': '_Posn($x, $y)'
```

Subclassing with custom constructors:

```
class Posn(x, y):
  nonfinal
  constructor (make):
    fun (x = 0, y = 0):
      make(x, y)

class Posn3D(z):
  extends Posn
  constructor (make):
    fun (~x: x = 0, ~y: y = 0, ~z: z = 0):
      make(x, y)(z)

class Posn4D(w):
  extends Posn3D
  constructor (make):
    fun (~x = 0, ~y = 0, ~z = 0, ~w = 0):
      make(~z: z, ~y: y, ~x: x)(w)

Posn4D()
Posn4D(~x: 1, ~y: 2, ~w: 4)
```

Subclassing with custom binding:

```
import rhombus/meta open

class Posn(x, y):
  nonfinal
  binding (bind):
    rule 'Posn[[$x $y]]': 'bind($x, $y)'

val Posn[[x y]]: Posn(1, 2)

class Posn3D(z):
  extends Posn
  binding (bind):
    rule 'Posn3D[([$x $y $z])]': 'bind[[$x $y]]($z)'

val Posn3D[([x3 y3 z3])]: Posn3D(1, 2, 3)
```

Mixing keywords for default constructors with customized constructors:

```
class Posn(~x, y):
  nonfinal
  constructor (make):
    fun (~ex: x, ~wy: y):
      make(~x: 1, y)

class Posn3D(z):
  extends Posn
  constructor (make):
    fun (x, ~y: y, z):
      make(~ex: 1, ~wy: y)(z)
  internal _Posn3D

class Posn4D(w):
  extends Posn3D
  constructor (make):
    fun (x, y, z, w):
      make(x, ~y: y, z)(w)

Posn(~ex: 1, ~wy: 2)
Posn3D(1, ~y: 2, 3)
_Posn3D(~x: 1, 2, 3)
Posn4D(1, 2, 3, 4)
```

Mixing default-value forms and customized constructors:

```
class Posn(x, y, dist = x+y):
  nonfinal
  constructor (make):
    fun | (x, y): make(x, y)
        | (dist): make(dist, 0, dist)

Posn(2, 3)
Posn(5)
```

Helper definition within class:

```
class Posn0D():
  val mutable singleton: #false
  constructor (make):
    fun ():
      unless singleton | singleton := make()
      singleton

Posn0D() === Posn0D()
```

Open Issues
-----------

An uncooperative custom constructor for a non-final class might return
an instance of itself or some other subclass when called on behalf of
the constructor for a different subclass.

Class clause macros should be able to receive information about the
fields of the enclosing class. Moving `extends` out of the class body
would make it easier to expose to all clause forms, but a better
approach may be to report the information accumulated so far, which
might not include the superclass is `extends` is later in the body.

Prior art
---------

R6RS Records, especially constructor protocols.

Contributors
------------

* Jack Firth
* Matthew Flatt
* Alex Knauth
* Sam Phillips
* Sorawee Porncharoenwase
