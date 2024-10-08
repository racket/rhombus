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
   handling and the initialization sequence is intermingled with the
   rest of a class body, not to mention being disconnected from
   Racket's more modern approach to keyword arguments.

 * A way to control the binding pattern, annotation predicate,
   annotation constructor, namespace, printer, and equality operator
   for a class.

 * Support for methods, including inheritance for implementation, but
   also something like properties, traits, or mixins for interface.

 * Macro-extensiblity of the `class` form, where macros are able to
   extend the notation to perform the kinds of transformations that
   might be implemented via annotations in Java or decorators in
   Python.

Summary
-------

The syntax of `class` is (approximately; see Scribble-based docs for
more precise grammars):

```
class identifier(field_spec, ...):
  class_clause_or_body_or_export
  ...

field_spec := modifiers identifier maybe_annot maybe_default
            | keyword: modifier identifier maybe_annot maybe_default
            | keyword maybe_default

modifiers := private | mutable | private mutable | ε
maybe_annot := :: annotation | ε
maybe_default := = expr | ε
```

Specifying a keyword for a non-private field causes the constructor,
binding pattern, and annotation to expect a keyword for the
corresponding field instead of a by-position argument or subform.
Specifying a default-value expression causes the corresponding
argument in the constructor to be optional. Private fields from a
`field_spec` are not included in the default constructor, so they
either need defaults or a custom constructor.

The `interface` form is similar, but without fields, constructors,
custom binding, or custom annotation.

```
interface identifier:
  interface_clause_or_body_or_export
  ...
```

Predefined class-clause forms:

```
class_clause := extension_decl
              | field_decl
              | method_decl
              | represent_decl
              | authentic

extension_decl := extends identifier
                | implements_decl
                | nonfinal

implements_decl := implements identifier
                 | implements: identifier ...; ...
                 | private implements identifier
                 | private implements: identifier ...; ...
                
field_decl := field identifier maybe_annotation: body; ...
            | private field identifier maybe_annotation: body; ...

method_decl := method method_impl
             | override method_impl
             | final method_impl
             | private method_impl
             | abstract method_decl

method_impl := id(arg, ..) maybe_annot: body
             | id maybe_annot: entry_point

method_decl := id(arg, ..) maybe_annot
             | id maybe_annot

represent_decl := constructor: entry_point
                | binding: entry_point
                | annotation: entry_point
                | internal identifier
```

An `extends` clause specifies a superclass. The superclass must be
nonfinal. The `nonfinal` clauses can specify a finality other than the
default, which is that the class is final.

An `implements` clauses specifies one or more interfaces. Interfaces
tend to have abstract methods that must be implemented by a (sub)class
before the (sub)class can be instantiated. The combination `private
implements` implements an interface privately, which can communicate
to the creator of an interface in combination with `internal`, but
does not necessarily expose the implemented methods. For example,
privately implementing a printing method can customize a trusted
printing implementation without exposing the method to callers that
might provide unsuitable arguments. Private implementation also
supports some of the same patterns as `define-local-member-name` in
Racket.

Each `field` clause adds additional mutable fields to the class, but
the extra fields are not included in the default constructor, etc.,
even in the internal view of the constructor. A field can be declared
as `private`, which means that it can only be accessed within methods
of the class or through an `internal` identifier. For most purposes, a
`private field` class is more convenient that `private` in a class's
initial `field_spec`, unless immutability is important.

Each `method`, `override`, method-shaped `final`, or method-shaped
`final` private declaration adds a method to the class. A `method` can
appear after `override`. An `override`, `method`, or both can appear
after `final`. A `method` can appear after `private`. An `abstract`
declaration also adds a method, but without an implementation. A
result annotation (not counting any that is an the `entry_point`)
applies to the method and any overriding implementation in a subclass.

The `constructor`, `binding` and `annotation` clause forms support
customizing those aspects of the class.

If `internal` is present, then it binds the associated `identifier` to
the an internal view of the class in the same context as the `class`
definition. The representation accesses a default constructor,
binding, and annotation, and it provides static access private fields
and methods.

The `authentic` clause is a performance hack that disallows custodians
and impersonators.

Predefined interface-clause forms are similar, but usually simpler:

```
interface_clause := extension_decl
                  | method_decl
                  | internal identifier

extension_decl := extends identifier
                | extends: identifier ...; ...
                
method_decl := method method_spec
             | override method_spec
             | final method_spec
             | private method_spec
             | abstract identifier
```

If `internal` is present in `interface`, then it binds the associated
`identifier` to the representation in the same context as the
`interface` definition, and it can be used to recognize private
implementations of the interface as well as access private methods.

Both class and interface names work as annotations, and the
annotation is satisfied by an instance of the class, subclass, or (in
the case of an interface) implementing class. A class name further
provides an annotation constructor via `.of`, by default, but an
interface name does not.

Design
------

The block after `class` or `interface` contains a mixture of class
clauses and other definitions and expressions. Clauses can add extra
fields to the class, add methods to a class or interface, customize
the constructor of a class, and so on.

Definitions and expressions that appear in a `class` or `interface`
block are evaluated at *class* or *interface* creation time (instead
of *instance* creation time, like someone familiar with Racket's
`class` might expect). Allowing definitions here is necessary to
support macro extensibility of `class` or `interface` using
macro-generating macros, which is a common Racket and Rhombus idiom.
It's possible for a form within the block to be ambiguous as a
class/interface clause or a definition or expression. Potential
ambiguity is resolved by ordering parse attempts: each form in the
block is first tried as a class/interface clause, then as a
definition, and finally as an expression (analogous to the way that
definitions and then expressions are tried in a Rhombus module or
body blocks generally). Class-clause macros and interface-clause
macros are defined separately, but an identifier can be bound in both
spaces (as well as the expression space, and so on).

The right-hand side of a `field` declaration is similarly evaluated
once when the class is defined. The resulting constant is used to
initialize the field in each instance. Since these fields must be
mutable to be useful, the `mutable` keyword is not needed or allowed
in a `field` clause.

A `class` or `interface` body can contain `export` clauses, because
the class or interface also serves as a namespace. Exoprted names must
be distinct from fields and methods, which are implicitly exported
from a class. A field name is exported from a class as an accessor for
the field. A method name is exported from a class as a procedure that
expects an instance of the class followed by arguments to the object's
methods.

Clauses like `method` expect an immediate function. A `method` clause
can be written like `fun` (but with `method` or similar form name in
place of `fun`), or it can be written as an identifier followed by a
block that contains an `entry_point`. The `entry_point` syntactic
category includes `fun` with the same syntax as its expression form.
When a method is declared with `method` or `final` without `override`,
then the a method with the same name must not be declared in a
superclass or superinterface, if any. When a method is declared with
`override`, then it must be declared in a superclass or
superinterface. (By "superinterface", we include interfaces
implemented by a class.)

Within a method, `this` refers to the object that was used for the
method call. A class's fields and methods can be access via `this`,
and an interface's methods can be access similarly, but they also can
be referenced directly, including inherited field and method names.
When a method argument has the same name as a field or method, then it
shadows the field or method.

Method and fields names must all be distinct, both within a class and
taking into account superclass and superinterface public fields and
methods. A private field or method name is not visible outside of a
class or interface, except when using an `internal` name, so it is not
required to be distinct from subclass or subinterface fields and
methods. Method names inherited from multiple implemented interfaces
must all be implemented the same way, either abstract, implemented in
the same originating interface, or overridden in the implementing
class. All field and methods names can be accessed through an object
with `.`, but private field and methods names can only be accessed
statically, either within the defining class or via an `internal`
binding. In static mode (i.e., when `use_static` is declared), then a
method can only be used in a call form; when dynamic `.` is used to
access a method, then it does not have to be a call, and the result of
the `.` expression is a closure over the object.

When a class implements an interface privately, the private methods
are overridden with `private override` (and no other use of `private`
combined with `override` makes sense). If an interface is declared to
be implemented both privately and normally (perhaps as a
superinterface of a normally implemented interface), the interface is
implemented normally. Individual method names of a privately
implemented interface may overlap with methods of a normally
implemented interface, in which case those methods are public.

The `constructor` clause must either have an anonymous-function shape
like `fun` (but with `constructor` in place of `fun`) or be followed
by a blocking containing an immediate function as an `entry_point`.
Similarly, `binding` and `annotation` can have a pattern-rule shape or
expect a meta-time `entry_point`; the `rule` form serves an
`entry_point` for simple pattern-matching macro transformations. (When
`constructor` is specified, then typically `binding` and `annotation`
should also be specified.)

A class is final unless `nonfinal` is present (i.e., by default,
classes do not have subclasses).

When a class extends a superclass that has a custom constructor, the
new class's default constructor assumes that the superclass construct
accepts arguments consistent with its immediate and inherited
`field_spec`s. In a custom constructor, the binding of `super` within
the constructor is a curried constructor: it expects the arguments
that the superclass wants, and it returns a function to consume the
arguments that the default constructor wants, including arguments for
`private` `field_spec` fields; the result of calling the second
function is the class instance. A customized constructor is not
obligated to call `super`, but it is obligated to return an instance
of the class. When `super` or its result produces an instance of
class, it will be an instance of a subclass if the constructor was
call on behalf of the subclass.

When a class extends a superclass that has a customized binding or
annotation, then the class must also have a customized binding or
annotation, respectively. There is no `super` binding or annotation;
instead, `internal` can be used to get an internal binding and
annotation that expects components only for the immediate class. That
internal view can be combined with the superclass binding or
annotation and the `&&` binding or annotation operator.

Examples
--------

Basic:

```
class Posn(x, y)

Posn(1, 2)

val p: Posn(1, 2)
p.x

Posn(1, 2).x
Posn.x(p)

fun dist(p :: Posn):
  p.x + p.y

val Posn(x, y): Posn(1, 2)
x
```

Methods:

```
class Posn(x, y):
  nonfinal
  method mdist(): x + y
  method is_close(): mdist() < 6

class Posn3D(z):
  extends Posn
  override mdist(): x + y + z
  method is_in_2D(): z == 0

val p: Posn3D(1, 2, 3)
p.mdist()
p.is_in_2D()
p.is_close()

Posn.mdist(p)
Posn3D.is_in_2D(p)
```

Private fields:

```
class Posn(x, y):
  private field c :: String: "red"
  method color:
    fun | (): c
        | (s :: String): c := s
        | (~like: p :: Posn): c := p.c
                              
val p: Posn(1, 2)
p.color("blue")
val p2: Posn(3, 4)
p2.color(~like: p)
Posn.color(p2, ~like: p)
```

Keyword constructor arguments:

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

val p2: Posn(2, 3, 17)
p2.dist
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
  constructor (x = 0, y = 0):
    super(x, y)
  internal _Posn
  binding:
    rule | 'Posn()': '_Posn(_, _)'
         | 'Posn($(x :: Group))': '_Posn($x, _)'
         | 'Posn($x, $y)': '_Posn($x, $y)'

Posn()
Posn(1)
Posn(1, 2)

val Posn(x): Posn(1, 2)
x

val Posn(_, y): Posn(1, 2)
y
```

Subclassing with custom constructors:

```
class Posn(x, y):
  nonfinal
  constructor (x = 0, y = 0):
    super(x, y)

class Posn3D(z):
  extends Posn
  nonfinal
  constructor (~x: x = 0, ~y: y = 0, ~z: z = 0):
    super(x, y)(z)

class Posn4D(w):
  extends Posn3D
  constructor (~x = 0, ~y = 0, ~z = 0, ~w = 0):
    super(~z: z, ~y: y, ~x: x)(w)

Posn4D()
Posn4D(~x: 1, ~y: 2, ~w: 4)
```

Subclassing with custom binding:

```
import rhombus/meta open

class Posn(x, y):
  nonfinal
  internal _Posn
  binding 'Posn[[$x $y]]': '_Posn($x, $y)'

val Posn[[x y]]: Posn(1, 2)

class Posn3D(z):
  extends Posn
  internal _Posn3D
  binding 'Posn3D[([$x $y $z])]': 'Posn[[$x $y]] && _Posn3D($z)'

val Posn3D[([x3 y3 z3])]: Posn3D(1, 2, 3)
```

Mixing keywords for default constructors with customized constructors:

```
class Posn(~x, y):
  nonfinal
  constructor (~ex: x, ~wy: y):
    super(~x: 1, y)

class Posn3D(z):
  extends Posn
  nonfinal
  constructor (x, ~y: y, z):
    super(~ex: 1, ~wy: y)(z)
  internal _Posn3D

class Posn4D(w):
  extends Posn3D
  constructor (x, y, z, w):
    super(x, ~y: y, z)(w)

Posn(~ex: 1, ~wy: 2)
Posn3D(1, ~y: 2, 3)
_Posn3D(~x: 1, 2, 3)
Posn4D(1, 2, 3, 4)
```

Mixing default-value forms and customized constructors:

```
class Posn(x, y, dist = x+y):
  nonfinal
  constructor:
    fun | (x, y): super(x, y)
        | (dist): super(dist, 0, dist)

Posn(2, 3)
Posn(5)
```

Helper definition within class:

```
class Posn0D():
  val mutable singleton: #false
  constructor:
    fun ():
      unless singleton | singleton := super()
      singleton

Posn0D() === Posn0D()
```

Interfaces:

```
interface Shape:
  abstract area
  method ten_area(): 10 * area()

interface Polygon:
  extends Shape
  abstract sides
  method has_corners(): #true

interface Circle:
  extends Shape
  method has_sides(): #false

class ApproxCircle():
  implements: Polygon Circle
  override area(): 33
  override sides(): 100

val a: ApproxCircle()

a.area()
(a :~ Polygon).has_corners()
(a :~ Circle).has_sides()

(a :~ Shape).area()
(a :~ Polygon).area()
(a :~ Circle).area()

a.ten_area()
```

Overlap of public and privately implemented interfaces:

```
interface Stool:
  internal _Stool
  abstract legs
  abstract seat

interface Cow:
  abstract legs
  abstract horns

class MilkShed():
  private implements Stool
  implements Cow
  override legs(): 4
  override horns(): 2
  private override seat(): 1

val m: MilkShed()

!(m is_a Stool)
m is_a _Stool
m is_a Cow
m.legs()
m.horns()
(m :~ Stool).seat()
```

Private immutable field:

```
class Posn(x, y, private stamp):
  constructor (x, y):
    super(x, y, current_stamp())
  internal _Posn

val p: Posn(1, 2)
val Posn(x, y): p
_Posn.stamp(p)
(p :~ _Posn).stamp
```

Open Issues
-----------

Access to private fields and methods relies on static resolution of
the private reference. Up to this point, we have tried to make dynamic
resolution produce the same answer, if more slowly. Still, statis
resolution of a namespace `.` is a kind of precedent. And for the
limited case of accessing private members, hopefully all relevant code
is in the same maintenance region, so maybe it will be ok to rely on a
static-information system that is somewhat brittle.

An uncooperative custom constructor for a non-final class might return
an instance of itself or some other subclass when called on behalf of
the constructor for a different subclass.

Class clause macros should be able to receive information about the
fields of the enclosing class. Moving `extends` out of the class body
would make it easier to expose to all clause forms, but a better
approach may be to report the information accumulated so far, which
might not include the superclass is `extends` is later in the body.

It would be nice to support implementing different interfaces that use
the same name for a method without forcing the method implementations
to be the same. That goal seems to fundamentally conflict with dynamic
`.`; it might make sense to compromise on dynamic `.`, or it might be
better to just live with a prohibition against same-named method in
superinterfaces.

Although an abstract method can be written with arguments and
annotations, only the result annotation has any effect and is
propagated to overrides. More generally, there's no support for
checking that a method override takes arguments consistent with the
overriden declaration. It's not clear how argument checking would
work. Result checking, meanwhile, justifies static information about
the result of a method.

Expansion order in the body of a `class` form limits the places where
the class being defined can be referenced. For example, this works:

```
class Posn(x, y):
  export: get_origin
  fun get_origin(): Posn(0, 0)

Posn.get_origin()
```

However, neither `origin` nor `get_origin` (with a `Posn` result
annotation) works when written as follows:

```
class Posn(x, y):
  export: origin get_origin
  val origin: Posn(0, 0)
  fun get_origin() :: Posn: Posn(0, 0)
```

A `method` form in `Posn` can use `Posn` as a return annotation
because, essentially, it cannot be referenced directly in the body of
a `class` form. But the `get_origin` above needs to be build for later
use within the body, along with information its explicit return
annotation, and so `Posn` would have to be given a meaning before it
is defined.

An `origin` variable and return-annotation `get_origin` function can
be written outside the class and still exported by the class, because
a namespace can export bindings from outside the namespace:

```
class Posn(x, y):
  export: origin
          get_origin
val origin: Posn(0, 0)
fun get_origin() :: Posn: Posn(0, 0)

Posn.origin
Posn.get_origin()
```

Prior art
---------

R6RS Records, especially constructor protocols.

Java classes and interfaces.

C++ private superclasses.

Contributors
------------

* Robby Findler
* Jack Firth
* Matthew Flatt
* Ben Greenman
* Alex Knauth
* Sam Phillips
* Sorawee Porncharoenwase
