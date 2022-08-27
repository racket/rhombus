Modules, Namespaces, and Naming Conventions
===========================================

In contrast to Racket, Rhombus will take advantage of hierarchical
naming with a `.` separator, instead of using prefixes that are within
an identifier. For example, instead of a name like `vector-length` or
`array_length`, Rhombus has `Array.length`. Similarly, `List.of` is an
annotation constructor. Sometimes, a name works both by itself and as
a namespace for reaching more nested bindings, as in the case of
`List` as a constructor plus `List.length` as a function.

By convention, constructor-like and type-like names are capitalized,
while other names are not. So, a module or namespace name is typically
not capitalized, unless it also corresponds to a constructor or type.

Namespace `.` versus Expression `.`
-----------------------------------

The `.` in `List.length` is a differnt kind of `.` than the one in
`[1, 2, 3].length`. In the latter case, the `.` is an expression
operator, and `.` as an expression operator resolves a field name
statically if possible, but falls back to dynamic resolution. As a
namespace accessor, `.` is always resolved statically.

Within the Rhombus implementation, a namespace `.` is resolved through
different mechanisms than an expression `.`. A namespace `.` is
resolved directly during enforestation so that a dotted reference can
play the same role as an identifier for an expression, a binding
pattern, an annotation, etc. When `.` does not follow an identifier
that is bound as a namespace, then it is instead parsed as an operator
in its context, such as an expression or binding operator.

Namespaces and Modules
----------------------

A namespace is similar to a module, and the `import` form unifies the
two in certain ways. In the case of `import: rhombus/macro`, for
example, imported bindings are given a `macro.` prefix, as in
`macro.expr.rule`, making `macro` act like a namespace. By default,
the last component of a module path is used as the namespace-like
prefix, but the `as` operator can select an alternative; for example,
`import: rhombus/macro as M` binds `M`. A module's exports can be
dumped into the importing binding context as in `import: rhombus/macro
open`, in which case the prefix is not used.

A namespace can similarly be opened, as in `import: .List open`. Note
that `.List` is used instead of just `List` to distinguish a namespace
name like `List` from a module path like `rhombus`.

A module can export namespaces, including namespace-like identifiers
that stand for modules. To the degree that modules and namespace are
different, a namespace-like identifier for a module continues to act
like a module reference. For example, the bindings of a namespace-like
module reference can be phase-shifted with `for_meta`, while the
bindings of a namespace cannot.

As a shorthand, `import` allows using `.` on the imported name to
directly import a namespace (or namespace-like module name) that is
exported by a module or namespace. For example, `import: rhombus.List`
imports just the `List` binding from the `rhombus` module, and
`import: rhombus.List open` dumps the bindings of `rhombus.List` into
the enclosing context without any of the other bindings of `rhombus`.
Furthermore, the last component of a dotted reference can be a
non-namespace identifier, as in `import: rhombus.List.of`, which
imports only `List.of` into the encosing context. An imported
identifer that is not a namespace or module cannot be unpacked with
`open`. Overall, there are really three kinds of binding that are
folded into `import`: from a module, from a namespace, or just one
binding.

Note that when a name refers both to a namespace and a binding that
can be used directly, `import` binds both. For example, `import:
rhombus.List` binds `List` as an expression as well as `List.length`
as an expression and `List.of` as an annotation constructor.

Defining a Namespace
--------------------

Define a namespace using the `namespace` form followed by the name of
the namespace. Export bindings from the namespace using `export` in
the `namespace` body, the same as for a module.

```
namespace math:
  export:
    tau
    Complex
  val pi: 3.14
  val tau: 2 * pi
  class Complex(real, imag)

math.tau                   // prints 6.28
// math.pi                 // would be an error
math.Complex(0, math.tau)  // prints Complex(0, 6.28)
```

A namespace is *not* a submodule. The body of a `namespace` form can
still refer to bindings in the environment of the `namespace` form.
Also, it is not possible to independently import a namespace without
instantiating its enclosing module.

A namespace can export bindings that are visible in the environment of
the `namespace` form, including namespaces, namespace-like module
names, and so on, leading to longer `.`ed paths.

```
namespace subject:
  export:
    math
    english
  nest english:
    export: greeting
    val greeting: "Hello"

subject.english.greeting  // prints "Hello"
subject.math.tau          // prints 6.28

export: subject // exported by the enclosing module or namespace
```

Suppose the above example is an an `example/stuff` module. Then,
`math` might be imported with

```
import: example/stuff.subject.math
```


Namespace Extension
-------------------

As an alternative putting definitions in the body of a `namespace`
form, a namespace can be extended by using a dotted name with
a definition form like `val`:

```
namespace science

val science.oxygen: 8
val science.silicon: 14
```

Existing namespaces can be extended in the same way.

```
val List.one: [1]
```

An extension does not mutate a namespace globally. It only extends a
namespace in the binding context of the extension.

```
begin:
  val List.two: [1, 2]
  List.two    // prints [1, 2]

// List.two   // would be an error
```

An extended namespace can be exported from a module or namespace. When
an extended namespace is imported, then any extensions visible at the
export are also exported. The non-extended part of the namespace
counts as the same binding as the original namespace, so importing an
extended namespace does not conflict. Along those lines, two different
extensions of a namespace can be imported into the same context to
combine the extensions, as long as the extensions do not conflict.


Mutual Dependencies
-------------------

A namespace can accomodate certain forms of mutual dependency. For
example, this works:

```
namespace a:
  export: alpha          
  fun alpha():
    b.beta()

namespace b:
  export: beta
  fun beta():
    a.alpha()
```

A key reason that it works is that `b.beta` is nested enough that its
expansion is delayed until all bindings are determined for enclosing
binding context.

The following also works for the same reason:

```
namespace a:
  export: alpha          
  fun alpha():
    import: .b open
    beta()

namespace b:
  export: beta
  fun beta():
    import: .a open
    alpha()
```

The following variant *does not* work, because a namespace's body is
still in the same expansion context the `namespace` form itself, and
`b` is not yet defined at the point that `import: .b open` must be
resolved:

```
namespace a:
  import: .b open
  export: alpha          
  fun alpha():
    beta()

namespace b:
  import: .a open
  export: beta
  fun beta():
    alpha()
```

A hybrid works, where only `a` is used in the `namespace b` body,
since `a` has been defined at that point:

```
namespace a:
  export: alpha          
  fun alpha():
    import: .b open
    beta()

namespace b:
  import: .a open
  export: beta
  fun beta():
    alpha()
```

Namespaces and Spaces
---------------------

The `rhombus` language provides multiple bindings for a name like
`List` for different contexts: one for expressions, one for binding
contexts, on for annotations, and so on. These different contexts are
called “spaces”, and conceptually correspond to different top-level
namespaces are that implicitly used and extended.

Identifiers can be bound in a “default” space, and that binding
applies to any space that does not have a more specific binding. At
the same time, a binding may resolve to something that works only in
certain spaces, such as an expression transformer that would not apply
in an annotation space, even if the binding is visible from there; in
that case, a use of the identifier in spaces where it does not apply
is the same as using an unbound identifier.

Resolving a dotted name can involve a search though different spaces.
For example, to resolve `x.y` in space S, `x` is first checked in
space S; if it resolves to a namespace, then `y` is used from that
namespace. Otherwise, `x` is checked in the default space, and if it
is a namespace there, that namespace is used for `y`. The meaning of
`y` within that namespace tries a space S export followed by a
default-space export, and so on. If `x` is not bound to a namespace in
either S or the default space, then `x` is treated as an identifier in
space S—which will inolve another search through S and then the
default space to determine the meaning of `x` there (and the `.` after
`x` is most likely treated as an operator in that space, although it
depends on what `x` means).

The `namespace` form binds a namespace name in the default space, but
future extensions will likely allow the space for binding to be
selected. Similarly, `val`, `def`, `let`, `expr.rule`, `expr.macro`,
and `defn.macro` all bind in the default space, but `bind.rule`,
`annotation.rule`, and other forms bind in specific spaces.

The possibility of different bindings in different spaces means that
it's possible for a name to be bound to different namespaces in
different spaces, or to be bound to a namespace in one space and a
non-namespace in a different space. For the most part, being able to
bind in multiple spaces is helpful, but this possibility could create
unhelpful binding effects when programmers misue space-specific
binding capabilities.

Rationale
---------

 * On `namespace`: The original implementation used `nest`, because
   Racet already uses the word “namespace” differently. We switched to
   `namespace` and generally the meaning of “namespace” here, because
   we're able to change terminology for Rhombus, and because
   `namespace` is consistent with C++, C#, and other languages.

 * On overloading `.`: We are making `.` play multiple roles that are
   fundamentally distinct: static namespace resolution, a field
   operator, and an `import` shorthand. Different operators could be
   used, but similarities between the uses are such that `.` is
   traditionally used for all roles, and Rhombus continues that
   tradition.

 * On overloading `import`: Opening a namespace could be considered a
   completely different kind of action than importing from a module,
   and different forms like `import:` and `open:` could be used. We
   have opted to provide both operations in Rhombus through a unified
   veneer. We expect `open` on a namespace to be used rarely, in which
   case filing it under `import` seems more tidy than using up a
   top-level binding for something rarely needed.

 * On a leading `.` to indicate a namespace name: The leading `.` for
   use with `import` is not pretty at first glance, but it avoids
   potential confusion between module names, and seems good enough for
   now. We expect importing from a non-module namespace to be
   relatively infrequent.


Contributors
------------

* Robby Findler
* Jack Firth
* Matthew Flatt
* Ben Greenman
* Sid Kasivajhula
* Alex Knauth
* Sorawee Porncharoenwase
