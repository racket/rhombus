# Dot providers and other static information

An expression that can be used to the left of a `.` is a _dot
provider_. More generally, a binding or an expression can have _static
information_. Being a dot provider means having one kind of static
information, but other kinds of static information—in combination with
expression forms that recognize that information—may lead to a
surrounding expression being a dot provider.

In addition to dot providers, core Rhombus recognizes _result
information_ to be used by a function-call form. In the proposal's
`Line` example, `Line` is a dot provider, so that `Line.p1` is
allowed; but `Line.p1` has static information to say that its result
is a dot provider. The static information on `Line.p1` is given to it
by the `Line` dot expander. When `Line.p1` is used in the function
position of a function-call form, the function-call form unwraps
result information and associates the unwrapped static information to
the call expression as a whole. That information makes the call
expression a dot provider to access `x` and `y`, and that's why
`Line.p1(e).x` can be syntactically valid (for any `e`).

Note that static information is associated with an expression, not a
value. So, if `Line.p1` is used an argument to a function (instead of
being called), then the function ultimately receives a value and knows
nothing of the expression that generated the value. That is, no part
of the function's implementation can take advantage of the fact that
directly calling `Line.p1` would have formed a dot provider.

Static information for an expression is represented in a key–value
form. Internally, being a dot provider means having static information
keyed by `dot_ct.provider_key`. Function calls, meanwhile, recognize
static information keyed by `expr_ct.call_result_key`, and the value
with `expr_ct.call_result_key` is static information to associate with
a function-call expression. That static information might include a
`dot_ct.provider_key` entry, making the function call as dot provider.
It might include another `expr_ct.call_result_key`, which would make
sense if the function is curried. Or it might be other static
information that is useful to other enclosing syntactic forms.

A binding form can associate static information to every use of a
binding. Most commonly, that association is performed through a
contract with the `::` binding operator. A contract pairs a predicate
with a set of static information to associate with any variable that is
bound with the contract. The static information is propagated to every
use of the bound variable. That's why a binding `p :: Posn` makes
every `p` a dot provider: the contract `Posn` indicates that every
binding with the contract gets a dot provider to access `x` and `y`.
When `::` is used in an expression, then static information indicated
by the contract is similarly associated with the overall `::`
expression, which is why `(e :: Posn)` is a dot provider for any
expression `e`.

## Rules for predefined forms

To summarize the dot-provider and static-information behavior of
structure types:

 * A structure-type name bound by `struct` is a dot provider. It
   provides field-accessor functions, as in `Posn.x`.

 * As a contract, a structure-type name makes any binding or
   expression using the contract a dot provider.

 * When a structure-type field has a contract, then that contract's
   static information is associated with a field accessed through `.`.
   In the `Line` example, the `p2` field of `Line` has a `Posn`
   contract, so a `l1 :: Line` binding means that `l1.p2` is a dot
   provider to access `x` and `y`.

 * When a structure-type field has a contract, then that contract's
   static information is associated as result information for a field
   accessor accessed through `.`. For example, `Line.p1` gets the
   `Posn` contract's static information as its _result_ information,
   so `Line.p1(e)` has that information, which means that `Line.p1(e)`
   is a dot provider.

 * When a structure-type field has a contract, and when the structure
   type is used as a pattern form in a binding, then the contract's
   static information is associated with the argument pattern. For
   example, `Line(p1, p2)` as a binding pattern associates `Posn`
   information to `p1` and to `p2`, which means that they're dot
   providers.
   
More rules about _result_ information and static information in general:

 * A expression that is parentheses wrapped around an inner expression
   has the same static information as the inner expression.

 * When a `fun` defintition form include a result contract, then the
   contract's information is associated to the defined function name
   as result information. For example, if a function defintion starts
   `fun flip(x) :: Posn`, then `Posn` static information is associated
   to `flip` as result information, so `flip(x)` is a dot provider.
   The same applies to a `def` form that behaves like a `fun`
   definition form.

 * When a function call's function position has result static
   information, the function call as a whole is given that static
   information. For example, since `Line.p1` has result information
   that describes a dot provider, `Line.p1(e)` is a dot provider.

 * When `val` or `def` is used with a plain-identifier binding, when the
   right-hand side contains only a (possibly parenthesized) call to a
   previously defined (possibly parenthesized) function name, and
   which that function name has _result_ information, then the
   unwrapped result information is
   associated to the binding. For example, with `val origin: Posn(0,
   0)`, because `Posn` has result information that describes a dot
   provider, the `origin` identifier is a dot provider. _This rule
   considers a right-hand side of `val` or `def` that is not yet
   parsed, unlike all other rules listed here._

The last rule above means that the following definition `l1` does not
need an explicit `:: Line` declaration after `l1`:

```
def l1: Line(Posn(1, 2), Posn(3, 4))
```

_Why does the type-like propagation of contracts go this far and no
 further? See the current
 [rationale](0000-rhombus.md#rationale-and-alternatives)._


## API

The `contract.macro` form defines a contract. In the simplest case,
the expansion of a contract can be another contract:

```
contract.macro ?AlsoPosn: ?Posn

Posn(1, 2) :: AlsoPosn  // prints Posn(1, 2)
```

Note that `contract.macro` defines only a contract. To make `AlsoPosn`
also a binding operator, use `bind.macro`, and so on:

```
bind.macro ?(AlsoPosn (¿x, ¿y) ¿tail ...):
  values(?(Posn(¿x, ¿y)), tail)

def AlsoPosn(x, y): Posn(1, 2)
x  // prints 1
```

To define a contract with explicit control over the associated
predicate, use `contract_ct.pack_predicate`. Here's another way to
define `AlsoPosn`, where the implementation creates a new predicate
that uses `is_a` with `Posn`:

```
contract.macro ?AlsoPosn:
  contract_ct.pack_predicate(?(fun (x): x is_a Posn))
```

The `contract_ct.pack_predicate` takes an optional second argument,
which is static information to associate with uses of the contract.
Static information (the second argument to
`contract_ct.pack_predicate`) is a a parenthesized sequence of
parenthesized two-group elements, where the first group in each
element is a key and the second element is a value.

A value for `dot_ct.provider_key` should be a syntax object naming a
dot-provider transformer. So, if we want to define a `Vector` contract
that is another view on `Posn` where the ”fields” are `angle` and
`magnitude` instead of `x` and `y`, we start with a contract
definition that refers to a `vector_dot_provider` that we will define:

```
contract.macro ?Vector:
  contract_ct.pack_predicate(?(fun (x): x is_a Posn),
                             ?((¿(dot_ct.provider_key), vector_dot_provider)))
```

A dot-provider transformer is defined using `dot.macro`. A
dot-provider transformer always receives three parts, which are the
parsed expression to the left of the dot, the dot itself, and an
identifier to the right of the dot. The dot provider associated with
`Vector` access `angle` and `magnitude` “fields” by calling helper
functions:

```
dot.macro ?(vector_dot_provider ¿left ¿dot ¿right):
  match right
   | ?angle: ?(vector_angle(¿left))
   | ?magnitude: ?(vector_magnitude(¿left))

fun vector_angle(Posn(x, y)): atan(y, x)
fun vector_magnitude(Posn(x, y)): sqrt(x*x + y*y)
```

With those pieces in place, a binding using `:: Vector` creates a dot
provider:

```
def vec :: Vector: Posn(3, 4)
vec.angle      // prints 0.9272952180016122
vec.magnitude  // prints 5
```

A macro can explicitly associate static information with an expression
by using `static_info_ct.wrap`:

```
expr.macro ?(or_zero ¿p ¿tail ...):
  val expansion: ?(¿p || Posn(0,0))
  values(static_info_ct.wrap(expansion,
                             ?((¿(dot_ct.provider_key),
                                vector_dot_provider))),
         tail)
  
or_zero(Posn(3, 4)).magnitude // prints 5
or_zero(#false).magnitude     // prints 0
```

A similar effect could be acheived by expanding to `?((¿p ||
Posn(0,0)) :: Vector)`, but for better or worse, this implementation
of `or_zero` omits an extra predicate on the result of the expression,
and instead claims that it will always work as a `Vector`.

If a name is otherwise bbound by has no static information associated
with the binding, the `static_info.macro` form can associate static
information. In the following example, `zero` is defined without a
result contract, but `static_info.macro` is used to associate static
information to `zero` using `expr_ct.call_result_key`. The value for
`expr_ct.call_result_key` should be static information itself, so we
use `static_info_ct.pack` to pack it from a syntax-object
representation.

_The `static_info_ct.wrap` and `contract_ct.pack_predicate` functions
automatically pack for you, because they expect a syntax object that
represents static information. The overall right-hand side result for
`static_info.macro` is similarly automatically packed._

```
fun zero(): Posn(0, 0)
static_info.macro ?zero: ?((¿(expr_ct.call_result_key),
                            ¿(static_info_ct.pack(?((¿(dot_ct.provider_key),
                                                     vector_dot_provider))))))
zero().magnitude  // prints 0
```

The `static_info.macro` form expects `?` followed by an
identifier or operator, not a more funciton-like pattern, because its
mean to define a constant association between a name and static
information.


A dot provider like `vector_dot_provider` normally would not be
exposed outside of the module that implements `Vector`. But if a dot
provider is used directly, then it receives itself as the left
argument:

```
dot.macro ?(hello ¿left ¿dot ¿right):
  match right
   | ?english: ?"Hi"
   | ?chinese: ?"你好"
   | ?spanish: ?"Hola"

hello.chinese  // prints "你好"
hello.spanish  // prints "Hola"
hello.english  // prints "Hello"
// hello.greek  // would be a compile-time match error
```

A direct use like this makes sense when a dot provider is not
associated with a run-time value. Attempting to use `hello` in an
expression position other than before a `.` results in a static error.
