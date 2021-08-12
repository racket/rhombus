# Contracts

Contracts produce [static information](static-info.md) when used with
the `::` binding or expression operator. Similar to binding macros,
which can either be simple expansions or use lower-level machines, a
contract macro can use lower-level machinery to explicitly produce
static information or manipulate static information produced by
subcontract forms.

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
predicate, use `contract_ct.pack_predicate`. This implementation if
`IsPosn` creates a new predicate that uses `is_a` with `Posn`, so it
checks whether something is a `Posn` instance, but it doesn't act as a
`Posn`-like binding form or constructor:

```
contract.macro ?IsPosn:
  contract_ct.pack_predicate(?(fun (x): x is_a Posn))

fun get_x(p :: IsPosn): Posn.x(p)
get_x(Posn(1, 2)) // prints 1
// get_x(10)      // would be a run-time error
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

If a name is otherwise bound but has no static information associated
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
identifier or operator, not a more funciton-like pattern, because it's
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
