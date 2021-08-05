# Dot providers

Rhombus forms must specify when they create dot providers. For the
forms covered in this proposal, here are all the cases that create dot
providers (modulo parentheses, which can be added around any
expression except as noted):

 * A structure-type name bound by `struct` is a dot provider. It
   provides field-accessor functions, as in `Posn.x`.

 * An identfier that is bound using the `::` binding operator and a
   structure-type name is a dot provider. It provides field values, as
   in `p.x` when `p` is bound using `p :: Posn`.

 * A `::` expression with a structure-type contract on the right is a
   dot provider. For example `e :: Posn` is a dot provider, no matter
   what expression is used in place of `e`, and so `(e :: Posn).x`
   will be syntactically valid.

 * An expression that uses `.` to access a field of an instance is a
   dot provider if the accessed field has a structure-type contract.
   In the example above, `l1.p2` is a dot provider, and that's why
   `l1.p2.x` is valid.

 * When an expression uses `.` to get an accessor function from a
   structure-type name, when that expression is immediately called as
   a function, and when the accessed field has a structure-type
   contract, then the function call is a dot provider. For example,
   `Line.p1(e)` is a dot provider, no matter what expression is used
   in place of `e`, and so `Line.p1(e).x` will be syntactically valid.

 * When a function name is bound using `fun` or `def` with a result
   contract that is a structure type, then a function call expression
   that uses that name directly is a dot provider. For example,
   `same_posn(e)` is a dot provider, since `same_posn` is defined
   using `fun` and the result contract `Posn`, and `same_posn(e).x` is
   syntactically valid.

 * When `val` or `def` is used with a plain-identifier binding and the
   right-hand side contains only an unparenthesized call to a
   previously defined (and unparenthesized) structure-type constructor
   or a function with a return contract, then the result contract is
   implicitly applied to the binding. Consequently, a use of the
   identifier is a dot provider. For example, with `val origin:
   Posn(0, 0)`, then each use of `origin` is a dot provider.

The last rule above means that the earlier example definition of `l1`
does not need an explicit `:: Line` declaration:

```
def l1: Line(Posn(1, 2), Posn(3, 4))
```

_Why does the type-like propagation of contracts go this far and no
 further? See the current
 [rationale](0000-rhombus.md#rationale-and-alternatives)._

New syntactic forms can add new dot-provider expression forms. For
exmaple, a macro-implemented expression operator might expand to a use
of `::` to give an expression or a binding a structure-type contract.

At a lower level, a new contract form can combine a predicate with a
dot provider to associate with the places where the contract is used,
the same as a structure-type name does.

```
// not real code...

contract.macro Pair:
  ?(is_pair, pair_dot_provider)

contract.dot_provider ?(pair_dot_provider ¿left ¿right ¿tail ...):
  values(match right
          | ?fst: ?(pair_fst(¿left))
          | ?snd: ?(pair_snd(¿left)))

fun 
  | is_pair(cons(a, d)): #true
  | is_pair(v): #false
fun pair_fst(cons(a, d)): a
fun pair_snd(cons(a, d)): a  

let p :: Pair: cons(1, 2)
p.fst  // prints 1
```

