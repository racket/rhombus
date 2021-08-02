# Low-level binding macros

A binding form using the low-level protocol needs three parts:

 * An identifier that is used as a name for the input value, at last
   to the degree that the input value uses an inferred name. The
   binding form might bind this name in the third part, but it is not
   obligated to do so.

 * A transformer function that generates code that checks the input
   value for a match. The generated block may include additional
   definitions before branching or after branching toward success, but
   normally no bindings visible to code around the binding are created
   at this step.

 * A transformer function that generates definitions for the bound
   variables. These definitions happen only after the match is
   successful, and the bindings are visible only after the matching
   part of the expansion. Also, these bindings are the ones that
   are affected by `forward`.

To make binding work both in definition contexts and `match` search
contexts, the check-generating transformer function must be
parameterized over the handling of branches. Toward that end, it
recieves three extra arguments: the name of an `if`-like form that
we'll call `IF`, a `success` form, and a `failure` form. The
transformer uses the given `IF` to branch to a block that includes
`success` or just `failure`. The `IF` form must be used in tail
position with respect to the generated code, where the “then” part of
an `IF` is still in tail position for nesting. The transformer must
use `failure` and only `failure` in the “else” part of each `IF`, and
it must use `success` exactly once within a “then” branch of one or
more nested `IF`s.

Unfortunately, there's one more complication. The result of a macro
must be represented as syntax—even a binding macro—and a transformer
function as a first-class compile-time value should not be used as
syntax. (Such representation are sometimes called “3-D syntax,” and
they're best avoided.) So, a low-level binding macro must uses a
defunctionalized representation of its identifier and two functions,
which makes it four parts:

 * an identifier that will be bound to the binding input value;

 * the name of a compile-time transformer function that is bound with
   `binding_match_macro`; and

 * the name of a compile-time transformer function that is bound with
   `binding_bind_macro`; and

 * data as a syntax object to pass to each of the transformer
   functions (effectively: the fused closure for those two functions).

These four pieces are assembled into a parenthesized-tuple syntax
object, and then packed with the `pack_binding` function to turn it
into a valid binding expansion. The transformer functions are called
with a variable for the matcher input as a first argument and the
transformer's data as the second argument. The match-building
transformer in addition receives the `IF` form name, a `success` form,
and a `failure` form.

_There's almost certainly a better approach to low-level binding
macros than exposing pieces this way, but this is how far I've gotten,
and maybe there's some value to spelling out this machinery._

Here's a use of the low-level protocol to implement a `fruit` pattern,
which matches only things that are fruits according to `is_fruit`:

```
binding_macro ?(fruit(¿id) ¿tail ...):
  values(pack_binding(?(¿id,
                        build_fruit_match,
                        build_fruit_bind,
                        // builder needs id:
                        ¿id)),
         tail)

binding_match_macro ?(build_fruit_match(¿arg, ¿id, ¿IF, ¿success, ¿failure)):
  ?{
    ¿IF is_fruit(¿arg)
    | ¿success
    | ¿failure
  }

binding_bind_macro ?(build_fruit_bind(¿arg, ¿id)):
  ?{
    define ¿id: ¿arg
  }

value fruit(snack): "apple"
snack // prints "apple"

// value fruit(dessert): "cookie"  // would fail with a match error
```

The `fruit` binding form assumes (without directly checking) that its
argument is an identifier. Binding forms normally should accomodate
other, nested binding forms, instead. A `binding_operator` transformer
receives already-parsed sub-bindings as arguments, and the transformer
can use `unpack_binding` to extract the three parts of a parsed
binding.

As an example, here's an infix `<&>` operator that takes two bindings
and makes sure a value can be matched to both. The binding forms on
either size of `<&>` can bind variables. The `<&>` builder is
responsible for binding the input name that each sub-binding expects
before it deploys the corresponding builder. The only way to find out
if a sub-binding matches is to call its builder, providing the same
`IF` and `failure` that the original builder was given, and possibly
extending the `success` form. A builder must be used in tail position,
and it's `success` position is a tail position.

```
binding_operator ?(¿a <&> ¿b):
  match unpack_binding(a)
   | ?(¿a_id, ¿a_matcher, ¿a_binder, ¿a_data):
       pack_binding(?(¿a_id,
                      build_anding_match,
                      build_anding_bind,
                      (¿a, ¿b)))

binding_match_macro ?(build_anding_match(¿in_id, (¿a, ¿b),
                                         ¿IF, ¿success, ¿failure)):
   match unpack_binding(a)
   | ?(¿a_id, ¿a_matcher, ¿a_binder, ¿a_data):
       match unpack_binding(b)
        | ?(¿b_id, ¿b_matcher, ¿b_binder, ¿b_data):
            ?{
              ¿a_matcher(¿in_id, ¿a_data, ¿IF,
                         ¿b_matcher(¿in_id, ¿b_data, ¿IF, ¿success, ¿failure),
                         ¿failure)
            }

binding_bind_macro ?(build_anding_bind(¿in_id, (¿a, ¿b))):
  match unpack_binding(a)
   | ?(¿a_id, ¿a_matcher, ¿a_binder, ¿a_data):
       match unpack_binding(b)
        | ?(¿b_id, ¿b_matcher, ¿b_binder, ¿b_data):
            ?{
              ¿a_binder(¿in_id, ¿a_data)
              ¿b_binder(¿in_id, ¿b_data)
            }

value one <&> 1: 1
one  // prints 1
// value two <&> 1: 2 // would fail, since 2 does not match 1

value Posn(0, y) <&> Posn(x, 1) : Posn(0, 1)
x  // prints 0
y  // prints 1
```

One subtlety here is the syntactic category of `IF` or a builder call.
The `IF` form might be a definition form, or it might be an expression
form, and a builder is expected to work in either case, so a builder
call's category is the same as `IF`. An `IF` alternative is written as
a block, as is a `success` form, but the block may be inlined into a
definition context.
