# Static information: dot providers and more

A binding or an expression can have associated _static information_
that is used to enable, reject, or resolve certain expression forms.
For example, an expression can be used to the left of a `.` only when
it has static information to specify how a field name after `.`
resolves to an field accessor. See the [main
document](0000-rhombus.md#static-information) for an overview of
static information and its role.

## Representing static information

Static information for an expression or binding is represented in
key–value form. When static information is associated with a binding,
it is propagated to each use of the bound variable, so we can refer to
an expression E that has static information without loss of
generality.

The prototype implementation of Rhombus currently uses five built-in
static-information keys:

 * `dot_ct.provider_key` --- names a compile-time function that
   macro-expands any use of `.` after E. For example, assuming a
   `struct Posn(x, y)` declaration, `p :: Posn` associates
   `dot_ct.provider_key` with uses of `p` to expand `p.x` and `p.y` to
   field accesses. An expression is a _dot provider_ when it has
   static information mapped by `dot_ct.provider_key`.

 * `expr_ct.call_result_key` --- provides static information to be
   attached to a function-call expression where the function position
   is E. (The arguments in the call do not matter.) For example,
   `struct Posn(x, y)` associates `expr_ct.call_result_key` with
   `Posn` itself, and the associated value is static information with
   `dot_ct.provider_key`. So, `Posn(1, 2)` gets `dot_ct.provider_key`
   that makes `Posn(1, 2).x` select the `1`.

 * `expr_ct.ref_result_key` --- provides static information to be
   attached to a `[` ... `]` map reference where E is to the left
   of the `[` ... `]`. (The index expression inside `[` ... `]` does
   not matter.) For example `ps :: List.of(Posn)` associates
   `expr_ct.ref_result_key` to `ps`, where the associated value
   includes is static information with `dot_ct.provider_key`. So,
   `ps[i].x` is allowed an selects an `x` field from the `Posn`
   instance produced by `ps[i]`.

 * `expr_ct.map_ref_key` and `expr_ct.map_set_key` --- names a form to
   use for a `[` ... `]` map reference or assignment where E is to
   the left of the `[` ... `]`. (The index expression inside `[` ...
   `]` does not matter.) For example `p :: Array` associates
   `expr_ct.map_ref` to `p` so that `p[i]` uses an array-specific
   referencing operation, and it associates `expr_ct.map_ref` to
   `p` so that `p[i] = n` uses an array-specific assignment operation.

In addition, a `struct` declaration generates a fresh key for each
every field in the declared structure type. The value to be associated
with the key is analogous to `expr_ct.call_result_key` or
`expr_ct.ref_result_key`, but for references to the field through `.`
or through an accessor function like `Posn.x`.

Static information is associated to a binding through a binding
operator/macro, and it can be associated to an expression through a
binding or through an expression operator/macro that adds static
information to its parsed form (i.e., expansion). For example, the
`::` operator associates static information through a contract. A
contract pairs a predicate with a set of static information to
associate with any variable that is bound with the contract. That's
why a binding `p :: Posn` makes every `p` a dot provider: the contract
`Posn` indicates that every binding with the contract gets a dot
provider to access `x` and `y`. When `::` is used in an expression,
then static information indicated by the contract is similarly
associated with the overall `::` expression, which is why `(e ::
Posn)` is a dot provider for any expression `e`. Function-call forms
and map-reference forms similarly attach static information to
their parsed forms, sometimes, based on static information attached to
the first subexpression.

Note that static information is associated with an expression, not a
value. So, if `Posn` is passed as an argument to a function (instead
of being called directly as a constructor), then the function
ultimately receives a value and knows nothing of the expression that
generated the value. That is, no part of the function's implementation
can take advantage of the fact that directly calling `Posn` would have
formed a dot provider. The function might have an contract on its
argument that indicates a dot-provider constructor, but that's a
feature of the formal argument, and not of an actual value.

## Rules

Exactly how static information is associated to expressions and
bindings depends on the expression or binding form. So, it's not
possible to write down exhaustive type-like rules for for Rhombus
static information in the same way that it's not possible to write
down a full grammar. But in the same way that syntactic forms can be
defined with local grammars over a notion of expression,
static-information uses cn be decribed locally over a notion of
expressions and binding.

Below is how some rules for basic built-in forms might be written.
Read `Γ` as “the local evironment mapping names to static
information”, and read `τ` as “static information”. Read `→` as a
compile-time expansion.

```
    Γ ⊢ id : Γ(id)

       Γ ⊢ e : τ
      -----------
      Γ ⊢ (e) : τ

      Γ ⊢ e_1 : τ 
 τ(call_result) = τ_r
----------------------
   Γ ⊢ e_1(e_2) : τ_r

      Γ ⊢ e_1 : τ 
  τ(ref_result) = τ_r
 ---------------------
   Γ ⊢ e_1[e_2] : τ_r

      Γ ⊢ e_1 : τ
   τ(dot_provider) = p
    p(e_1, id) → e_2
      Γ ⊢ e_2 : τ_r
  --------------------
    Γ ⊢ e_1.id : τ_r
```

More work will be needed on notation, but it should be just a matter
of adapting type (and, for bindings, bidirectional-type) notation.
Ideally, as in Turnstile, binding and expression forms could be
implemented in something like this notation, in much the same way that
macros are written in a by-example way that is inspired by grammar
notation.

## Rules for predefined forms

Here's a summary of the static-information behavior of structure
types:

 * A structure-type name bound by `struct` is a dot provider. It
   provides field-accessor functions, as in `Posn.x`.

 * As a contract, a structure-type name makes any binding or
   expression using the contract a dot provider. A structure type name
   followed by `.of` has the same effect; in addition, it associates
   any information implied by the argument contracts as static
   information for fields accessed from the binding or exression
   through a dot. For example, assuming a `struct Rect(top_left,
   side)` declaration, `r :: Rect.of(Posn, Integer)` causes
   `r.top_left` to have `Posn` information, with means that
   `r.top_left.x` works.

 * When a structure-type field has a contract, then that contract's
   static information is associated with a field accessed through `.`.
   In the `Line` example, the `p2` field of `Line` has a `Posn`
   contract, so a `l1 :: Line` binding means that `l1.p2` is a dot
   provider to access `x` and `y`.

 * When a structure-type field has a contract, then that contract's
   static information is associated as result information for a field
   accessor accessed through `.`. For example, `Line.p1` gets the
   `Posn` contract's static information as its call-result
   information, so `Line.p1(e)` has that information, which means that
   `Line.p1(e)` is a dot provider.

 * When a structure-type field has a contract, and when the structure
   type is used as a pattern form in a binding, then the contract's
   static information is associated with the argument pattern. For
   example, `Line(p1, p2)` as a binding pattern associates `Posn`
   information to `p1` and to `p2`, which means that they're dot
   providers.

 * When a structure-type name is used as a binding pattern, any
   “downward” static information that flows the binding is checked for
   static information keyed by the structure type's accessors, and
   that information is propoagated as “downward” information to the
   corersponding binding subpattern. For example, if `Rect(tl, s)` as
   a binding receives “downward” information that associates (the
   internal key for) `Rect.top_left` to `Posn`-contract information,
   then the binding form `tl` receives `Posn`-contract information.

More rules about static information in general:

 * A expression that is parentheses wrapped around an inner expression
   has the same static information as the inner expression.

 * When a function call's function position has result static
   information, the function call as a whole is given that static
   information. For example, since `Line.p1` has result information
   that describes a dot provider, `Line.p1(e)` is a dot provider.

 * When a `fun` defintition form includes a result contract, then the
   contract's information is associated to the defined function name
   as call-result information. For example, if a function defintion
   starts `fun flip(x) :: Posn`, then `Posn` static information is
   associated to `flip` as result information, so `flip(x)` is a dot
   provider. The same applies to a `def` form that behaves like a
   `fun` definition form.

 * When the right-hand side of a `val`, `def`, or `let` has a single
   group, and when that goes does not start with a definition-form
   name, then static information from that right-hand side expression
   is propagated to the binding side. For example, `val p: Posn(1, 2)`
   associated that static information of `Posn(1, 2)` with `p`, which
   among other things means that `p.x` will be allowed.

The `List`, `Array`, and `Map` expression and binding forms are
analogous to structure-type forms. For example, `Array` as a
constructor in an expression form associates reference-result
information to the overall `Array` expression, as does `[` ... `]` for
constructing a list. In a list binding pattern, when `...` is used
after a binding subpattern, the “upward” static information of the
subpattern for each identifier wrapped as reference-result information
for the identifier outside the list pattern, since each; for example
`[p :: Posn, ...]` as a binding pattern causes `p` to have static
information that says its reference result as `Posn`-contract
information. The `List.of`, `Array.of`, and `Map.of` contract forms in
bindings propagate “downward“ reference-result information to nsted
contracts. “Downward” static information is used by `List` or `[` ...
`]` pattern constructions only in the case that there's a single
element binding pattern followed by `...`, while `Array` and `Map` as
pattern constructors cannot use “downward” information.

The `::` binding form and the `matching` contract form allow static
information to flow both “downward” and “upward” through both
contracts and binding patterns.

## Binding patterns and static information

See [the low-level binding API](binding-macros.md) for information on
how binding macros receive and produce static information. A
Turnstile-like sets of forms should be built on top of that low-level
API.

## Contracts and static information

See [the low-level contract API](contract-macros.md) for information on
how contract macros receive and produce static information. A
Turnstile-like sets of forms should be built on top of that low-level
API, too.
