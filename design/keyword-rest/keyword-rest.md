- Feature Name: Keyword Rest
- Start Date: 2022-07-02
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

A keyword rest parameter for function definitions and
lambdas to receive all keyword arguments in a dictionary,
and corresponding keyword rest argument syntax in function
applications to pass dictionaries in.
For completeness this also includes normal positional rest
parameters, as well as normal rest argument passing in
function applications and data constructors.

# Motivation

Associating keywords with their argument values via
a map data structure is better than having them in separate
lists whose values are only associated by index across the
lists. 
On the other side, passing dictionaries into function
applications is much more convenient than making sure the
two lists maintain their parallel and sorted order as they
go through potential mergers, additions, or removals.
A sorted map data structure can maintain the sorted order
through those operations.

# Guide-level explanation

Positional rest arguments are marked with `&`, while
keyword rest arguments are marked with `~&`. Both of these
are normal positions with ellipsis-depth 0.

Syntactic sugar `x ...` can be used at the end of function
parameters, function applications, and data constructors as
the equivalent of `& [x ...]` to give `x` ellipsis-depth 1.

## Functions

Positional rest parameters are declared with `&`, while
keyword rest parameters are declared with `~&`.

A `fun` expression with `&` of the form

```
									expression
fun (& binding) maybe_result_annotation:
  body
  ...
```

Produces a function that accepts an arbitrary number of
positional arguments and collects them in a list value
bound to `binding`.
This does not accept any keyword arguments.

A `fun` expression with `~&` of the form

```
									expression
fun (~& binding) maybe_result_annotation:
  body
  ...
```

Produces a function that accepts any keyword arguments and
collects them in a sorted-map value bound to `binding`.
This does not accept any positional arguments.

A `fun` expression with both `&` and `~&` of the form

```
									expression
fun (& binding, ~& binding) maybe_result_annotation:
  body
  ...
```

Produces a function that accepts any positional arguments
and any keyword arguments, collecting them in a list value
and a sorted-map value, respectively.

The `& binding` and the `~& binding` can appear in either
order with respect to each other, but they must come last
with respect to the other parameters.

```
									expression
fun (kwopt_binding, ..., kwrst_binding, ...) maybe_result_annotation:
  body
  ...

kwopt_binding = binding
              | keyword: binding
              | binding = default_expr
              | keyword: binding = default_expr

kwrst_binding = & binding
              | ~& binding
```

Similarly, a `fun` definition may also have `& binding`
and/or `~& binding` at the end of the parameters

```
									definition
fun identifier(kwopt_binding, ..., kwrst_binding, ...) maybe_result_annotation:
  body
  ...
```

Examples:

```
> fun p(& l):
    l
> p(1, 2, 3)
[1, 2, 3]
> fun k(~& m):
    m
> k(~scale: 1, ~dx: 7, ~dy: 11)
{keyword(~dx): 7, keyword(~dy): 11, keyword(~scale): 1}
> fun n(& l, ~& m):
    [l, m]
> n(1, 2, ~dx: 7, ~scale: 2)
[[1, 2], {keyword(~dx): 7, keyword(~scale): 2}]
```

## Function Applications

A function application expression with `&` of the form

```
									expression
expr(& expr)
```

Applies the function produced by the first `expr` with the
content of the list produced by the second `expr` as the
positional arguments.
Similar to using `(apply expr expr)` in Racket.

A function application expression with `~&` of the form

```
									expression
expr(~& expr)
```

Applies the function produced by the first `expr` with the
content of the dictionary produced by the second `expr` as
the keyword arguments.
Similar to using `(keyword-apply/dict expr expr '())` in Racket.

A function application expression with `&` and `~&` of the form

```
									expression
f_expr(& l_expr, ~& m_expr)
```

Applies the function produced by `f_expr` with the
content of the list produced by `l_expr` as the positional
arguments and the dictionary produced by `m_expr` as the
keyword arguments.
Similar to using `(keyword-apply/dict f_expr m_expr l_expr)` in Racket.

The `& expr` and the `~& expr` can appear in either
order with respect to each other, but they must come last
with respect to the other arguments.

```
									expression
expr(arg, ..., kwrst_arg, ...)

arg = expr
    | keyword: expr

kwrst_arg = & expr
          | ~& expr
```

Examples:

```
> fun add(& l -: List):
    for values(sum = 0):
      ~each i: l
      sum+i
> add(1, 2, 3, 4)
10
> add(& [1, 2, 3, 4])
10
> add(1, 2, & [3, 4])
10
> fun transform(Posn(x, y),
                ~scale: factor = 1,
                ~dx: dx = 0,
                ~dy: dy = 0):
    Posn(factor*x + dx, factor*y + dy)
> transform(Posn(1, 2))
Posn(1, 2)
> transform(Posn(1, 2), ~& {keyword(~dx): 7})
Posn(8, 2)
> transform(Posn(1, 2), ~& {keyword(~dx): 7, keyword(~scale): 2})
Posn(9, 4)
> transform(Posn(1, 2), ~scale: 2, ~& {keyword(~dx): 7, keyword(~dy): 11})
Posn(9, 15)
```

## Data Constructors

The rest marker `&` can also be used in data constructors
such as `[]` for lists and `{}` for maps.

Examples:

```
> [1, 2, & [3, 4]]
[1, 2, 3, 4]
> {"a": 1, "b": 2, & {"c": 3, "d", 4}}
{"a": 1, "b": 2, "c": 3, "d", 4}
```

## Ellipses

Syntactic sugar `x ...` can be used at the end of function
parameters, function applications, and data constructors as
the equivalent of `& [x ...]` to give `x` ellipsis-depth 1.

Examples:

```
> fun el(x ...):
    [[x, x] ...]
> el(1, 2, 3)
[[1, 1], [2, 2], [3, 3]]
> val [a ...] = ["a", "b", "c"]
> [a ...]
["a", "b", "c"]
> el("", a ...)
[["", ""], ["a", "a"], ["b", "b"], ["c", "c"], ["d", "d"]]
```

Using an identifier with ellipsis-depth 1 or more in a
context not under at least that number of ellipses is a
syntax error.

# Reference-level explanation

## Functions

```
									definition
fun identifier(kwopt_binding, ..., kwrst_binding, ...) maybe_result_annotation:
  body
  ...

									expression
fun (kwopt_binding, ..., kwrst_binding, ...) maybe_result_annotation:
  body
  ...


kwopt_binding = binding
              | keyword: binding
              | binding = default_expr
              | keyword: binding = default_expr

kwrst_binding = & binding
              | ~& binding
              | binding ...

maybe_result_annotation = : annotation
                        | ϵ
```

Binds `identifier` as a function, or when `identifier` is
not supplied, serves as an expression that produces a
function value.

## Function Applications

```
									expression
expr(arg, ..., kwrst_arg, ...)

arg = expr
    | keyword: expr

kwrst_arg = & expr
          | ~& expr
          | expr ...
```

## Data Constructors

List expressions may use either `&` for ellipsis-depth 0 or
`...` for ellipsis-depth 1

```
									expression
[expr, ..., maybe_rst_expr]

maybe_rst_expr = & expr
               | expr ...
               | ϵ
```

List bindings are similar in depth 0 `&` and depth 1 `...`

```
									binding
[binding, ..., maybe_rst_binding]

maybe_rst_binding = & binding
                  | binding ...
                  | ϵ
```

Map expressions allow `&` but not `...`

```
									expression
{expr: expr, ..., maybe_rst_expr}

maybe_rst_expr = & expr
               | ϵ
```

Map bindings are similar in allowing `&` but not `...`

```
									binding
{binding: binding, ..., maybe_rst_binding}

maybe_rst_binding = & binding
                  | ϵ
```

## Ellipses

Expressions under ellipses are evaluated under iteration
similarly to template metafunction calls under ellipses in
Racket's `syntax` form.

Examples:

```
> val [x ...]: [1, 2, 3]
> [x + 1, ...]
[2, 3, 4]
> val [y ...]: [30, 40, 50]
> [x + y, ...]
[31, 42, 53]
```

# Drawbacks and Alternatives
[drawbacks]: #drawbacks

The behavior of ellipses might be considered confusing for
"arbitrary" expressions, moreso than it would be for a
restricted subset of template syntax.
Iteration by ellipses according to ellipsis-depth might
also be hard to implement, harder for arbitrary expressions
than a restricted subset.

A potential alternative is to restrict the
bindings/expressions under ellipses to be simpler, for
example only allowing identifiers of the proper
ellipsis-depth there.

The use of the `&` rest marker in data constructors and
bindings make sense for most immutable/functional data
structures, but they make less sense for mutable data.

Some potential alternative here are to have the patterns not
support matching on the mutable versions of these data
structures at all, vs. have them create copies every time,
risking quadratic waste sneaking in, vs. have them create
slices that implement aliasing.

# Prior art
[prior-art]: #prior-art

Racket's function [`keyword-apply/dict`](https://docs.racket-lang.org/reference/dicts.html#%28def._%28%28lib._racket%2Fdict..rkt%29._keyword-apply%2Fdict%29%29).

Alex Knauth's Racket package [`kw-utils`](https://pkgs.racket-lang.org/package/kw-utils), module [`kw-utils/kw-hash-lambda`](https://docs.racket-lang.org/kw-utils/kw-hash_scrbl.html).

Jack Firth's Racket package [`arguments`](https://pkgs.racket-lang.org/package/arguments), module [`arguments`](https://docs.racket-lang.org/arguments/index.html).

Racket Users mailing list conversation [Extracting known keywords from make-keyword-procedure](https://groups.google.com/g/racket-users/c/3_Vc3t0fTGs/m/HpLZQCwADQAJ).

Rhombus prototype issue #66 [Some syntax that avoids the verbosity in the keyword-apply etc kind of functions in the common case of passing keywords around](https://github.com/racket/rhombus-prototype/issues/66).

Rhombus prototype discussion #219 [Rest keyword argument](https://github.com/racket/rhombus-prototype/discussions/219).

Python [function definitions with starargs](https://docs.python.org/3/reference/compound_stmts.html#function-definitions).

Python [function calls with `*expression` and `**expression`](https://docs.python.org/3/reference/expressions.html#calls).

Clojure [functions with `& next-param`](https://clojuredocs.org/clojure.core/fn).

Racket's [`syntax`](https://docs.racket-lang.org/reference/stx-patterns.html#%28form._%28%28lib._racket%2Fprivate%2Fstxcase-scheme..rkt%29._syntax%29%29) templates, [`...`](https://docs.racket-lang.org/reference/stx-patterns.html#%28form._%28%28lib._racket%2Fprivate%2Fstxcase-scheme..rkt%29._......%29%29) ellipses, [`syntax/parse`](https://docs.racket-lang.org/syntax/stxparse.html), and [`define-template-metafunction`](https://docs.racket-lang.org/syntax/Experimental.html#%28form._%28%28lib._syntax%2Fparse%2Fexperimental%2Ftemplate..rkt%29._define-template-metafunction%29%29).

# Unresolved questions
[unresolved]: #unresolved-questions

Ellipses bring up many unresolved questions.
If those are out of scope for the design of rest arguments and
keyword arguments, `...` ellipses can be omitted for now,
and rest arguments can still be expressed with `&`.
Then once ellipses are figured out they could be added in
the future with syntactic sugar translating `x ...` into
`& [x ...]`.

There are also unresolved questions for the behavior of the
`&` rest marker in the `{}` map constructor.
Should it only accept maps there?
With the multiple kinds of maps, should it only accept maps
of the same kind, or all kinds?
Or should it accept a generic interface, such as
dictionaries, multidicts, or even all bisequences?
Should "association lists" as mere lists of pairs be
included in those interfaces?

More unresolved questions on the behavior of the `&` rest
marker in patterns such as `[]` and `{}`: if those can
match mutable versions of their respective data structures,
should those patterns support `&`, or should they fail to
match or even error on the attempt?
Or should they copy the data and risk quadradic waste?
Or should they create slices that implement aliasing?
If so how do the slices behave on mutation of each data
structure, in both directions?
