- Feature Name: Enforestation and expansion for shrubberies
- Start Date: 2021-07-14
- RFC PR: [racket/rhombus-brainstorming#162](https://github.com/racket/rhombus-brainstorming/pull/162)

# Summary
[summary]: #summary

[Shrubbery notation](https://github.com/mflatt/rhombus-brainstorming/blob/shrubbery/shrubbery/0000-shrubbery.md)
specifies how to parse a sequence of characters into a coarse-grained
block structure, but it leaves the interpretation of that block
structure to another layer of parsing—not to mention more fine-grained
grouping between lexing and block structure. This proposal describes
an adaptation of the Racket and Honu parsing techniques of _expansion_
and _enforestation_ targeted to shrubbery notation.

# Motivation
[motivation]: #motivation

Shrubbery notation is a vehicle for using a programming language in
much the same way that S-expression notation is a vehicle. Like
Racket's macro system as built on S-expressions, this proposal
introduces an expander layer that is built on shrubberies.

Here's an example of the kind of language that the expander is meant
to support:

```
define home: Posn(3, 7)

define manhattan_distance(p :: Posn):
  abs(home.x - p.x) + 5 * abs(home.y - p.y)

define another_manhattan_distance(Posn(x, y)):
  abs(home.x - x) + 5 * abs(home.y - y)

define
 | should_take_cab(Posn(0, 0)): #false
 | should_take_cab(p :: Posn):
     manhattan_distance(p) > 10 || currently_raining()
 | should_take_cab(#false): #false
```

The intent here is that `define` is macro-implemented and recognizes
various forms of definitions, including simple binding, function
forms, and match-dispatch function forms. The `define` macro is not
meant to know about `::` specifically; the `::` is meant to be a binding
operator that checks whether the value flowing to the binding
satisfies a predicate, and it also binds compile-time information
indicating that `p` is known to be a `Posn` instance. The name `Posn`
works in an expression to construct a position value, while in a
binding position, `Posn` also works to construct a pattern-matching
binding (again, without `define` knowing anything specific about
`Posn`). Meanwhile, `.`, `-`, `+`, `*`, `<`, and `||` are the obvious
operators with the usual precedence. Unlike the other operators, the
`.` operator's right-hand side is not an expression; it must always be
an identifier.

To define a new expander layer for shrubbery notation, we take
Racket's primitives as given: syntax objects, scopes, modules, and
more. We also recycle some variant of Racket as the target for
shrubbery expansion. In principle, the variant could be minimal,
corresponding to the core forms that all Racket modules expand into,
but some larger variant (including keyword arguments, for example) is
likely a better choice of interoperability with Racket modules.
Finally, the enforestation and expansion process here are defined in
terms of the S-expression form of parsed shrubbery notation (really,
syntax-object form, so it can include scopes to determine a mapping for
identifiers and operators). Even so, a gap remains between shrubbery
notation and Racket's macro expander, because shrubbery notation is
intended to be used with less grouping than is normally present in
S-expressions.

This proposal does not go as far as specifying a language with
specific definition forms, expression forms, and so on. The proposal
only specifies an extensible traversal of shrubberies to turn them
into Racket terms. In particular, this proposal addresses the problem
of parsing a mixture of operators and other terms within a group,
allowing the definition of infix and prefix operators for expression
and binding positions. The handling of operators is directly based on
Honu, but with several refinements—including a framework for relative
and non-transitive operator precedence that is more like Fortress than
Honu.

Although this proposal does not specify definition forms, we assume
some way of mapping an identifier or operator to a compile-time
entity, such as a macro transformer. (Throughout this proposal, we use
“mapping” to refer to this binding in the sense of `define-syntax`, as
opposed to binding positions in a language that is built with the
Rhombus expander.) We also assume that mappings can be local, so that
delaying the expansion of nested groups is useful while compile-time
mappings are created by surrounding terms.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

For brevity, we call this proposal's content “Rhombus expansion,” even
though it does not define a full candidate Rhombus language, and
although it's in many ways independent of a specific language. That's
similar to referring to “Racket expansion,” by which we do not
necessarily mean something involving `#lang racket`.

_Suggestion:_ skip the “API” part of each section on a first reading.

## Syntactic categories and transformer kinds

Rhombus expansion involves four syntactic categories:

 * declarations (in a module's immediate body or at the top level)

 * definitions

 * expressions

 * bindings

The first three exist in Racket's expander as reflected by
`syntax-local-context`. The fourth corresponds to `match` patterns,
and we make them more pervasive in Rhombus. For its three syntactic
categories, the Racket expander has only one kind of transformer as
one kind of compile-time value: a procedure of arity 1; nevertheless,
some macros work only in, say, definition positions or in a module
body. Rhombus expansion uses different kinds of compile-time values
for its different expansion positions, so a mapping can effectively
declare where it's mean to be used. The different kinds of values are
implemented through structure-type properties, so a compile-time value
can implement multiple kinds to create a mapping that is allowed in
multiple contexts (especially expressions versus bindings).

The relevant syntactic category for a shrubbery is determined by its
surrounding forms, and not inherent to the shrubbery. For example,
`cons(x, y)` might mean one thing as an expression and another as a
binding. Exactly where the contexts reside in a module depends on a
specific Rhombus language that is built on the Rhombus expander, so
it's difficult to say more here. Meanwhile, a full Rhombus language
may have more syntactic categories than the ones directly supported by
the expander, in the same way that a Racket program can have more
through syntactic extensions like `match` or Typed Racket. The four
categories for the Rhombus expander are merely the ones that are
directly supported by the expander and its API.

Within any category, the rules for operator expansion differ from the
rules for identifier expansion. Furthermore, operators can either act
like macros and take over parsing after the operator (useful for a `.`
operator, for example), or they can be more like variables, translated
based on already-translated arguments. Operators used in expressions
and operators used in bindings are represented as different kinds of
compile-time values—but, again, an operator's compile-time value can
implement both.

Only expression and binding contexts support operators. That is, the
Rhombus expander will dispatch on operator mapping only during the
expansion of expressions. A macro transformer that implements a
declaration or definition is free to consult an operator's mapping,
but whatever it does with that information is up to the transformer.
The practical result is that declaration and definition forms are
(like Racket) based on a leading identifier.

### API

The Rhombus expander defines three Racket macros and six syntax
classes that serve as entry points to the Rhombus expander:

 * The `rhombus-top` form is normally be used in a `#%module-begin`
   implementation to wrap the result of the shrubbery parser. It
   expands at the Racket level by Rhombus-expanding a sequence of
   declaration, definition, and expression forms.

 * The `rhombus-block` form can be used to create a nested
   binding/mapping context. It expands to Rhombus-expand a sequence of
   definition and expression forms, requiring the last expansion to be
   an expression.

 * The `rhombus-expression` form expands to Rhombus-expand (with
   enforestation) a single expression.

 * The `:declaration` syntax class matches and eagerly Rhombus-expands
   a single declaration form. The result is a sequence of Racket
   declarations, definitions, and expressions.
   
 * The `:definition` syntax class matches and eagerly Rhombus-expands
   a single definition form. The result is a sequence of Racket
   declarations and expressions.

 * The `:expression` syntax class matches and eagerly Rhombus-expands
   a single expression form. The result is a Racket expression.

 * The `:binding` syntax class matches and eagerly Rhombus-expands a
   single binding form. The result combines information about a
   matcher and result bindings.

 * The `:op+expression+tail` syntax class matches and eagerly
   Rhombus-expands a single expression form after a leading operator,
   where the operator is used only for its precedence information to
   determine the end of the expression (if an infix operator of lower
   precedence is encountered). This syntax class is meant to be used to
   continue parsing an expression. The result is a Racket expression
   and a sequence of remaining terms.

 * The `:op+binding+tail` syntax class is analogous to
   `:op+expression+tail`, but for binding bindings.

Naturally, a Rhombus program will not refer to these Racket entities
directly, but they are used to implement Rhombus. But a Rhombus
program that defines new transformers will use forms provide by
Rhombus that ultimately make use of these Racket entities.

## Identifier transformers

The Rhombus expander checks an identifier's mapping for a transformer
when the identifier is at the start of a group or when it starts an
expression or binding in other forms. An identifier is bound to a
transformer using Racket's mapping machinery (`define-syntax`, etc.),
but not to just a procedure. Instead, for the mapping to serve as a
Rhombus expander, the compile-time value must instantiate a structure
type or implement a suitable structure-type property that is provided
by the Rhombus expander.

In all expansion contexts, a macro transformer receives all remaining
terms in the enclosing group after the triggering identifier or
operator. A declaration or definition transformer is obliged to use
all of the terms. In an expression or binding context, the transformer
consumes as many as it wants, and it returns the remaining terms
alongside its expansion result.

### API

The Rhombus expander checks an identifier's mapping for a transformer
when the identifier is at the start of a group in `rhombus-top`,
`rhombus-block` `:declaration`, or `:definition`, or when it starts an
expression or binding in other contexts.

The structure types provided by the Rhombus expander to implement
transformers:

 * `(rhombus-declaration-transformer proc)`: `proc` is
    `((listof syntax?) . -> . stx-list?)`
    where the result list contains Racket declarations, definitions,
    and expressions.
 
 * `(rhombus-definition-transformer proc)`: `proc` is
    `((listof syntax?) . -> . (values stx-list? stx-list?))`
    where the first result list has Racket definitions and
    expressions, and the second result list has Racket expressions.
    The intent of the second result is to expose whether any expressions
    are returned at the end, making the form suitable as the last
    position within a local binding context.
 
 * `(rhombus-expression-transformer proc)`: `proc` is
    `((listof syntax?) . -> . (values syntax? stx-list?))`
    where the first result is a Racket expression, and the last
    result is a list of remaining terms (intended to be a tail of the
    input list of terms).
 
 * `(rhombus-binding-transformer proc)`: `proc` is
    `((listof syntax?) . -> . (values (listof identifier?) syntax? (listof identifier?) syntax? stx-list?))`
    where the first result is a list of identifiers to bind as
    variables, the second result is a Racket expression for a matcher
    function, the third result is a list of identifiers to bind as
    syntax (possibly referring to other identifiers bound as variables
    or syntax), the fourth result is a compile-time expression to
    supply the values to bind as syntax, and the last result is a list
    of remaining terms (intended to be a tail of the input list of
    terms). A matcher procedure takes one argument, and it returns one
    plus _N_ results: the first result is a boolean indicating whether
    the binding succeeds, which is useful for pattern-matching
    bindings, and the remaining _N_ result supply values for the _N_
    variables returned by the transformer.

For each of these structure types, the Rhombus expander also provides
a structure-type property and a predicate to recognize instances of
the structure type and property. The property and predicate have the
the obvious names (add `prop:` for the property and `?` for the
predicate). The property value must be an accessor procedure that
takes the value (the same one with the property) and returns an
instance of the corresponding structure type.

Again, a Rhombus program that defines new transformers will use forms
provide by Rhombus that ultimately make use of these Racket entities.

## Operators and operator transformers

Shrubbery notation distinguishes identifiers and operators, but
operators are bound at the Racket level in identifier form (i.e., the
identifier that has the same characters as the operator). Operator
mappings come in two flavors: plain operators, where the argument
expression(s) of the operator are delivered to a converted in parsed
form, or operator transformers, where the unparsed stream of terms
after the operators is delivered to the transformer procedure. For an
infix operator transformer, the operator's left argument is delivered
in parsed form, the same as for a prefix operator.

Since operators can be infix or prefix, plain or transformer, and
expression or binding, the Rhombus expander provides eight
compile-time constructors to operator representations. They are listed
further below.

Each constructor takes three or four arguments, where the prefix
variants take three arguments and the infix variants take four
argument. The first argument is always an operator name as an
identifier, which will be used for precedence comparisons; this
identifier is normally the same one that is bound to the operator. The
second argument is always precedence information, to be described
below. For infix operators, the third argument indicates
associativity. The last argument is a converter or transformer
procedure.

### API

The next section explains the representation of precedence and
associativity. The expression operator constructors are as follows:

 * `(rhombus-prefix-operator id prec proc)`: `proc` is
   `(syntax? syntax? . -> . syntax?)`
   where the first argument is a Racket expression as the operator's
   argument, the second argument is an identifier for the operator
   (potentially useful for its source location), and the result
   is a Racket expression.

 * `(rhombus-infix-operator id prec assc proc)`: `proc` is
   `(syntax? syntax? syntax? . -> . syntax?)`
   where the first argument is a Racket expression as the operator's
   left argument, the second argument is a Racket expression as the
   operator's right argument, the third argument is an identifier for
   the operator (potentially useful for its source location), and the
   result is a Racket expression.

 * `(rhombus-prefix-operator-transformer id prec proc)`: `proc` is
   `((listof syntax?) . -> . (values syntax? stx-list?))`
   where the argument is a list of terms within a group starting with
   the operator, the first result is a Racket expression, and the last
   result is a list of remaining terms (intended to be a tail of the
   input list of terms). Note that this protocol is the same as for
   `rhombus-expression-transformer`.

 * `(rhombus-prefix-operator-transformer id prec assc proc)`: `proc` is
   `(syntax? (listof syntax?) . -> . (values syntax? stx-list?))`
   where the first argument is a Racket expression for the operator's
   left argument, and the remaining arguments and results are the same
   as for `rhombus-prefix-operator-transformer`.

Four similar constructors are provided for binding operators. In each
case, the arguments are the same as for expression operators, and the
results are the same as for `rhombus-binding-transformer`.

 * `(rhombus-prefix-binding-operator id prec proc)`

 * `(rhombus-infix-binding-operator id prec assc proc)`

 * `(rhombus-prefix-binding-operator-transformer id prec proc)`

 * `(rhombus-infix-binding-operator-transformer id prec assc proc)`

For operator transformers, the `:op+expression+tail` and
`:op+binding+tail` syntax classes are potentially useful for parsing
the argument after an operator. Any plain operator can be defined as
an operator transformer using those syntax classes.


## Operator precedence and associativity

Instead of attaching a numeric precedence to each operator, as in
Honu, each operator declares its precedence relative to other
operators. That is, an operator can declare that some other operators
have weaker precedence, that some other operators have stronger
precedence, and that some operators that have the same precedence
(which implicitly includes the operator itself). An infix operator's
associativity is relevant only for operators at the same precedence
(including the operator itself), as either left-associative,
right-associative, or non-associative.

Unlike a system based on numeric precedence levels, precedence in
Rhombus is not a complete or partial order. It's not even transitive.
Operators A and B have a precedence relationship only if either A
indicates a relationship to B or B indicates a relationship to A. This
approach turns out to work fine with a Pratt-style parser (as used in
Honu), which only ever needs to work with precedence when it has two
specific operators to compare. A potential advantage of non-transitive
precedence is avoiding an order among operands that make no sense
next to each other. An operator can declare a default precedence
relationship to other operators, but must declare the default
explicitly.

If two operators both claim a precedence relationship to each other,
the relationship must be consistent; for example, A cannot claim to
have stronger precedence than B if B claims stronger precedence than
A. Also, if one or both operators claim a same precedence strength to
the other, the operators must have the same associativity. The
consistency of precedence claims between A and B is checked only at
the point where A and B are compared by the enforesting expander,
and inconsistent claims trigger a syntax error at that point.

Precedence is relevant for both infix (binary) and prefix (unary)
operators, but only infix operators have an associativity. When an
infix operator is involved in a precedence comparison, the other
operator is always infix (appearing syntactically after the prefix
operator).

### API

A precedence is represented as a list of pairs. The `car` of each pair
is either an identifier or `'default`, where `'default` refers to any
operator not otherwise mentioned in the list and not the same as the
describing operator itself. The `cdr` of each pair is either
`'stronger`, `'same`, or `'weaker`, meaning that the describing
operator has a stronger precedence, the same precedence, or a weaker
precedence than the operator bound to the identifier in the pair's
`car`.

An associativity is represented as either `'left`, `'right`, or `#f`.

## Implicit operators

In much the same way that `#%app` and `#%datum` are implicitly used in
many Racket expressions, Rhombus expressions and bindings involve a
number of implicit operators:

 * `#%call`: implicit infix for an expression followed by a parenthesized
   term

 * `#%tuple`: implicit prefix for a parenthesized term that is not
   immediately after an expression

 * `#%ref`: implicit infix for an expression followed by a
   square-bracketed term

 * `#%array`: implicit prefix for a square-bracketed term that is not
   immediately after an expression

 * `#%juxtapose`: implicit infix for adjacent expressions with no
   operator between them when `#%call` and `#%ref` do not apply

 * `#%block`: implicit prefix for a block (written in curly braces or
   indentation) in an expression position

 * `#%alts`: implicit prefix for a block of alternatives (written with
   bar notation) in an expression position

 * `#%literal`: implicit prefix for a literal, such as a number o
   boolean

A language's `#%call` implementation most likely creates a function
call, `#%tuple` most likely does nothing for a single expression in
parentheses (so parentheses can be used for grouping) and might
otherwise create a tuple value, `#%juxtapose` probably reports an
error, and `#%block` probably creates a nested definition context.
Implicit operators are likely to have the higher possible precedence
and be left-associative, but they are not constrained to those
conventions by the Rhombus expander.

Note that the names of implicit operators are not shrubbery operator
tokens. Mapping works in terms of Racket identifiers, whether for
shrubbery identifiers for operators, so there's no inherent problem
with using these “operator” names for mapping. Unlike explicitly
referring to `#%app` in Racket, however, it is impossible for a
program in shrubbery notation to explicitly refer to an implicit
operator.

### API

Normally, mappings for implicit operators need to be operator
transformers, and not plain operators. When an operator transformer's
procedure receives a list of terms, there will be no representative to
the implicit operator itself. For example, the first term in the list
for a `#%call` or `#%tuple` transformer will be a `'parens` term.


## Enforestation algorithm

The Rhombus parsing algorithm is similar to figure 1 of [the Honu
paper](http://www.cs.utah.edu/plt/publications/gpce12-rf.pdf), but
with some key differences:

 * A prefix or infix operator has an associated converter procedure to
   produce a Racket expression form, instead of always making a `bin`
   or `un` AST node.
  
 * A prefix or infix operator can be bound to a transformer, in which
   case all parsing for the (second) argument is up to the
   transformer. A prefix operator as a transformer acts just like a
   transformer bound to an identifier, and an infix operator as a
   transformer is analogous.
    
 * Function calls, array references, and list construction are not
   quite built-in. Instead, those positions correspond to the use of
   implicit operators, such as `#%call`.
   
 * The paper's prefix-operator case seems wrong; the old operator and
   combiner should be pushed onto the stack.

The implementation represents pending operators during enforestation
through the Racket continuation instead of an explicit stack; that is,
instead of pushing on the stack, a recursive call is made to the
enforestation function. A transformer can call back into the
enforestation function, and it provides a current operator for
precedence purposes, and the enforestation function will stop when it
encounters either the end of the input or an operator at lower
precedence; for the latter case, the enforestation function returns
the remaining terms.


### API

The enforestation algorithm is built into `rhombus-expression`,
`rhombus-binding`, `:expression`, `:binding`, `:op+expression+tail`,
and `:op+binding+tail`. The `:op+expression+tail` and
`:op+binding+tail` syntax classes effectively calls the enforestation
algorithm with a current operator, while the others start
enforestation without a current operator.


# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

See [parse.rkt](rhombus/parse.rkt) and associated modules. The
proposal include a sketch of `#lang rhombus` that defines only a
handful of operators, only a basic definition form, and only some of
the implement forms, but it illustrates how things fit together.

This `#lang rhombus` implementation is currently also available as a
package: [https://github.com/mflatt/shrubbery-rhombus-0](https://github.com/mflatt/shrubbery-rhombus-0).
The package is likely to evolve more quickly than the `#lang sketch`
that is part of the proposal.

# Drawbacks
[drawbacks]: #drawbacks

Much like shrubbery notation itself, the Rhombus expander tries to be
general, but it embeds a certain amount of opinion on syntax and
program organization. Function calls do not have to be written with
parentheses around arguments (for example, `#%juxtapose` is available
for implementing ML-style function calls), but having `#%call`
available encourages that style. Along similar lines, the Rhombus
expander supports a certain style of infix and prefix operators, but
it does not directly support all possible kinds of operators; for
example, a backward `.` that puts the field first will not work well,
since the left-hand side of an operator is always parsed as an
expression. The combination of opinion and choice provided by the
Rhombus expander leads to a large set of implicit operators.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

The four syntactic categories to be directly supported—declarations,
definitions, expressions, and binding—reflect experience with Racket.
The first three have been a successful baseline in Racket, while the
lack of built-in extensibility for binding positions for `lambda` and
`let` has limited the use of pattern matching. A natural fifth
category would be types, and that category is omitted here with the
expectation that an base Rhombus language will not be statically
typed. Other possibile categories include method and field positions;
those, again, are meant to be left to libraries.

The choice to constrain operator dispatch to expression positions is
intended as a simplification. Operators could be straightforwardly
allowed to have prefix declaration- and definition-transformer
mappings, and some form of infix operators could be supported. One
possible use of `::` as a declaration infix operator would be for type
declarations separate from definitions:

```
f :: Integer -> Integer
define f(x):
  x+1
```

Since declarations and definitions do not naturally nest in the same
way as expressions, however, the way infix operators should work at
that level is not as clear as it is for the expression level. A
language with types that supports the above notation would likely need
a multi-pass, partial-expansion transversal of a definition context to
gather and relate definitions and type declarations, and that seems
better left to a library or extension than the Rhombus expander core.

For opinions on non-transitive operator precedence, see
[Jeff Walker's blog post](https://blog.adamant-lang.org/2019/operator-precedence/).

Operator transformers could implement all operators, with full
precedence handling, but supporting non-transformer operators is
convenient and arguably clarifying in the algorithm.

# Prior art
[prior-art]: #prior-art

The Rhombus enforestation and expansion algorithm is directly based on
[Honu: Syntactic Extension for Algebraic Notation through
Enforestation](http://www.cs.utah.edu/plt/publications/gpce12-rf.pdf).

The [Fortress](http://www.ccs.neu.edu/home/samth/fortress-spec.pdf)
language has non-transitive operator precedence.

# Unresolved questions
[unresolved-questions]: #unresolved-questions

# Future possibilities
[future-possibilities]: #future-possibilities

This proposal still leaves open exactly the way that the notation
would be used to express a new programming language. The example is
meant to be suggestive, however, and similar examples influenced the
design choices.
