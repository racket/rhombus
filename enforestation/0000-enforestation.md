- Feature Name: Enforestation and expansion for shrubberies
- Start Date: 2021-07-14
- RFC PR: [racket/rhombus-brainstorming#162](https://github.com/racket/rhombus-brainstorming/pull/162)

# Summary
[summary]: #summary

[Shrubbery notation](https://github.com/mflatt/rhombus-brainstorming/blob/shrubbery/shrubbery/0000-shrubbery.md)
specifies how to parse a sequence of characters into a coarse-grained
block structure, but it leaves the interpretation of that block
structure to another layer of parsing—not to mention more fine-grained
grouping in between lexing and block structure. This proposal describes
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
#lang rhombus

import:
  weather

struct Posn(x, y)  

val home :: Posn:
  Posn(3, 7)

fun abs(n): if n < 0 | -n | n

fun manhattan_distance(p :: Posn):
  abs(home.x - p.x) + 5 * abs(home.y - p.y)

fun another_manhattan_distance(Posn(x, y)):
  abs(home.x - x) + 5 * abs(home.y - y)

fun
 | should_take_cab(Posn(0, 0)): #false
 | should_take_cab(p :: Posn):
     manhattan_distance(p) > 10 || weather.currently_raining()
 | should_take_cab(#false): #false
```

The intent here is that `val` and `fun` are macro-implemented and
recognize various forms of definitions, including simple binding,
functions, and functions that have pattern-matching cases. The `val`
and `function` forms are not meant to know about `::` specifically;
the `::` is meant to be a binding operator that checks whether the
value flowing to the binding satisfies a predicate, and it may also
associate compile-time information to a binding, such as the
information that `p` is a `Posn` instance. The name `Posn` works in an
expression to construct a position value, while in a binding position,
`Posn` works to construct a pattern-matching binding (again, without
`val` or `fun` knowing anything specific about `Posn`). Meanwhile, `.`, `-`,
`+`, `*`, `<`, and `||` are the obvious operators with the usual
precedence. Unlike the other operators, the `.` operator's right-hand
side is not an expression; it must always be an identifier. The
`weather.currently_raining` form looks like a use of the `.` operator,
but it's meant here to be a use of the namer `weather` that is bound by
`import` and recognizes `.` to access the imported `currently_raining`
binding, which might be a macro instead of a variable that is bound to
a function.

The Rhombus expander is further meant to a support a language where
new operators can be defined in a function-like way, like this:

```
operator (x <> y):
  Posn(x, y)

1 <> 2 // same as Posn(1, 2)
```

Or operators can be defined in a more general, macro-like way. For
example, defining `->` as an alias for `.` requires a macro, since the
right-hand side of `.` is not an expression. Using `?` for “quote” and
`¿` for “unquote,” the `->` operator might be implemented in a
pattern-matching macro as

```
expr.macro ?(¿x -> ¿y ¿tail ...):
  values(?(¿x . ¿y), tail)

home->x + 1 // same as home.x + 1
```

The intent here is that `expr.macro` (the `.` there is like using an
import, accessing the `macro` form within an `expr` group of bindings)
allows a macro to consume as many terms after the operator as it
wants, and the macro must return two values: a quoted expression for
the expansion plus leftover terms for further expression parsing
(i.e., `tail` in the example use will hold `+ 1`). Macros can work for
other contexts, too, such as binding positions. Here's a definition
that extends the `<>` operator to make it work in binding positions:

```
bind.macro ?(¿x <> ¿y ¿tail ...):
  values(?(Posn(¿x, ¿y)), tail)

// uses <> both for argument binding and result expression:
fun flip(x <> y):
  y <> x

flip(1 <> 2) // produces 2 <> 1
```

The above examples run in a prototype `#lang rhombus` that is included
with this proposal, but the examples and prototype are meant only to
illustrate some of the ingredients that the expander supports. This
proposal does not intended to specify a language with specific
definition forms, expression forms, and so on.

To define a new expander layer for shrubbery notation, we take
Racket's primitives as given: syntax objects, scopes, modules, and
more. We also recycle some variant of Racket as the target for
shrubbery expansion. In principle, the variant could be minimal,
corresponding to the core forms that all Racket modules expand into,
but some larger variant (including keyword arguments, for example) is
likely a better choice of interoperability with Racket modules. The
enforestation and expansion process here are defined in terms of the
S-expression form of parsed shrubbery notation (really, syntax-object
form, so it can include scopes to determine a mapping for identifiers
and operators). The `<>` and `->` examples above use operator- and
macro-definition forms in terms of shrubbery notation, but this
proposal focused on the lower-level mechanisms that allow such
shrubbery-native forms to be implemented.

Since shrubbery notation is intended to be used with less grouping
than is normally present in S-expressions, a key goal of this proposal
is to specify an extensible traversal of shrubberies to turn them into
Racket terms. In particular, the proposal addresses the problem of
parsing a mixture of operators and other terms within a shrubbery
group, allowing the definition of infix, prefix, and postfix operators
for expressions, bindings, and other contexts. The handling of
operators is directly based on Honu, but with several
refinements—including a framework for relative and non-transitive
operator precedence that is more like Fortress than Honu.

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

## Syntactic categories

Rhombus expansion involves various syntactic categories that determine
different kinds of expansion contexts. The specific set of contexts
depends on the language, and not the Rhombus expander, but here are
some possible contexts:

 * declarations (in a module's immediate body or at the top level)

 * definitions

 * expressions

 * bindings (like `match` patterns, but everywhere)

In Racket's expander, a few core contexts are reflected by
`syntax-local-context`, but the Racket expander has only one kind of
transformer that is represented by one kind of compile-time value: a
procedure of arity 1. Nevertheless, some macros work only in, say,
definition positions or in a module body. Rhombus expansion instead
expects different kinds of compile-time values for different expansion
contexts, so a mapping can declare where it's meant to be used.

The Rhombus expander is parameterized over the way that different
kinds of compile-time values for different contexts are recognized,
but they are expected to be implemented through structure-type
properties. A compile-time value can then implement multiple kinds of
transformers to create a mapping that is works in multiple contexts.
For example, the example `<>` operator is useful in both expression
and binding contexts, with a suitable meaning in each context.

Different contexts may also consult different mapping spaces in the
sense of `(provide (for-space ....))`. Contexts like declarations,
definitions, and expressions are likely to use the default space,
while binding, require, and provide contexts might use their own
spaces. For example, in the prototype language supplied with this
proposal, `operator` and `bind.macro` can both bind `<>` because
the former binds in the default space and the latter in the binding
space. The Rhombus expander itself is, again, parameterized over the
way that mapping spaces are used.

The relevant syntactic category for a shrubbery is determined by its
surrounding forms, and not inherent to the shrubbery. For example,
`Posn(x, y)` or `x <> y` in the example mean one thing as an
expression and another as a binding. Exactly where the contexts reside
in a module depends on a specific Rhombus language that is built on
the Rhombus expander. Meanwhile, a full Rhombus language can have
different or more syntactic categories than the ones listed above. A
Rhombus language likely allows extensions to create even more
contexts, just like Racket program can have more contexts through
syntactic extensions, such as `match` or Typed Racket.

## Hierarchical naming

A language implemented with the Rhombus expander may have another
dimension of name resolution that is orthogonal to different mapping
spaces. For example, a language include a hierarchical naming strategy
to reach a binding through sequence of identifiers separated by `.`,
and hierarchical references might be used to reach mappings for
expressions, bindings, or more. In the initial example in this
proposal `weather.currently_raining` is that kind of access, as is
`expr.macro` and `bind.macro`.

The example language “overloads” `.` for hierarchical namespace use as
well as field access, but the Rhombus expander minimizes any
assumptions about the form of hierarchical names. A hierarchical
reference must start with an identifier or operator that is mapped in
the default space to a _name root_, and that identifier must be
followed with the use of a designated name-path operator. A name root
is implemented by a transformer that is similar to a prefix macro
transformer (as explained in the next section), but it ”expands” to a
new identifier whose binding is checked in a space that's suitable to
the context—or it expands to a reference to another name root, in
which case the new root is expanded recursively).

## Operator and macro transformers

Some contexts in a Rhombus language (likely including expression
contexts) will support infix, prefix, and postfix operators. The
Rhombus expander provides an _enforestation_ framework for parsing
forms that involve multiple operators, each with a declared precedence
and associativity. The enforestation process also allows an operator
transformer to completely take over parsing of terms that follow the
operator within a shrubbery group. An “operator” in this sense can be
named by either a shrubbery operator or a shrubbery identifier, so the
prefix-operator protocol suffices for defining macros in a traditional
sense.

For an infix operator, enforestation always parses the left-hand
argument (i.e., the part before the operator) in the same context as
the operator's context. For example, the left-hand argument to an
infix expression operator `+` or `.` is always parsed as an
expression. For the right-hand (or only, in the case of prefix)
argument, the operator's mapping selects one of two protocols:
_automatic_, where the right-hand argument is also parsed in the same
context, or _macro_, where the operator's transformer receives the
full sequence of terms remaining in the enclosing group. An operator
using the macro protocol parses remaining terms as it sees fit, and
then it returns the still-remaining terms that it does not consume.
For example, `+` for expressions is likely implemented as an automatic
infix operator, since both of its arguments are also expressions,
while `.` is likely implemented as a macro infix operator so that it's
right-hand “argument” is always parsed as a field identifier. In the
earlier `<>` and `->` examples, `<>` is implemented as an automatic
infix operator for expressions, while `<>` for bindings and `->` for
expressions were implemented as macro infix operators.

Roughly, an operator that uses the macro protocol takes on some of the
burden of dealing with precedence, at least for terms after the
operator. For operators like `.` or `->`, this is no problem, because
the right-hand side has a fixed shape. Other operators may need to
call back into the enforestation algorithm, and the Rhombus expander
provides facilities to enable that.

A postfix operator is implemented as a macro infix operator that
consumes no additional terms after the operator. For example, a
postfix `!` might be defined (shadowing the normal `!` for “not”) as
follows:

```
fun
 | factorial(0): 1
 | factorial(n): n*factorial(n-1)
         
expr.macro ?(¿a ! ¿tail ...):
  values(?(factorial(¿a)), tail)

10! + 1 // = 3628801
```

Since the Rhombus expander provides a way for macro transformers to
resume enforestation, all operators could be implemented with the
macro protocol. The automatic protocol is just a convenient shortcut.

Some contexts might constrain the allowed forms of operators to prefix
or infix, constrain the names used for operators, and/or eschew one of
the operator protocols. For example, declaration and definition
contexts might allow only macro prefix operators with identifier
names. The provided `#lang rhombus` prototype makes that choice, and
it also allows expression forms with operators to appear in the same
places as declaration and definition forms.

## Operator precedence and associativity

Instead of attaching a numeric precedence to each operator, as in
Honu, each operator declares its precedence relative to other
operators. That is, an operator can declare that its precedence is
stronger than certain other operators, weaker than certain other
operators, the same as certain other operators (implicitly
including the operator itself), and the same as certain other operators
when in a specific order (e.g., `*` is not allowed to the right of `/`).
An infix operator's associativity is
relevant only for operators at the same precedence (including the
operator itself), as either left-associative, right-associative, or
non-associative.

Our example `<>` definition did not specify any precedence
relationships, so it cannot be used next to `*`:

```
1 <> 2 * 3 // not allowed
```

In this example, enforestation would report that `<>` and `*` are
unrelated, so parentheses are needed somewhere. The prototype `#lang
rhombus` supports precedence declarations through a `weaker_than`
keyword (where the use of `'` here imitates the keyword syntax that is
used in the prototype for function definitions and calls):

```
operator (x <> y,
          'weaker_than': * / + -):
  Posn(x, y)

1 <> 2 * 3 // same as Posn(1, 6)
```

Unlike a system based on numeric precedence levels, precedence in the
Rhombus expander is not a complete or partial order. It's not even
transitive. Operators A and B have a precedence relationship only if
either A indicates a relationship to B or B indicates a relationship
to A. This approach turns out to work fine with a Pratt-style parser
(as used in Honu), which only ever needs to work with precedence when
it has two specific operators to compare. A potential advantage of
non-transitive precedence is avoiding an order among operands that
make no sense next to each other. An operator can declare a default
precedence relationship to other operators, but it must declare the
default explicitly.

If two operators both claim a precedence relationship to each other,
the relationship must be consistent; for example, A cannot claim to
have stronger precedence than B if B claims stronger precedence than
A. Also, if one or both operators claim a same precedence strength to
the other, the operators must have the same associativity. The
consistency of precedence claims between A and B is checked only at
the point where A and B are compared by the enforesting expander,
and inconsistent claims trigger a syntax error at that point.

Procedurally, precedence is relevant when enforestation encounters an
infix operator on the right-hand side of some other operator.
Precedence determines whether the terms in between the two operators
form an argument to the earlier operator (on the way to producing an
argument for the later operator) or whether those terms form an
argument to the newly encountered operator (on the way to producing an
argument for the earlier operator). Note that when a prefix operator
is involved in a precedence comparison, the other operator is always a
later infix operator.

## Implicit operators

In much the same way that `#%app` and `#%datum` are implicitly used in
many Racket expressions, Rhombus enforestation consults a number of
implicit operators:

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

In an expression context, a Rhombus language's `#%call` implementation
most likely creates a function call, `#%tuple` most likely does
nothing for a single expression in parentheses (so parentheses can be
used for grouping) and might otherwise create a tuple value or return
multiple values, `#%juxtapose` probably reports an error, and
`#%block` probably creates a nested definition context. Implicit
operators are likely to have the highest possible precedence and be
left-associative, but they are not constrained to those conventions by
the Rhombus expander. Implicit operators are likely to be implemented
using the macro operator protocol instead of the automatic operator
protocol.

## Macro protocol

When an operator's mapping indicates a macro protocol, then the
operator's transformer procedure is called with a parsed left-hand
argument (in the case of an infix operator) and all of the remaining
terms in the operator's group starting with the operator. The
transformer procedure must return two values: the parsed form, which
normally incorporates the left-hand argument plus some consumed
additional terms, and the remaining terms that were not consumed. The
Rhombus expander does not check that the second result is a tail of
the original remaining terms, so a transformer could replace or
rearrange them, but that's probably not a good idea. The invocation of
a transformer for an implicit operator includes a synthesized term for
the implicit operator.

The enforestation process recognizes a superset of S-expressions
compared to those used to represent shrubberies. Specifically, it
recognizes a two-element list term that starts with the symbol
`'parsed`. The S-expression after `'parsed` is treated as an
already-parsed term and need not conform to the shrubbery grammar's
representation. Since the list starting `'parsed` itself has no
shrubbery representation, it's conveniently opaque to a shrubbery
layer of patterning matching. For example, in the earlier example
implementing the `->` macro infix operator,

```
expr.macro ?(¿x -> ¿y ¿tail ...):
  values(?(¿x . ¿y), tail)
```

the macro transformer receives an syntax-object representing the
already-parsed left-hand argument `x` as a `'parsed` list. The macro
therefore has no way to pull the expression apart, inspect it, or
rearrange it. Of course, such facilities could be made available to
the macro transformer in lower-level form. Meanwhile, `y` and `tail`
are likely unparsed terms, that can be inspected—although it's
possible that some other macro constructs a `->` expression using
already-parsed terms, in which case they are similarly opaque to the
`->` transformer.

For binding-operator macros, the prototype `#lang rhombus` supplied
with this proposal includes `unpack_binding` to expose certain pieces
of a binding's implementation, which allows the macro to compose or
adjust other binding expansions. New binding pieces can be put back
together into a parsed form using `pack_binding`.

Some contexts may oblige a macro transformer to consume all of the
remaining terms in a group. For example, a definition or declaration
context based on prefix identifiers like `import`, `val`, `fun`, and
`struct` might report an error if a transformer does not consume all
available terms (and that's the case in the prototype `#lang
rhombus`).

## Enforestation algorithm

The Rhombus parsing algorithm is similar to figure 1 of [the Honu
paper](http://www.cs.utah.edu/plt/publications/gpce12-rf.pdf), but
with some key differences:

 * When the inital term is an identifier followed by a name-path
   operator and `lookup` produces a name root for the identifier, the
   name-root transformer is applied. Its result, a new term and tail,
   are used for the new state, without changing the current operator
   (if any).

 * A prefix or infix operator has an associated transformer procedure
   to produce a Racket form, instead of always making a `bin` or `un`
   AST node. Whether the Racket form is an expression or some other
   form (such as pieces of a binding) depends on the context.
  
 * Operators using the _automatic_ protocol are dispatched as in the
   figure. If a prefix operator's protocol is _macro_, it behaves the
   same as the figure's case of an identifier that is mapped to a
   transformer. A macro infix operator's treatment is analogous.

 * Function calls, array references, and list construction are not
   built-in in the same way as Honu. Instead, those positions
   correspond to the use of implicit operators, such as `#%call`.
   
 * The figure's prefix-operator case seems wrong; the old operator and
   combiner should be pushed onto the stack.

 * Already-parsed forms that are encoded with `'parsed` (which are
   “term”s in the figure's terminology) are immediately converted to
   parsed form (i.e., “tree term”s in the figure) by removing the
   `'parsed` wrapper.

The implementation represents pending operators during enforestation
through the Racket continuation, instead of using an explicit stack;
that is, instead of pushing on the stack, a recursive call is made to
the enforestation function. A transformer can call back into the
enforestation function, and in that case, it provides a current
operator for precedence purposes, and the enforestation function will
stop when it encounters either the end of the input or an operator at
lower precedence. When enforestation stops at an operator with lower
precedence, the enforestation function returns both the parsed form
and the remaining terms.

An identifier/operator is connected to a transformer using Racket's
mapping machinery (`define-syntax`, etc.). The enforestation algorithm
is parameterized over the space (in the sense of `for-space`) it
should consult and accessor–predicate functions that extract infix and
prefix transformers from compile-time bindings.

When a context includes only prefix macro operators that are
constrained to consume all available terms, then enforestation is not
really relevant. In that case, the context just needs a way to find
and invoke operator transformers. The Rhombus expander provides a
shortcut that skips the full enforestation algorithm for that simpler
kind of context.

## API

The Rhombus expander's primary API consists of three Racket structure
types and one macro:

 * A `name-root` structure with one field:

      - `proc`: a transformer procedure, which takes a syntactic list
        of tokens and returns two values: a new head identifier or
        operator token, and a new tail syntactic list of tokens.

   A `prop:name-root` structure type property is also provided, and
   `name-root?` refers to implementations of `prop:name-root`. The
   property value must be a function that takes a structure
   implementing the property and returns a `name-root` instance. The
   `name-root` structure implements `prop:name-root` with a function that
   results the structure instance itself.

 * An `operator` structure type with `infix-operator` and
   `prefix-operator` structure subtypes. An `operator` structure has

      - `name`: an identifier syntax object

      - `precs`: an association list from identifiers
        to `'stronger`, `'weaker`, `'same`, `'same-on-left`, or
        `'same-on-right`, indicating that the
        `operator` instance's precedence is stronger than, weaker
        than, or the same as (when on the indicated side of) the
        associated identifier; `'default` can
        be used instead of an identifier to specify a default
        relationship

      - `protocol`: `'macro` or `'automatic`

      - `proc`: a transformer procedure, where the arguments and
        results depend on whether the operator is prefix or infix,
        macro or automatic

   A `prefix-operator` has no additional fields.

   An `infix-operator` has one additional field:

      - `assoc`: `'left`, `'right`, or `'none`

   These structure types are provided by the `enforest/operator`
   library.

 * A `define-enforest` macro that parameterizes the enforestation
   algorithm over the following:

    - `enforest`: the name to bind as an `enforest` function, which is
      used to start enforestation of a group. It takes a list of
      S-expressions for parsed shrubberies that were in a group, and
      it returns a parsed form. The `enforest` function drives a loop
      that calls the enforest-step function.

    - `enforest-step`: the name to bind as an enforest-step function,
      with continues an enforestation. It takes two arguments, which
      is the list of renaming terms in a group and the current
      operator. The result is two values: a parsed form and the
      remaining sequence of terms (starting with an infix operator
      that has lower precedence than the input operator).

    - `:form`: the name of a syntax class to bind (see
      `syntax-parse`), which matches a `group` shrubbery
      representation and enforests it. A match has an `parsed`
      attribute representing the enforestation result.

    - `:prefix-op+form+tail` and `:infix-op+form+tail`: names of
      splicing syntax classes that match an operator name followed by
      a sequence of terms and steps an enforestation. A match has an
      `parsed` attribute for the parsed result and a `tail` attribute
      for the remaining terms.

    - `form-kind-str` and `operator-kind-str`: string used in error
      reporting.

    - `in-space`: a function that takes an identifier syntax object
      and adds a space scope if the enforesting context uses a space.

    - `name-path-op`: an operator name that is recognized after a
      name-root identifier for hierarhical name references.

    - `prefix-operator-ref` and `infix-operator-ref`: functions that
      take a compile-time value and extract an instance of
      `prefix-operator` or `infix-operator`, respectively, if the
      value has one suitable for the context, returning `#f`
      otherwise. Normally, these functions use structure-property
      accessors.

    - `check-result`: a function that takes the result of an operator
       and checks whether the result is suitable for the context, used
       for earlier detection of errors; the `check-result` function
       should either raise an exception or return its argument
       (possibly adjusted).

    - `make-identifier-form`: a function that takes an identifier an
      produces a suitable parsed form. If a context does not have a
      meaning for unbound identifiers, `make-identifier-form` can
      report a syntax error.

   The `define-enforest` macro is provided by the `enforest` library.

To support simple contexts that have only prefix transformers and name
roots, the Rhombus expander API provides an additional structure type
and macro:

 * A `transformer` structure type with one field:

    - `proc`: a procedure that takes a list of terms and returns a
      parsed term

 * A `define-transform-class` macro that defines a syntax class to
   trigger parsing, given the following:

    - `:form`: the name of the syntax class to define, which matches a
       `group` shrubbery representation and parses it. A match has
       an `parsed` attribute representing the parsed result.

    - `form-kind-str`: string used in error reporting.

    - `name-path-op`: an operator name that is recognized after a
      name-root identifier for hierarhical name references.

    - `transformer-ref`: function that takes a compile-time value and
      extract an instance of `transformer`, if the value has one
      suitable for the context, returning `#f` otherwise. Normally,
      these functions use structure-property accessors.

    - `check-result`: a function that takes the result of an
       transformer and checks whether the result is suitable for the
       context, used for earlier detection of errors; the
       `check-result` function should either raise an exception or
       return its argument (possibly adjusted).

## Prototype implementation examples

The prototype `#lang rhombus` implementation starts with a
`#%module-begin` form that takes a shrubbery sequence wrapped with
`top` as its input. Simplifying somewhat, the implementation uses a
`rhombus-top` helper macro:

```
(define-syntax (rhombus-module-begin stx)
  (syntax-case stx ()
    [(_ (top . content))
     #`(#%module-begin
        (rhombus-top . content))]))
```

The `rhombus-top` macro tries to parse each top-level form as either a
declaration, definition, or expression. Parsing a declaration or
definition produces sequence of terms as the `parsed` attribute, while
parsing an expression produces a single expression as `parsed`:

```
(define-syntax (rhombus-top stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ form . forms)
     #`(begin
         #,(syntax-parse #'form
             [e::declaration #'(begin . e.parsed)]
             [e::definition #'(begin . e.parsed)]
             [e::expression #'(#%expression e.parsed)])
         (rhombus-top . forms))]))
```

This `rhombus-top` macro uses a typical trampolining pattern: the Racket
macro expander will perform any declaration or definition bindings
before expanding the recursive use of `rhombus-top`. which will then
force more declaration, definition, and expression parsing. That way,
Rhombus-level operators can be defined and then used in the same
module.

The `:definition` syntax class is defined using the simplified Rhombus
expander API:

```
  (define-transform-class :definition
    "definition"
    definition-transformer-ref
    check-definition-result)
```

Here, `definition-transformer-ref` refers to a function that extracts
a `transformer` structure from a compile-time value (returning `#f` if
no such structure is available). The `check-definition-result` function
makes sure that the low-level transformer returns at least a
list-shaped syntax object, but that's just for earlier error
detection.

A simple implementation of the `val` definition form could be like
this:

```
(define-syntax val
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (block)
        ;; match `val <binding>: <form> ... `, where
        ;; `:` grouping is represented by `block`
        [(_ b::binding (block rhs ...))
         (build-values-definitions #'b.parsed
                                   #'(rhombus-block rhs ...))]))))
```

Parsing `b` as `:binding` produces a `parsed` attribute that embeds
the identifiers to bind as well as predicates and conversion to apply
to the result of the right-hand side. The right-hand side is put into
a `rhombus-block` form, which bounces back to (local) definition and
expression parsing, roughly like this:

```
(define-syntax (rhombus-block stx)
  (syntax-parse stx
    [(_ . tail) #`(let () (rhombus-body . tail))]))

(define-syntax (rhombus-body stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ e::definition . tail)
     #`(begin
         (begin . e.parsed)
         (rhombus-body . tail))]
    [(_ e::expression . tail)
     #`(begin
         (#%expression e.parsed)
         (rhombus-body . tail))]))
```

Here's the definition of the `:expression` syntax class:

```
  (define-enforest enforest-expression enforest-expression-step
    :expression :prefix-op+expression+tail :infix-op+expression+tail
    "expression" "expression operator"
    in-expression-space
    '|.| expression-prefix-operator-ref expression-infix-operator-ref
    check-expression-result
    make-identifier-expression)
```

Expressions use the default mapping space, so `in-expression-space` is
just the identity function. The `expression-prefix-operator-ref` and
`expression-infix-operator-ref` accessors are analogous to
`definition-transformer-ref`, but for expression prefix and infix
operators.

An infix expression operator like `+` is defined roughly like this:

```
(provide (rename-out [rhombus+ +])) ; and similar for `rhombus-`, etc.

(define-syntax rhombus+
  (expression-infix-operator #'rhombus+
                             (list (cons #'rhombus* 'weaker)
                                   (cons #'rhombus/ 'weaker)
                                   (cons #'rhombus- 'same))
                             'automatic
                             (lambda (form1 form2 stx)
                                ;; this is where we compile to Racket's `+`:
                                (quasisyntax/loc stx (+ #,form1 #,form2)))
                             'left))
```

The actual implementation has more layers of abstraction, deals with
macro scope introductions, supports a `define*`-like `forward`
definition form, implements more complicated syntax, and so on. Some
part of the language would be built in this low-level way, including
operator- and macro-defining forms like `operator` and
`expr.macro`, and then more of the Rhombus prototype language
could be built using the Rhombus prototype language.

# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

See [enforest/main.rkt](enforest/main.rkt),
[enforest/operator.rkt](enforest/operator.rkt),
[enforest/transformer.rkt](enforest/transformer.rkt), and associated
modules.

The proposal includes a prototype of `#lang rhombus` with declaration,
definition, expression, binding, type-like contract, require, and
provide contexts.

This `#lang rhombus` implementation is currently also available as a
package:
[https://github.com/mflatt/shrubbery-rhombus-0](https://github.com/mflatt/shrubbery-rhombus-0).
The package is likely to evolve more quickly than the `#lang rhombus`
prototype that is part of the proposal.

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
since the left-hand side of an infix operator is always parsed as an
expression. The combination of opinion and choice provided by the
Rhombus expander leads to a large set of implicit operators.

Although mapping spaces in the sense of `for-space` (normally called
“binding spaces”) are directly supported by `provide` and `require` in
Racket, it's a relatively new and unproven feature.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

For opinions on non-transitive operator precedence, see
[Jeff Walker's blog post](https://blog.adamant-lang.org/2019/operator-precedence/).

Although hierarchical name resolution could be implemented to some
degree through a prefix macro that works in any space, that approach
would not interact well with precedence or multiple contexts that use
the same mapping space (such as definitions and expressions). For
example, if `weather.(***)` and `weather.(!!!)` are meant to access
the `***` and `!!!` operators from the `weather` imported module,
those operators might have different precedences or associativity,
while `weather` by itself (or `.` as an operator) can have only one
predecence and associativity. Hierarchical naming seems common and
important enough to support well in the Rhombus expander.

Operator transformers could implement all operators, with full
precedence handling, but supporting non-transformer operators is
convenient and arguably clarifying for the algorithm/implementation.

Identifiers are treated differently than literals and other syntactic
elements that default to triggering implicit operators. Identifiers
that not mapped to transformers are handled by the
`make-identifier-form` argument to `define-enforest` instead of using
an implicit like `#%literal` or `#%tuple`. This choice reflects how
identifiers (along with operators) consult mappings, and it biases the
treatment of identifiers in a way, but a given `make-identifier-form`
could dispatch to an implicit if it makes sense for the context.

Identifiers are recognized in the same positions as shrubbery
operators for enforestation, but shrubbery operators are not expected
to be variables (in the sense of “variable” versus “syntax” in Racket;
that is, local operator mappings are certainly expected, but not, say,
operators as function-argument names). This convention gives
enforestation the liberty of complaining if two infix operators are
used in a row, if an operator is used in a context for which it has no
mapping, or if an operator is used where its mapping in the context is
not to an operator suitable for the context.

When contexts are associated with spaces, lookup in a space still
effectively consults the default space. and that's why the Rhombus
expander further relies on different kinds of compile-time values for
transformers that work in different contexts. Multiple transformers
might be mapped to an identifier or operator in the default space
through structure-type properties. The choice of using a single value
or mapping in different spaces may depend on the situation and whether
a default-space mapping exists already; supporting both layers of
distinction provides flexibility and extensibility.

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
