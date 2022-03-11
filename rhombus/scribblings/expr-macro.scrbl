#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title[~tag: "expr-macro"]{Expression Macros}

Macros extend the syntax available for expressions, bindings,
definitions, and more. Each kind of macro extension has a different
protocol and a different form for defining a macro. In the case of
expressions, a macro receives the sequence of terms starting with the
macro operator/identifier within a group. A @deftech{rule} macro
consumes only the tokens that its pattern matches, and it’s right-hand
side is an immediate template expression that produces the macro
expansion. A general form of macro is implemented with arbitrary
compile-time computation, and it can return terms that the macro does
not consume.

For example, here's a @rhombus[thunk] macro that expects a block and
wraps as a zero-argument function

@(rhombusblock:
    import:
      rhombus/macro: open

    expr.rule 'thunk: $body':
      'fun (): $body'

    thunk: 1 + "oops"  // prints a function
    (thunk: 1 + 3)()   // prints 4
  )

The @rhombus[expr.rule] form expects @rhombus[''] to create a pattern
that matches a sequence of terms. Either the first or second term within
the pattern is an @emph{unescaped} identifier or operator to be defined;
conceptually, it’s unescaped because the macro matches a sequence of
terms that use that identifier or operator literally. If the first term
in the pattern is an unescaped identifier or operator, a prefix macro is
defined; otherwise, the second term must be unescaped, and an infix
macro is defined.

The @rhombus[expr.rule] form must be imported from
@rhombus[rhombus/macro], but @rhombus[def] behaves like
@rhombus[expr.rule] when the part before @rhombus[:] is valid for
@rhombus[expr.rule]:

@(rhombusblock:
    // no import needed

    def 'thunk: $body':
      'fun (): $body'

    (thunk: 1 + 3)()   // prints 4
  )

A postfix macro can be implemented as an infix operator that consumes no
additional terms after the operator. For example, a postfix @rhombus[!]
might be defined (shadowing the normal @rhombus[!] for ``not'') like
this:

@(rhombusblock:
    def '$a !':
      'factorial($a)'

    fun
    | factorial(0): 1
    | factorial(n): n*factorial(n-1)

    10! + 1 // = 3628801
  )

The @rhombus[expr.rule] or @rhombus[def] form is a shorthand for a more
general @rhombus[expr.macro] macro form. With @rhombus[expr.macro], the
macro implementation after @rhombus[:] is compile-time code. Importing
@rhombusmodname[rhombus/macro] imports all of the same bindings as
@rhombus[rhombus] into the compile-time phase, in addition to making
forms like @rhombus[expr.macro] available. Normally,
@rhombusmodname[rhombus/macro] should be imported without a prefix, otherwise a
prefix would have to be used for all Rhombus forms in compile-time
code—even for things like @rhombus[values] and @rhombus['']. In addition,
a macro defined with @rhombus[expr.macro] receives all remaining terms
in the enclosing group as input, and it must return two values: the
expanded expression and the remaining terms that have not been consumed.

For example, the @rhombus[!] macro can be equivalently written like this:

@(rhombusblock:
    expr.macro '$a ! $tail ......':
      values('factorial($a)', 'tail ......')
  )

Since an @rhombus[expr.macro] implementation can use arbitrary
compile-time code, it can inspect the input syntax objects in more way
than just pattern matching. However, already-parsed terms will be
opaque. When the macro transformer for @rhombus[!] is called,
@rhombus[a] will be bound to a syntax object representing a parsed
Rhombus expression, as opposed to an unparsed shrubbery. Currently,
there’s no way for a transformer to inspect a parsed Rhombus expression
(except by escaping to Racket). When the parsed expression is injected
back into an unparsed shrubbery, as happens in
@rhombus['factorial($a)'], it will later simply parse as itself.

The @rhombus[!] macro matches an unconsumed tail with @rhombus[......]
instead of @rhombus[...]. Using @rhombus[...] would implement the same
transformation, but less efficiently:

@(rhombusblock:
    expr.macro '$a ! $tail ...':
      values('factorial($a)', 'tail ...')

    // changing to 2000 `!`s or so makes parsing take noticeably long:
    0 ! ! ! ! ! ! ! ! ! ! ! !
  )

Since @rhombus[......] after @rhombus[tail] in a pattern binds
@rhombus[tail] to a parenthesized sequence, the macro’s second result
can be written as just @rhombus[tail]. That’s convenenient, although not
inherently more efficient:

@(rhombusblock:
    expr.macro '$a ! $tail ......':
      values('factorial($a)', tail)
  )

The @rhombus[......] operator cannot be used at the end of the input
group for a rule macro, because there’s implicitly a
@rhombus[$tail ......] at the end of every rule-macro pattern. If you
try replacing @rhombus[...] in the @rhombus[thunk] implementation with
@rhombus[......], it will work, but that’s because the @rhombus[......]
in that case is under a block created by @rhombus[:], and not in the
enclosing input group.

Whether pattern-based or not, an infix-operator macro’s left-hand input
is parsed. A prefix or infix macro’s right-hand input is not parsed by
default. To trigger parsing for a right-hand argument, include
@rhombus[~parsed_right] as a declaration at the start of the macro body.
Often, in that case, you might as well use @rhombus[operator], but
macros provide control over evaluator order. For example, this
@rhombus[+<=] operator is like @rhombus[+], but evaluates its right-hand
side before it’s left-hand side:

@(rhombusblock:
    def '$a +<= $b':
      ~parsed_right
      '$b + $a'

    1 +<= 2                       // prints 3
    // (1+"oops") +<= (2+"ouch")  // would complain about "ouch", not "oops"
  )

Declaring @rhombus[~parsed_right] affects a @rhombus[expr.macro] macro
in a second way: the macro will receive only the left (if any) and right
arguments, and will not receieve or return the tail of the ennclosing
group. In other words, declaring @rhombus[~parse_right] uses the same
argument and return protocol as a rule-based macro, but the template
part can be implemented by arbitrary compile-time expressions.

In the same way that @rhombus[operator] supports operators that are both
prefix and infix, you can use an alt-block with @rhombus[expr.rule] or
@rhombus[expr.macro] to create a prefix-and-infix macro. Furthermore, an
@rhombus[expr.rule] or @rhombus[expr.macro] form can have multiple
prefix blocks or multiple infix blocks, where the each block’s pattern
is tried in order; in that case, only the first prefix block (if any)
and first infix block (if any) can have precedence and associativity
declarations that apply to all cases, and none of the cases can use
@rhombus[~parsed_right].
