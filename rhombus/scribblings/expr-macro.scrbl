#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@(def macro_eval = make_rhombus_eval())

@title(~tag: "expr-macro"){Expression Macros}

Macros extend the syntax available for expressions, bindings,
definitions, and more. Each kind of macro extension has a different
protocol and a different form for defining a macro. In the case of
expressions, a macro receives the sequence of terms starting with the
macro operator/identifier within a group. A macro
consumes only the tokens that its pattern matches, and it’s right-hand
side is an immediate template expression that produces the macro
expansion. A general form of macro is implemented with arbitrary
compile-time computation, and it can return terms that the macro's
pattern matches but that the macro does not consume.

For example, here's a @rhombus(thunk) macro that expects a block and
wraps as a zero-argument function:

@examples(
  ~eval: macro_eval
  ~defn:
    import:
      rhombus/meta open
  ~defn:
    expr.macro 'thunk: $body':
      'fun (): $body'
  ~repl:
    thunk: 1 + "oops"
    (thunk: 1 + 3)()
)

An @rhombus(expr.macro) form starts with @quotes to describe a pattern
that matches a sequence of terms. Either the first or second term within
the pattern is an @emph{unescaped} identifier or operator to be defined;
conceptually, it’s unescaped because the macro matches a sequence of
terms that use that identifier or operator literally. If the first term
in the pattern is an unescaped identifier or operator, a prefix macro is
defined; otherwise, the second term must be unescaped, and an infix
macro is defined. The above macro definition starts with an unescaped
@rhombus(thunk), so it defined the @rhombus(thunk) macro.

The @rhombus(expr.macro) form must be imported from
@rhombusmodname(rhombus/meta), but a @rhombus(macro) is
available in just @rhombuslangname(rhombus). The @rhombus(macro) form
is more limited, because it's body must be a template written with @quotes,
instead of an arbitrary compile-time expression.

@examples(
  ~defn:
    :
      // no import needed
      macro 'thunk: $body':
        'fun (): $body'
  ~repl:
    (thunk: 1 + 3)()
)

A postfix macro can be implemented as an infix operator that consumes no
additional terms after the operator. For example, a postfix
@rhombus(no_fail) might be defined like this:

@examples(
  ~eval: macro_eval
  ~defn:
    macro '$left no_fail':
      'try:
         $left
         ~catch _: #false'
  ~repl:
    "hello" no_fail
    math.sqrt("hello") no_fail
    math.sqrt("hello") no_fail || "undefined"
)

With @rhombus(expr.macro) (but not @rhombus(macro)), the
macro implementation after @colon is compile-time code. Importing
@rhombusmodname(rhombus/meta) imports all of the same bindings as
@rhombuslangname(rhombus) into the compile-time phase, in addition to making
forms like @rhombus(expr.macro) available. Normally,
@rhombusmodname(rhombus/meta) should be imported without a prefix, otherwise a
prefix would have to be used for all Rhombus forms in compile-time
code—even for things like @rhombus(values) and @(quotes).

Whether defined by @rhombus(macro) or @rhombus(expr.macro), an
infix/postfix macro’s left-hand input is always parsed before the
macro's pattern is potentially matched, and an infix/postfix macro's
pattern must always have just @rhombus($, ~bind) followed by an
identifier for its left-hand side. The pattern variable is bound to an
opaque representation of the parsed form. In the definition of
@rhombus(no_fail) above, the pattern variable @rhombus(left) stands for a parsed
form, and not just a single shrubbery term. The form is parsed according
to declared precedence relationships; specify precence for a macro in
the same way as for @rhombus(operator).

@examples(
  ~eval: macro_eval
  ~defn:
    macro '$left no_fail':
      ~weaker_than: /
      'try:
         $left
         ~catch _: #false'
  ~repl:
    1/0 no_fail
)

Since an @rhombus(expr.macro) implementation can use arbitrary
compile-time code, it can inspect the input syntax objects in more way
than just pattern matching. However, already-parsed terms will be
opaque. Currently, there’s no way for a transformer to inspect a parsed
Rhombus expression (except by escaping to Racket). When the parsed
expression is injected back into an unparsed shrubbery, as happens in
@rhombus('try: $left'), it will later simply parse as itself.

Whether an infix or prefix macro’s right-side is parsed depends on the
shape of the macro pattern, and specifically whether the defined
operator or identifier is followed by just @rhombus($, ~bind) and an
identifier. If so, the right-hand side of a macro's input is parsed,
analogous to the left-hand side of an infix/postfix macro; the identifier
pattern variable stands for an opaque parsed form. With both arguments
as parsed, you might as well use @rhombus(operator) in many cases, but
macros provide control over evaluation order. For example, this
@rhombus(+<=) operator is like @rhombus(+), but evaluates its right-hand
side before it’s left-hand side:

@examples(
  ~defn:
    macro '$left +<= $right':
      '$right + $left'
  ~repl:
    1 +<= 2
    ~error:
      (1+"oops") +<= (2+"ouch")  // complains about "ouch", not "oops"
)

If the infix/prefix operator in a macro pattern is not followed by just
@rhombus($, ~bind) and an identifier, then it matches an unparsed
sequence of shrubbery terms after the operator. That kind of operator might not
be infix in the sense of the @rhombus(+) operator, which expects two
expressions, but infix in the sense of the @rhombus(.) operator, which
expects an expression on the left but a plain identifier on the right.
For example, to match just an identifier in a macro pattern,
annotate a pattern variable with @rhombus(::, ~unquote_bind) and
@rhombus(Identifier, ~stxclass):

@examples(
  ~eval: macro_eval
  ~defn:
    macro '$left is_name $(name :: Identifier)':
      ~stronger_than: ~other
      '$left == #'$name'
  ~repl:
    #'apple is_name apple
    #'banana is_name apple
    #'banana is_name apple || #'banana is_name banana
)

As illustrated by the last @rhombus(is_name) example, additional terms
can be in the group after a sequence that matches a macro's pattern. Expression
parsing continues with those additional terms. A macro can use a
@rhombus(..., ~bind) repetition to match all remaining terms in the
group, instead. In that case, the macro may want to consume some of them, but
return others back to the group to be parsed normally. The macro can
return two values: the expanded expression and the remaining terms that
have not been consumed.

For example, the @rhombus(no_fail) macro can be equivalently written like this:

@examples(
  ~eval: macro_eval
  ~defn:
    expr.macro '$left no_fail $tail ...':
      values('try: $left; ~catch _: #false', '$tail ...')
)

Returning a single value is allowed, and that's the same as returning an
empty sequence for the remaining terms. Two return values are allowed
only when the macro pattern ends with
@rhombus(#,(@rhombus($, ~bind))#,(@rhombus(identifier, ~var)) #,(@rhombus(..., ~bind)))
or with @rhombus(#,(@rhombus($, ~bind))()), where @rhombus(#,(@rhombus($, ~bind))())
means that the pattern is required to match to the end of a group. In some
cases,  @rhombus(#,(@rhombus($, ~bind))()) can be useful to constrain
a macro uses to appear at the end of a group.

@examples(
  ~eval: macro_eval
  ~defn:
    macro '$left EOM $()':
      ~weaker_than: ~other
      '$left'
  ~repl:
    1 + 2 EOM
    ~error:
      1 + 2 EOM 3 * 4
)

In our latest definition of @rhombus(no_fail), changing the @rhombus(tail)
pattern to @rhombus((tail :: Term)) would disable the special treatment
of @rhombus(..., ~bind) at the end of a pattern and template sequence
and reify the tail as a fresh list---so don't do this:

@examples(
  ~eval: macro_eval
  ~defn:
    expr.macro '$left no_fail $(tail :: Term) ... $()':
      values('try: $left; ~catch _: #false', '$tail ...')
  ~repl:
    :
      // changing to 2000 `no_fail`s or so makes parsing take noticeably long:
      0 no_fail no_fail no_fail no_fail no_fail no_fail no_fail
)

In the same way that @rhombus(operator) supports operators that are both
prefix and infix, you can use @vbar alternatives with
@rhombus(expr.macro) to create a prefix-and-infix macro. Furthermore, an
@rhombus(expr.macro) form can have multiple prefix blocks or multiple
infix blocks, where the each block’s pattern is tried in order; in that
case, only the first prefix block (if any) and first infix block (if
any) can have precedence and associativity declarations that apply to
all cases---or precedence can be written before all alternatives by
nesting the alternatives under @rhombus(match), as in @rhombus(operator).


@(close_eval(macro_eval))
