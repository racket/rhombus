#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
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
compile-time computation, and it can return terms that the macro does
not consume.

For example, here's a @rhombus(thunk) macro that expects a block and
wraps as a zero-argument function

@demo(
  ~eval: macro_eval
  ~defn:
    import:
      rhombus/meta open

    expr.macro 'thunk: $body':
      'fun (): $body'
  ~repl:
    thunk: 1 + "oops"
    (thunk: 1 + 3)()
)

The @rhombus(expr.macro) form expects @rhombus('') to create a pattern
that matches a sequence of terms. Either the first or second term within
the pattern is an @emph{unescaped} identifier or operator to be defined;
conceptually, it’s unescaped because the macro matches a sequence of
terms that use that identifier or operator literally. If the first term
in the pattern is an unescaped identifier or operator, a prefix macro is
defined; otherwise, the second term must be unescaped, and an infix
macro is defined.

The @rhombus(expr.macro) form must be imported from
@rhombusmodname(rhombus/meta), but @rhombus(macro) is available in just
@rhombusmodname(rhombus):

@demo(
  ~defn:
    :
      // no import needed      
      macro 'thunk: $body':
        'fun (): $body'
  ~repl:
    (thunk: 1 + 3)()
)

A postfix macro can be implemented as an infix operator that consumes no
additional terms after the operator. For example, a postfix @rhombus(!)
might be defined (shadowing the normal @rhombus(!) for ``not'') like
this:

@demo(
  ~eval: macro_eval
  ~defn:
    macro '$a !':
      'factorial($a)'
  ~defn:
    fun
    | factorial(0): 1
    | factorial(n): n*factorial(n-1)
  ~repl:
    10! + 1
)

With @rhombus(expr.macro) (but not @rhombus(macro)), the
macro implementation after @rhombus(:) is compile-time code. Importing
@rhombusmodname(rhombus/meta) imports all of the same bindings as
@rhombusmodname(rhombus) into the compile-time phase, in addition to making
forms like @rhombus(expr.macro) available. Normally,
@rhombusmodname(rhombus/meta) should be imported without a prefix, otherwise a
prefix would have to be used for all Rhombus forms in compile-time
code—even for things like @rhombus(values) and @rhombus('').

A macro defined with @rhombus(expr.macro) matches any prefix of terms in
the enclosing group. By using @rhombus(..., ~bind), it can consume all
remaining terms in the group. In that case, the macro may want to
consume some of them, but return others back to the group to be parsed
normally, so the macro can return two values: the expanded expression
and the remaining terms that have not been consumed.

For example, the @rhombus(!) macro can be equivalently written like this:

@demo(
  ~eval: macro_eval
  ~defn:
    expr.macro '$a ! $tail ...':
      values('factorial($a)', '$tail ...')
)

Returning a single value is allowed, and is the same as returning an
empty sequence for the remaining terms, which is useful when
@rhombus($(~end)) is needed only to indicate that no further terms are
allowed in the group. Two return values are allowed only when the
pattern ends with
@rhombus($ #,(@rhombus(identifier, ~var)) #,(@rhombus(..., ~bind))) or
when a @rhombus($(~end)) pseudo-escape is added to the end of the
pattern. Adding a @rhombus($(~end)) pseudo-escape while returning one
value can be useful to disallow additional terms after the matched
pattern.

Since an @rhombus(expr.macro) implementation can use arbitrary
compile-time code, it can inspect the input syntax objects in more way
than just pattern matching. However, already-parsed terms will be
opaque. When the macro transformer for @rhombus(!) is called,
@rhombus(a) will be bound to a syntax object representing a parsed
Rhombus expression, as opposed to an unparsed shrubbery. Currently,
there’s no way for a transformer to inspect a parsed Rhombus expression
(except by escaping to Racket). When the parsed expression is injected
back into an unparsed shrubbery, as happens in
@rhombus('factorial($a)'), it will later simply parse as itself.

Changing the @rhombus(tail) pattern to @rhombus((tail :: Term)) would
disable the special treatment of @rhombus(..., ~bind) at the end of a
pattern and template sequence and reify the tail as a fresh list---so
don't do this:

@demo(
  ~eval: macro_eval
  ~defn:
    expr.macro '$a ! $(tail :: Term) ... $(~end)':
      values('factorial($a)', '$tail ...')
  ~repl:
    :
      // changing to 2000 `!`s or so makes parsing take noticeably long:
      0 ! ! ! ! ! ! ! ! ! ! ! !
)

Whether pattern-based or not, an infix-operator macro’s left-hand input
is parsed. A prefix or infix macro’s right-hand input is not parsed by
default. To trigger parsing for a right-hand argument, wrap the pattern
binding with parentheses and the keyword @rhombus(~parsed). You can also
wrap the left-hand variable with @rhombus(~parsed), but that's redundant,
because the left-hand argument is always parsed first. With both arguments
as parsed, you might as well use @rhombus(operator) in many cases, but
macros provide control over evaluator order. For example, this
@rhombus(+<=) operator is like @rhombus(+), but evaluates its right-hand
side before it’s left-hand side:

@demo(
  ~defn:
    macro '$a +<= $(~parsed b)':
      '$b + $a'
  ~repl:
    1 +<= 2
    ~error: (1+"oops") +<= (2+"ouch")  // complains about "ouch", not "oops"
)

Declaring @rhombus(~parsed) affects a @rhombus(expr.macro) macro in a
second way: the macro will receive only the left (if any) and right
arguments, and will not receive and cannot return the tail of the
enclosing group.

In the same way that @rhombus(operator) supports operators that are both
prefix and infix, you can use an alt-block with @rhombus(expr.macro) to
create a prefix-and-infix macro. Furthermore, an @rhombus(expr.macro)
form can have multiple prefix blocks or multiple infix blocks, where the
each block’s pattern is tried in order; in that case, only the first
prefix block (if any) and first infix block (if any) can have precedence
and associativity declarations that apply to all cases.


@close_eval(macro_eval)
