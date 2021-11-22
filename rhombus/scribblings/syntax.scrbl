#lang scribble/rhombus/manual
@(import: "util.rhm": no_prefix)

@title[~tag: "syntax"]{Syntax Objects}

The @rhombus['] operator quotes an immediately following shrubbery term
to produce a syntax object. The syntax object holds an unparsed
shrubbery, not a parsed Rhombus expression.

@(rhombusblock:
    '1         // prints a shrubbery: 1
    'hello     // prints a shrubbery: hello
    '(1 + 2)   // prints a shrubbery: (1 + 2)
    '(x:
        y)     // prints a shrubbery: x:« y »
    // '1 + 2  // would be a run-time error, since '1 is not a number
  )

The @rhombus['] operator is more precisely a quasiquoting operator. The
@rhombus[$] unquotes the immediate following terms. That is, the term
after @rhombus[$] is a Rhombus expression whose value replaces the
@rhombus[$] and its argument within the quoted form.

@(rhombusblock:
    '(1 + $(2 + 3))  // prints a shrubbery: (1 + 5)
  )

A @rhombus[$] only unquotes when it is followed by a term, otherwise the @rhombus[$]
itself remains quoted.

@(rhombusblock:
    '(1 + $(' $) 2)  // prints a shrubbery: '(1 + $ 2)
  )

A nested @rhombus['] increments the quoting level so that @rhombus[$]
does not escape, but an equivalently nested @rhombus[$] escapes.

@(rhombusblock:
    '(1 + '($($(2 + 3))))  // prints a shrubbery: '(1 + '($(5)))
  )

Like @rhombus[$], @rhombus[...] is treated specially within a @rhombus[']-quoted term (except,
like @rhombus[$] when it’s the only thing in the term). When @rhombus[...] immediately
follows a term that includes at least one @rhombus[$], the value of the
expression after that @rhombus[$] must produce a list of syntax objects. Then,
instead of the parenthesized group in place of @rhombus[$], the term before
@rhombus[...] is replicated as many times as the list has items, and each of
those items is used in one replication.

@(rhombusblock:
    def seq: ['1, '2, '3]

    '((hi $seq) ...) // prints a shrubbery: ((hi 1) (hi 2) (hi 3))
  )

There’s a subtlety here: could @rhombus[seq] have zero elements? If so,
replicating the @rhombus[hi] form zero times within a group would leave an
empty group, but a shrubbery never has an empty group. To manage this
gap, a parenthesized shrubbery form with zero groups is treated as
equivalent for @rhombus[$] replication to a parenthesized form with an empty
group. Similarly, when a @rhombus['] form describes a single parenthesized
group that turns out to be empty, it instead produces a parenthesized
form with zero groups. Attempting to generate a group with no terms
within a parenthesized form with multiple groups is an error.

@(rhombusblock:
    def seq: []

    '((hi $seq) ...)          // prints a shrubbery: ()
    // '(x, (hi $seq) ..., y) // would be a run-time error
  )

Square-bracket forms and block forms have the same special cases to
deal with an empty group as parenthesis forms.

When @rhombus[...] is the only term in a group, and when that group follows
another, then @rhombus[...] replicates the preceding group. For example,
putting @rhombus[...] after a @litchar{,} in parentheses means that it follows a the
group before the @litchar{,}, which effectively replicates that group with its
separating comma:

@(rhombusblock:
    def seq: ['1, '2, '3]

    '(hi $seq, ...) // prints a shrubbery: (hi 1, hi 2, hi 3)
  )

Along the same lines, @rhombus[...] just after a @litchar{|} can replicate a preceding
@litchar{|} block:

@(rhombusblock:
    def seq: ['1, '2, '3]

    '(cond | $seq | ...) // prints a shrubbery: cond |« 1  »|« 2  »|« 3 »
  )

In other words, @rhombus[...] in various places within a quoted shrubbery
works the way you’d expect it to work.

When @rhombus['] is used in a binding position, it constructs a pattern that
matches syntax objects, and it binds variables that are escaped in the
pattern with @rhombus[$].

@(rhombusblock:
    val '($x + $y): '(1 + (2 + 3))

    x  // prints a shrubbery: 1
    y  // prints a shrubbery: (2 + 3)
  )

A @rhombus[$]-escaped variable in a @rhombus['] pattern matches one term if
the group with the @rhombus[$] escape has other terms. Keep in mind
that @rhombus['] creates syntax objects containing shrubberies that are not
yet parsed, so a variable will not be matched to a multi-term sequence
that would be parsed as an expression. For example, a pattern variable
@rhombus[y] by itself cannot be matched to a sequence @rhombus[2 + 3]:

@(rhombusblock:«
    // val '($x + $y): '(1 + 2 + 3)  // would be a run-time error
  »)

If a @rhombus[$] escape is alone within its group, however, the
@rhombus[$]-escaped variable stands for a match to an entire group.

@(rhombusblock:
    val '($x): '(1 + 2 + 3)
    x  // prints a shrubbery group: 1 + 2 + 3
    )

A group syntax object can be substituted into a template when the escape
to substitute is similarly in its own group. Attempting to substitute a
multi-term group in any other template context is an error.

@(rhombusblock:
    val '($x): '(1 + 2 + 3)
    '[$x]      // prints a shrubbery: [1 + 2 + 3]
    // '[0 + $x]  // would be a run-time error
  )

At the same time, a group syntax object that has a single term in the
group is interchangeable with a single-term syntax object:

@(rhombusblock:
    val '($y): '(1)
    '[$y]      // prints a shrubbery: [1]
    '[0 + $y]  // prints a shrubbery: [0 + 1]
  )

To match a single term in a group context, annotate the pattern variable
with the @rhombus[Term] syntax class using the @rhombus[$:] operator.

@(rhombusblock:
    val '($(x $: Term)): '(1)
    x  // prints a shrubbery: 1

    // val '($(x $: Term)): '(1 + 2) // would be an run-time error
  )

Meanwhile, @rhombus[...] works the way you would expect in a pattern, matching
any @rhombus[...]-replicated pattern variables to form a list of matches:

@(rhombusblock:
    val '($x + $y ...): '(1 + 2 + 3)

    x  // prints a shrubbery: 1
    y  // prints a list: ['2, ' +, '3]
  )

A @rhombus[......] behaves similarly to @rhombus[...], but for a @deftech{tail
replication} that can only appear at the end of a group. In a patttern,
an escaped variable must appear before @rhombus[......], and instead of
binding the variable to a list, the variable is bound to a syntax object
for a parenthesized term that contains the matched tail. In a template,
an escaped expression must appear before @rhombus[......], and it must
produce a syntax object for a parenthesized term.

Use @rhombus[......] for tail sequences that you don’t need to inspect,
because the syntax-object representation can avoid work proportional to
the length of the matched tail. Avoiding that work can be important for
macros.

@(rhombusblock:
    val '($head $tail ......): '(1 2 3 4 5)

    head  // prints a shrubbery: 1
    tail  // prints a shrubbery: (2 3 4 5)
    '(0 $tail ......) // prints a shrubbery: (0 2 3 4 5)
  )

@aside{Using @rhombus[......] is similar to using @rhombus[.] in
 S-expression patterns and templates. The difference can avoid quadratic
 expansion costs, which is all the more important in the Rhombus
 expansion protocol, which must thread potentially long sequences into
 and out of macro transformers.}
