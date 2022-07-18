#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title[~tag: "syntax"]{Syntax Objects}

The @rhombus[''] form produces a syntax object. The syntax object holds
an unparsed shrubbery, not a parsed Rhombus expression.

@(rhombusblock:
    '1'          // prints a shrubbery: 1
    'hello'      // prints a shrubbery: hello
    '1 + 2'      // prints a shrubbery: 1 + 2
    'x:
       y'       // prints a shrubbery: x:« y »
    // '1' + 2  // would be a run-time error, since '1 is not a number
  )

The @rhombus[''] form is more precisely a quasiquoting operator. The
@rhombus[$] unquotes the immediate following terms. That is, the term
after @rhombus[$] is a Rhombus expression whose value replaces the
@rhombus[$] and its argument within the quoted form.

@(rhombusblock:
    '1 + $(2 + 3)'  // prints a shrubbery: 1 + 5
  )

A @rhombus[$] only unquotes when it is followed by a term, otherwise the @rhombus[$]
itself remains quoted.

@(rhombusblock:
    '1 + $('$') 2'  // prints a shrubbery: 1 + $ 2
  )

@aside{Nested @rhombus[''] does not increase the quoting level like
 Racket quasiquote.}

Like @rhombus[$], @rhombus[...] is treated specially within a @rhombus['']-quoted term (except,
like @rhombus[$], when it’s the only thing in the term). When @rhombus[...] immediately
follows a term that includes at least one @rhombus[$], the
form after that @rhombus[$] must refer to a repetition. Then,
instead of the parenthesized group in place of @rhombus[$], the term before
@rhombus[...] is replicated as many times as the repetition has items, and each of
those items is used in one replication.

@(rhombusblock:
    def [seq, ...]: ['1', '2', '3']

    '(hi $seq) ...' // prints a shrubbery: (hi 1) (hi 2) (hi 3)
  )

There’s a subtlety here: could @rhombus[seq] have zero elements? If so,
replicating the @rhombus[hi] form zero times within a group would leave
an empty group, but a shrubbery never has an empty group. To manage this
gap, a @rhombus[$] replication to a group with zero terms generates a
multi-group syntax object with zero groups. Attempting to generate a group
with no terms within a larger sequence with multiple groups is an error.

@(rhombusblock:
    def [seq, ...]: []

    '(hi $seq) ...'           // prints an empty shrubbery
    // 'x; (hi $seq) ...; y'  // would be a run-time error
  )

When @rhombus[...] is the only term in a group, and when that group follows
another, then @rhombus[...] replicates the preceding group. For example,
putting @rhombus[...] after a @litchar{,} in parentheses means that it follows a the
group before the @litchar{,}, which effectively replicates that group with its
separating comma:

@(rhombusblock:
    def [seq, ...]: ['1', '2', '3']

    '(hi $seq, ...)' // prints a shrubbery: (hi 1, hi 2, hi 3)
  )

Along the same lines, @rhombus[...] just after a @litchar{|} can replicate a preceding
@litchar{|} block:

@(rhombusblock:
    def [seq, ...]: ['1', '2', '3']

    'cond | $seq | ...' // prints a shrubbery: cond |« 1  » |« 2  » |« 3 »
  )

In other words, @rhombus[...] in various places within a quoted shrubbery
works the way you’d expect it to work.

When @rhombus[''] is used in a binding position, it constructs a pattern that
matches syntax objects, and it binds variables that are escaped in the
pattern with @rhombus[$].

@(rhombusblock:
    val '$x + $y': '1 + (2 + 3)'

    x  // prints a shrubbery: 1
    y  // prints a shrubbery: (2 + 3)
  )

A @rhombus[$]-escaped variable in a @rhombus[''] pattern matches one term if
the group with the @rhombus[$] escape has other terms. Keep in mind
that @rhombus[''] creates syntax objects containing shrubberies that are not
yet parsed, so a variable will not be matched to a multi-term sequence
that would be parsed as an expression. For example, a pattern variable
@rhombus[y] by itself cannot be matched to a sequence @rhombus[2 + 3]:

@(rhombusblock:«
    // val '$x + $y': '1 + 2 + 3'  // would be a run-time error
  »)

If a @rhombus[$] escape is alone within its group, however, the
@rhombus[$]-escaped variable stands for a match to an entire group.

@(rhombusblock:
    val '$x': '1 + 2 + 3'
    x  // prints a shrubbery group: 1 + 2 + 3
    )

A group syntax object can be substituted into a template when the escape
to substitute is similarly in its own group. Attempting to substitute a
multi-term group in any other template context is an error.

@(rhombusblock:
    val '$x': '1 + 2 + 3'
    '[$x]'     // prints a shrubbery: [1 + 2 + 3]
    // '[0 + $x]'  // would be a run-time error
  )

At the same time, a group syntax object that has a single term in the
group is interchangeable with a single-term syntax object:

@(rhombusblock:
    val '$y': '1'
    '[$y]'      // prints a shrubbery: [1]
    '[0 + $y]'  // prints a shrubbery: [0 + 1]
  )

To match a single term in a group context, annotate the pattern variable
with the @rhombus[Term, ~stxclass] syntax class using the @rhombus[::] operator.

@(rhombusblock:
    val '$(x :: Term)': '1'
    x  // prints a shrubbery: 1

    // val '$(x :: Term)': '1 + 2' // would be an run-time error
  )

If a @rhombus[$] escape is not only alone within its group, but the
group is the only one in a sequence of groups, then the
@rhombus[$]-escaped variable stands for a match to an entire sequence of
groups. In a template, when a group-sequence context has a single group
with only an escape in the group, then it can be filled with a
multi-group syntax object. There is no contraint that the original and
destination contexts have the same shape, so a match from a block-like
context can be put into a brackets context, for example.

@(rhombusblock:
    val '$x': '1 + 2 + 3
               4 * 5 * 6'
    '[$x]'     // prints a shrubbery: [1 + 2 + 3, 4 * 5 * 6]
  )

In the same way that a single-term syntax object can be used as a group
syntax object, a single-group syntax object can be used as a multi-group
syntax object, and a single-term syntax object can be used as a
multi-term syntax object. Use the @rhombus[Group,~stxclass] syntax class
to match a single group instead of a multi-group sequence.

Meanwhile, @rhombus[..., ~bind] works the way you would expect in a
pattern, matching any @rhombus[..., ~bind]-replicated pattern variables
to form a repetition of matches:

@(rhombusblock:
    val '$x + $y ... + 0': '1 + 2 + 3 + 0'

    x         // prints a shrubbery: 1
    [y, ...]  // prints a list: ['2', '+', '3']
    '$y ...'  // prints a shrubbery: 2 + 3
  )

@aside{A tail pattern @rhombus[$$(@rhombus[$])$$(@rhombus[id, ~var]) $$(@rhombus[..., ~bind])] combined with a tail
 template @rhombus[$$(@rhombus[$])$$(@rhombus[id, ~var]) ...] is similar to using @litchar{.} in
 S-expression patterns and templates, where it allows sharing between the
 input and output syntax objects. That sharing and an associated expansion-cost difference
 is all the more important in the Rhombus expansion protocol, which
 must thread potentially long sequences into and out of macro
 transformers.}
