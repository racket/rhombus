#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def syntax_eval = make_rhombus_eval())

@title(~tag: "syntax"){Syntax Objects}

The @rhombus('') form produces a syntax object. The syntax object holds
an unparsed shrubbery, not a parsed Rhombus expression.

@demo(
  '1'
  'hello'
  '1 + 2'
  'x:
     y'
  ~error: '1' + 2
)

The @rhombus('') form is more precisely a quasiquoting operator. The
@rhombus($) operator unquotes the immediately following term. That is,
the term after @rhombus($) is a Rhombus expression whose value replaces
the @rhombus($) and its argument within the quoted form.

@demo(
  '1 + $(2 + 3)'
)

A @rhombus($) only unquotes when it is followed by a term, otherwise the @rhombus($)
itself remains quoted.

@demo(
  '1 + $('$') 2'
)

@aside{Nested @rhombus('') does not increase the quoting level, unlike
 Racket quasiquotation.}

Like @rhombus($), @rhombus(...) is treated specially within a @rhombus('')-quoted term (except,
like @rhombus($), when it’s the only thing in the term). When @rhombus(...) immediately
follows a term that includes at least one @rhombus($), the
form after that @rhombus($) must refer to a repetition. Then,
instead of the parenthesized group in place of @rhombus($), the term before
@rhombus(...) is replicated as many times as the repetition has items, and each of
those items is used in one replication.

@demo(
  ~defn:
    def [seq, ...] = ['1', '2', '3']
  ~repl:
    '(hi $seq) ...'
)

There’s a subtlety here: could @rhombus(seq) have zero elements? If so,
replicating the @rhombus(hi) form zero times within a group would leave
an empty group, but a shrubbery never has an empty group. To manage this
gap, a @rhombus($) replication to a group with zero terms generates a
multi-group syntax object with zero groups. Attempting to generate a group
with no terms within a larger sequence with multiple groups is an error.

@demo(
  ~repl:
    def [seq, ...] = []
    '(hi $seq) ...'
    ~error: 'x; (hi $seq) ...; y'
)

When @rhombus(...) is the only term in a group, and when that group follows
another, then @rhombus(...) replicates the preceding group. For example,
putting @rhombus(...) after a @litchar{,} in parentheses means that it follows a the
group before the @litchar{,}, which effectively replicates that group with its
separating comma:

@demo(
  ~repl:
    def [seq, ...] = ['1', '2', '3']
    '(hi $seq, ...)'
)

Along the same lines, @rhombus(...) just after a @litchar{|} can replicate a preceding
@litchar{|} block:

@demo(
  ~repl:
    def [seq, ...] = ['1', '2', '3']
    'cond | $seq | ...'
)

In other words, @rhombus(...) in various places within a quoted shrubbery
works the way you’d expect it to work.

When @rhombus('') is used in a binding position, it constructs a pattern that
matches syntax objects, and it binds variables that are escaped in the
pattern with @rhombus($).

@demo(
  ~repl:
    def '$x + $y' = '1 + (2 + 3)'
    x
    y
)

A @rhombus(..., ~bind) works the way you would expect in a syntax
pattern, matching any @rhombus(..., ~bind)-replicated pattern variables
to form a repetition of matches:

@demo(
  ~defn:
    def '$x + $y ... + 0' = '1 + 2 + 3 + 0'
  ~repl:
    x
    [y, ...]
    '$y ...'
)

@aside{A tail pattern @rhombus(#,(@rhombus($))#,(@rhombus(id, ~var)) #,(@rhombus(..., ~bind))) combined with a tail
 template @rhombus(#,(@rhombus($))#,(@rhombus(id, ~var)) ...) is similar to using @litchar{.} in
 S-expression patterns and templates, where it allows sharing between the
 input and output syntax objects. That sharing and an associated expansion-cost difference
 is all the more important in the Rhombus expansion protocol, which
 must thread potentially long sequences into and out of macro
 transformers.}

A @rhombus($)-escaped variable in a @rhombus('') pattern matches one
term among other terms in the group. Keep in mind that @rhombus('')
creates syntax objects containing shrubberies that are not yet parsed,
so a variable will @emph{not} be matched to a multi-term sequence that would be
parsed as an expression. For example, a pattern variable @rhombus(y) by
itself cannot be matched to a sequence @rhombus(2 + 3):

@demo(
  ~error: def '$x + $y' = '1 + 2 + 3'
)

Having pattern variables always stand for individual terms turns out to
be tedious, however. For example, to match a @rhombus(thunk) pattern
with a block that has any number of groups with any number of terms,
you'd have to use two layers of ellipses. Then, to substitute that same
body into a @rhombus(fun) template, you'd have to use the two layers of
ellipses again.

@demo(
  ~eval:
    syntax_eval
  ~defn:
    def thunk_form = 'thunk:
                        def x = 1
                        x + 1'
    def 'thunk: $term ...; ...' = thunk_form
  ~repl:
    'fun () $term ...; ...'
)

As a shorthand, when an escaped variable is alone in its group in a
pattern, it stands for a match to the whole group (at least by default).
A pattern variable that is alone in a multi-group context similarly
stands for a match to all the groups.

@demo(
  ~eval:
    syntax_eval
  ~defn:
    def 'thunk: $group; ...' = thunk_form
    def 'thunk: $body' = thunk_form
  ~repl:
    [group, ...]
    body
)

These multi-term and multi-group syntax objects can be spliced into
similar positions in templates, where an escape is by itself within its
group or by itself in a multi-group position.

@demo(
  ~eval:
    syntax_eval
  ~repl:
    'fun (): $group; ...'
    'fun (): $body'
)

There is no contraint that the original and destination contexts have
the same shape, so a match from a block-like context can be put into a
brackets context, for example.

@demo(
  def '$x' = '1 + 2 + 3
              4 * 5 * 6'
  '[$x]'
)

As a further generalization, a multi-term, single-group syntax object
can be spliced in place of any term escape.

@demo(
  ~repl:
    def '$x' = '1 + 2 + 3'
    '0 + $x'
)

This splicing rule applies only for escapes in templates, and not for
patterns. Also, a multi-group syntax object will not splice multiple
groups in place of a group escape, because that turns out to create
ambiguitites among group and term contexts. Meanwhile, a single-term
syntax object can be used as a group syntax object, a single-group
syntax object can be used as a multi-group syntax object, and a
single-term syntax object can be used as a multi-group syntax object.

Sometimes, a pattern variable that is alone within its group needs to
match a single term and not a group of terms. To match a single term in
a group context, annotate the pattern variable with the
@rhombus(Term, ~stxclass) syntax class using the @rhombus(::) operator.

@demo(
  ~repl:
    def '$(x :: Term)' = '1'
    x
  ~repl:
    ~error: def '$(x :: Term)' = '1 + 2'
)

Use the @rhombus(Group,~stxclass) syntax class to match a single group
instead of a multi-group sequence.

@close_eval(syntax_eval)
