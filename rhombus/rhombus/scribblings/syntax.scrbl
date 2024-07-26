#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def syntax_eval = make_rhombus_eval())

@title(~tag: "syntax"){Syntax Objects}

The @quotes form produces a @tech{syntax object}. The syntax object holds
an unparsed shrubbery, not a parsed Rhombus expression.

@examples(
  '1'
  'hello'
  '1 + 2'
  'x:
     y'
  ~error:
    '1' + 2
)

Within the @quotes, the
@rhombus($) operator unquotes the immediately following term. That is,
the term after @rhombus($) is a Rhombus expression whose value replaces
the @rhombus($) and its argument within the quoted form.

@examples(
  '1 + $(2 + 3)'
)

A @rhombus($) only unquotes when it is followed by a term, otherwise the @rhombus($)
itself remains quoted.

@examples(
  '1 + $('$') 2'
)

Nesting quotes do not require a corresponding nesting of escaping
@rhombus($) to escape outside the original quotes. For example,
@rhombus('('$(1+2)')') is the same as @rhombus('('3')') in Rhombus,
even though the escape is inside two layers of quotes.

Like @rhombus($), @rhombus(...) is treated specially within a @(quotes)-quoted term (except,
like @rhombus($), when it’s the only thing in the term). When @rhombus(...) immediately
follows a term that includes at least one @rhombus($), the
form after that @rhombus($) must refer to a repetition. Then,
instead of the parenthesized group in place of @rhombus($), the term before
@rhombus(...) is replicated as many times as the repetition has items, and each of
those items is used in one replication.

@examples(
  ~defn:
    def [seq, ...] = ['1', '2', '3']
  ~repl:
    '$seq ...'
    '(hi $seq) ...'
)

When @rhombus(...) is the only term in a group, and when that group follows
another, then @rhombus(...) replicates the preceding group. For example,
putting @rhombus(...) after a @comma in parentheses means that it follows a the
group before the @comma, which effectively replicates that group with its
separating comma:

@examples(
  ~repl:
    def [seq, ...] = ['1', '2', '3']
    '(hi $seq, ...)'
)

Along the same lines, @rhombus(...) just after a @vbar can replicate a preceding
@litchar{|} block:

@examples(
  ~repl:
    def [seq, ...] = ['1', '2', '3']
    'cond | $seq | ...'
)

In other words, @rhombus(...) in various places within a quoted shrubbery
works the way you’d expect it to work.

When @quotes is used in a binding position, it constructs a pattern that
matches syntax objects, and it binds variables that are escaped in the
pattern with @rhombus($).

@examples(
  ~repl:
    def '$x + $y' = '1 + (2 + 3)'
    x
    y
)

A @rhombus(..., ~bind) works the way you would expect in a syntax
pattern, matching any @rhombus(..., ~bind)-replicated pattern variables
to form a repetition of matches:

@examples(
  ~defn:
    def '$x + $y ... + 0' = '1 + 2 + 3 + 0'
  ~repl:
    x
    [y, ...]
    '$y ...'
)

@margin_note_block{A tail pattern
 @rhombus(#,(@rhombus($, ~bind))#,(@rhombus(id, ~var)) #,(@rhombus(..., ~bind)))
 combined with a tail template
 @rhombus(#,(@rhombus($))#,(@rhombus(id, ~var)) ...)
 is similar to using @litchar{.} in
 S-expression patterns and templates, where it allows sharing between the
 input and output syntax objects. That sharing and an associated expansion-cost difference
 is all the more important in the Rhombus expansion protocol, which
 must thread potentially long sequences into and out of macro
 transformers.}

A @rhombus($)-escaped variable in a @quotes pattern matches one term
among other terms in the group. A block created with @litchar{:} counts
as a single term of its enclosing group, and a sequence of @litchar{|}
alternatives (not an individual alternative) similarly counts as one term.

@examples(
  ~defn:
    def '$x $y' = 'block: 1 2 3'
  ~repl:
    x
    y
  ~defn:
    def '$z $w' = 'cond | is_ok: "good" | ~else: "bad"'
  ~repl:
    z
    w
)

Keep in mind that @quotes
creates syntax objects containing shrubberies that are not yet parsed,
so a variable will @emph{not} be matched to a multi-term sequence that would be
parsed as an expression. For example, a pattern variable @rhombus(y) by
itself cannot be matched to a sequence @rhombus(2 + 3):

@examples(
  ~error:
    def '1 + $y + 4' = '1 + 2 + 3 + 4'
)

Having pattern variables always stand for individual terms turns out to
be tedious, however. For example, to match a @rhombus(thunk) pattern
with a block that has any number of groups with any number of terms,
you'd have to use two layers of ellipses. Then, to substitute that same
body into a @rhombus(fun) template, you'd have to use the two layers of
ellipses again.

@examples(
  ~eval:
    syntax_eval
  ~defn:
    def thunk_form = 'thunk:
                        def x = 1
                        x + 1'
    def 'thunk: $term ...; ...' = thunk_form
  ~repl:
    'fun (): $term ...; ...'
)

As a shorthand, when an escaped variable is alone in its group in a
pattern, it stands for a match to the whole group (at least by default).
A pattern variable that is alone in a multi-group context similarly
stands for a match to all the groups.

@examples(
  ~eval:
    syntax_eval
  ~defn:
    def 'thunk: $group; ...' = thunk_form
    def 'thunk: $body' = thunk_form
  ~repl:
    [group, ...]
    body
)

As a further generalization, when an escaped variable is at the end of
its group in a pattern, it stands for a match to remaining terms in group.

@examples(
  ~repl:
    def '1 + $y' = '1 + 2 + 3 + 4'
    y
)

These multi-term and multi-group syntax objects can be spliced into
similar positions in templates, where an escape is by itself within its
group or by itself in a multi-group position.

@examples(
  ~eval:
    syntax_eval
  ~repl:
    'fun (): $group; ...'
    'fun (): $body'
)

There is no constraint that the original and destination contexts have
the same shape, so a match from a block-like context can be put into a
brackets context, for example.

@examples(
  def '$x' = '1 + 2 + 3
              4 * 5 * 6'
  '[$x]'
)

A multi-term, single-group syntax object can be spliced in place of any
term escape, even if it is not at the end of the group.

@examples(
  ~repl:
    def '$x' = '1 + 2 + 3'
    '0 + $x + 4'
)

A multi-group syntax object splices multiple groups in place of a group
escape only when the escape is alone in its group. A list of group syntax
objects does not splice into group contexts, because that would create
ambiguities among group and term contexts. Meanwhile, a single-term
syntax object can be used as a group syntax object, a single-group
syntax object can be used as a multi-group syntax object, and a
single-term syntax object can be used as a multi-group syntax object.

Sometimes, a pattern variable that is at the end of a group is meant to
match a single term and not a group of terms. To match a single term in
a group context, annotate the pattern variable with the
@rhombus(Term, ~stxclass) syntax class using the @rhombus(::) operator.

@examples(
  ~repl:
    def '$(x :: Term)' = '1'
    x
  ~repl:
    ~error:
      def '$(x :: Term)' = '1 + 2'
)

You can similarly use the @rhombus(Group, ~stxclass) syntax class to
match a single group instead of a multi-group sequence. There are
several other predefined syntax classes, such as @rhombus(Identifier, ~stxclass)
to match an identifier, @rhombus(String, ~stxclass) to match a string
literal, and @rhombus(Int, ~stxclass) to match an integer literal.

In practice, you should use @rhombus(Block, ~stxclass) to match a
block, preserving its lexical context for the implicit
@rhombus(#%body, ~datum) form (see @secref("implicit")).

@examples(
  ~eval: syntax_eval
  ~defn:
    def 'thunk: $(body :: Block)' = thunk_form
  ~repl:
    'fun () $body'
)


@(close_eval(syntax_eval))
