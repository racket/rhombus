#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def syntax_eval = make_rhombus_eval())

@title(~tag: "syntax"){Syntax Objects}

The @quotes form produces a @deftech{syntax object}. The syntax object holds
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
like @rhombus($), when it's the only thing in the term). When @rhombus(...) immediately
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
works the way you'd expect it to work.

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

A @rhombus($)-escaped variable in a @quotes pattern matches one term in
with a group, or it matches a whole group if the a @rhombus($)-escaped
variable is alone within its group.

@examples(
  ~defn:
    def '$x $y' = '1 2'
  ~repl:
    x
    y
  ~repl:
    ~error:
      def '$x $y' = '1 2 3'
  ~defn:
    def '$x' = '1 2 3'
  ~repl:
    x
)

A block created with @litchar{:} counts as a
single term of its enclosing group, and a sequence of @litchar{|}
alternatives (not an individual alternative) similarly counts as one
term.

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
so single-term escape will @emph{not} be matched to a multi-term sequence that would be
parsed as an expression. For example, a pattern variable @rhombus(y)
cannot be matched to a sequence @rhombus(2 * 3) if there's another escape
after @rhombus($y).

@examples(
  ~error:
    def '1 + $y' = '1 + 2 * 3'
)

In the same way that an escaped variable alone in its group matches the
whole group as a sequence of terms, a pattern variable that is alone in
a multi-group context matches a sequence of groups. This rule applies
only for multi-group contexts where groups are not separated by
@litchar{,}, such as in a block.

@examples(
  ~eval:
    syntax_eval
  ~defn:
    def thunk_form = 'thunk:
                        def x = 1
                        x + 1'
  ~defn:
    def 'thunk: $group; ...' = thunk_form
    def 'thunk: $body' = thunk_form
  ~repl:
    [group, ...]
    body
)

Multi-term and multi-group syntax objects can be spliced into templates
where a single term or single group is expected. A list of terms can
similarly splice into a context where a single term is expected.

@examples(
  ~eval:
    syntax_eval
  ~repl:
    'fun (): $group; ...'
    'fun (): $body'
  ~repl:
    def '$x' = '1 + 2 + 3'
    '0 + $x + $x + 4'
  ~repl:
    '0 + $(['1', '+', '2', '+', '3']) + 4'
)

A list of group syntax objects does not splice into a group context,
because that would create ambiguities among group and term contexts;
instead, the list of group syntax objects must be converted to a
multi-group syntax object for splicing when that is the intent.
Meanwhile, a single-term syntax object can be used as a group syntax
object, a single-group syntax object can be used as a multi-group syntax
object, and a single-term syntax object can be used as a multi-group
syntax object.

For a multi-group match and template splice, there is no constraint that
the original and destination contexts have the same shape, so a match
from a block-like context can be put into a brackets context, for
example.

@examples(
  def '$x' = '1 + 2 + 3
              4 * 5 * 6'
  '[$x]'
)

Although a @rhombus($) escape followed by variable in a @quotes pattern
normally matches only one term, a variable can be annotated with the
@rhombus(TermSequence, ~stxclass) syntax class to match multiple terms.
Sequence matches can be ambiguous, in which case ambiguity is resolved
by eagerly matching terms to earlier escapes.

@examples(
  ~defn:
    def '$(x :: Sequence) + $(y :: Sequence)' = '1 + 2 * 3'
  ~repl:
    x
    y
  ~defn:
    def '$(x :: Sequence) + $(y :: Sequence)' = '0 + 1 + 2 * 3'
  ~repl:
    x
    y
)

The @rhombus(Group, ~stxclass) syntax class is similar to
@rhombus(Sequence, ~stxclass), except that it matches only non-empty
term sequences, and it is allowed only in specific positions that do not
create ambiguity: places within a group with no escapes afterward. There
are several other predefined syntax classes, such as
@rhombus(Identifier, ~stxclass) to match an identifier,
@rhombus(String, ~stxclass) to match a string literal, and
@rhombus(Int, ~stxclass) to match an integer literal.

Use @rhombus(Block, ~stxclass) to match the full content block, but also
keep the enclosing @colon (preserve its source location and raw text),
so that the bound identifier is a block as a single term instead of the
block body as a sequence of groups.

@examples(
  ~eval: syntax_eval
  ~defn:
    def 'thunk: $(body :: Block)' = thunk_form
  ~repl:
    'fun () $body'
)


@(close_eval(syntax_eval))
