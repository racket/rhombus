#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title(~tag: "stxobj"){Syntax Objects}

A @deftech{syntax object} encapsulates a shrubbery term, group, or
multi-group sequence with binding scopes and other metadata on
individual terms, and metadata potentially on individual syntax objects. See
@shrubref("top") for information on shrubbery notation, and specifically
@shrubref("parsed-rep") for information on representing shrubbery terms as
Rhombus values. The @rhombus(Syntax.make) function takes such a value
and wraps it as a syntax object, so that it can accumulate binding
scopes or hold other metadata, and functions like @rhombus(Syntax.unwrap)
expose that structure.

In addition to normal shrubbery structure, a syntax object can contain
@deftech{parsed} terms, which are opaque. The meaning and internal
structure of a parsed term depends on the parser that produced it. In
the case of parsing a syntax object as a Rhombus expression via
@rhombus(expr_meta.Parsed, ~stxclass), a parsed term encapsulates a
Racket expression. Pattern matching and functions like
@rhombus(Syntax.unwrap) treat parsed terms as opaque.

A quoted sequence of terms using @quotes is parsed as an
implicit use of the @rhombus(#%quotes) form, which is normally
bound to create a syntax object. For example, @rhombus('1.000')
is a syntax object that wraps the number @rhombus(1.0).

Metadata for a syntax object can include a source location and the raw
source text for a term, such as @rhombus("1.000") for a @rhombus(1.0)
that was written originally as @litchar{1.000}. Raw-source metadata
is used when printing a syntax error for a syntax object. Besides the
main text of a term, metadata can include a prefix string and/or
suffix string, which is used when printing a sequence of terms to
reflect the original layout. A group syntax object internally starts
with a @tt{group} tag that normally contains only prefix and suffix
text, leaving the group elements to supply their own text forms.
Finally, a syntax object can contain a tail string or and/or a tail
suffix; those normally appear only on a tag at the start of a syntax
object that represents a pair of parentheses, brackets, braces or
quotes, where the tail string corresponds to the closer, and the tail
suffix corresponds to text after the closer.

A syntax object that results from a match using a @tech{syntax class}
annotation has have fields in addition to the methods of all syntax
objects. If a field from a syntax class has the same name as a
@rhombus(Syntax) method, the field takes precedence for dynamic access
and for static access using @rhombus(Syntax.matched_of, ~annot) with the syntax
class's name.

@doc(
  ~also_meta
  expr.macro '«#%quotes '$term ...; ...'»'
  repet.macro '«#%quotes '$term ...; ...'»'
){

 Constructs a syntax object. When a single @rhombus(term) is present,
 the result is a single-term syntax object. When a single
 @rhombus(term ...) group is present with multiple @rhombus(term)s,
 the result is a group syntax object. The general case is a
 multi-group syntax object.

 @see_implicit(@rhombus(#%quotes), @quotes, "expression")

@examples(
  '1'
  'pi'
  '1 + 2'
  '1 + 2
   3 + 4'
)

 A @rhombus($) as a @rhombus(term, ~var) unquotes (i.e., escapes) the expression
 afterward; the value of that expression replaces the @rhombus($) term and expression. The value
 is normally a syntax object, but except for lists, other kinds of values are coerced
 to a syntax object via @rhombus(Syntax.make). Nested @quotes forms are allowed around
 @rhombus($) and do @emph{not} change whether the @rhombus($) escapes.

@examples(
  'x $(if #true | 'y' | 'why') z'
  'x $(1 + 2) z'
  '« x '$(1 + 2)' z »'
)

 The result of the expression after @rhombus($) can be a list, in which
 case and the elements of the list are spliced as terms in place of the
 @rhombus($) term and expression within the enclosing group. If the result
 is a syntax object, it can be a single-term syntax object or a group
 syntax object; in the latter case, the group terms are spliced in place
 of the escape.

@examples(
  'x $[1, 2, 3] z'
  'x $('1 2 3') z'
)

 Similarly, when an @rhombus($) escape is alone within its enclosing group, then the
 result of the expression after @rhombus($) can be a multi-group syntax object,
 in which case the group sequence is spliced in place of the escape.

@examples(
  'x; $('1; 2 3; 4'); z'
)

 A @dots_expr as a @rhombus(term, ~var) must follow a
 @rhombus(term, ~var) that includes at least one escape, and each of those
 escapes must contain a @tech{repetition} instead of an expression. The
 preceding term is replaced as many times as the repetition supplies
 values, where each value is inserted or spliced into the enclosing sequence.

@examples(
  def [x, ...] = [1, 2, 3]
  '(1 + $x) ...'
  '0 $('+ $x') ...'
  '0 $['+', x] ...'
)

 Multiple escapes can appear in the term before @dots_expr, in which the
 repetitions are drawn in parallel (assuming that they are at the same
 repetition depth), repetition @dots_expr can be nested around escapes,
 consecutive @dots_expr splice deeper repetitions, and
 so on, following the normal rules of @tech{repetitions}.

 Quotes work as a repetition to construct multiple syntax objects within
 another kind of repetition context, such as forming a list. All escapes
 must then be repetitions, instead of just expressions, and the depth of
 the repetition is the amount of repetition depth left over from the
 deepest escape.

@examples(
  def [[x, ...], ...] = [[1, 2, 3], [4], [5, 6]]
  ['[$x, ...]', ...]
)

}

@doc(
  ~also_meta
  bind.macro '«#%quotes '$term ...; ...'»'
){

 Matches a syntax object consistent with @rhombus(term, ~var)s.
 Identifiers and operators are matched symbolically (unrelated to
 binding), and other atomic terms are matched using @rhombus(==) on
 unwrapped syntax objects.

 A @rhombus($, ~bind) as a @rhombus(term)
 escapes to a subsequent unquoted binding that is matched against the corresponding
 portion of a candidate syntax object.
 A @dots in @rhombus(term, ~var) following a subpattern matches any number
 of instances of the preceding subpattern, and escapes in the pattern
 are bound as @tech{repetitions}. A @rhombus(#,(dots) ~nonempty) following a subpattern
 matches one or more instances, instead of zero or more instances. A
 @rhombus(#,(dots) ~once) following a subpattern
 matches zero instances or one instance. Multiple
 @dots can appear within a sequence; when matching
 is ambiguous, matching prefers earlier @dots repetitions to
 later ones.

@examples(
  match '1 + 2'
  | '$n + $m': [n, m]
  match '(1 / 1) (2 / 1) (3 / 1)'
  | '($x/1) ...': [x, ...]
  match '1 + 2 * 3'
  | '$x ... * 3': [x, ...]
  match '1 + 2 * 3'
  | '$x ... * $y ...': values([x, ...], [y, ...])
  match '1 + 2 * 3'
  | '$x ... ~nonempty $y ... ~nonempty': values([x, ...], [y, ...])
  match '1 ! = 3'
  | '$x $(n && '!') ... ~once = $y': [x, [n, ...], y]
  match '1 = 3'
  | '$x $(n && '!') ... ~once = $y': [x, [n, ...], y]
)

 Each @rhombus($, ~bind) escape is in either a term, group, or
 multi-group context. A @rhombus($, ~bind) escape is in a term context if
 it is followed by another escape within the same group. A
 @rhombus($, ~bind) escape is a multi-group context when it is alone
 within its group and when the group is alone within its enclosing group
 sequence. All other escapes are in a group context. An escape may impose
 constraints more limiting than its context, such as using
 @rhombus(Term, ~stxclass) within an escape in a group context. Escaping
 to a group pattern in a term context is a syntax error, as is using a
 multi-group pattern in a group or term context. A sequence escape (such
 as a use of a @tech{syntax class} of kind @rhombus(~sequence)) can be
 used in a term context.

@examples(
  ~error:
    match '1 + 2'
    | '$(a :: Group) + $b': a
  match '1 + 2 + 3 + 4'
  | '$(a :: Sequence) + $b': a
  match '1 + 2 + 3'
  | '$a + $(b :: Group)': b
)

 A @rhombus($, ~bind) or @dots as the only @rhombus(term) matches
 each of those literally. To match @rhombus($, ~datum) or
 @rhombus(..., ~datum) literally within a larger sequence of @rhombus(term)s,
 use @rhombus($, ~bind) to escape to a nested pattern, such as
 @rhombus(#,(@rhombus($, ~bind))('#,(@rhombus($))')). Simialrly,
 to match a literal @rhombus(~nonempty) or @rhombus(~once) after a @dots repetition, use
 @rhombus(#,(@rhombus($, ~bind))('~nonempty')) or @rhombus(#,(@rhombus($, ~bind))('~once')).

@examples(
  match Syntax.literal '1 $ 2'
  | '$n $('$') $m': [n, m]
)

 To match identifier or operators based on binding instead of
 symbolically, use @rhombus($, ~bind) to escape, and then use
 @rhombus(bound_as, ~unquote_bind) within the escape.

 @see_implicit(@rhombus(#%quotes, ~bind), @quotes, "binding")

}

@doc(
  annot.macro 'Syntax'
  annot.macro 'Term'
  annot.macro 'Group'
  annot.macro 'Block'
  annot.macro 'TermSequence'
  annot.macro 'Identifier'
  annot.macro 'Operator'
  annot.macro 'Name'
  annot.macro 'IdentifierName'
){

 The @rhombus(Syntax, ~annot) annotation matches any syntax object,
 while the others match more specific forms of syntax objects:

@itemlist(

 @item{@rhombus(Term, ~annot) matches only a single-term syntax object.}

 @item{@rhombus(Group, ~annot) matches only a single-group syntax object.}

 @item{@rhombus(Block, ~annot) matches only a block (which is a
  single-term syntax object).}

 @item{@rhombus(TermSequence, ~annot) matches only a single-group
  syntax object or a multi-group sequence with zero groups.}

 @item{@rhombus(Identifier, ~annot) matches only an identifier (which is
  a single-term syntax object).}

 @item{@rhombus(Operator, ~annot) matches only an operator (which is
  a single-term syntax object).}

 @item{@rhombus(Name, ~annot) matches a syntax object that is an
  identifier, operator, or dotted multi-term group that fits the shape of
  an @nontermref(op_or_id_name).}

 @item{@rhombus(IdentifierName, ~annot) matches a syntax object that is an
  identifier or dotted multi-term group that fits the shape of
  an @nontermref(id_name).}


)

}


@doc(
  annot.macro 'Syntax.matched_of($name)'
){

 Satisfied by a @tech{syntax object} that was produced by a match to a
 syntax pattern with a @rhombus(::, ~unquote_bind) annotation and the
 syntax class @rhombus(name). The static information of
 @rhombus(Syntax.matched_of(name), ~annot) provides statically resolved
 access to fields declared by the syntax class, including fields that are
 repetitions.

@examples(
  ~defn:
    syntax_class ManyThenOne
    | '$a ... $b'
  ~defn:
    fun describe(mto :: Syntax.matched_of(ManyThenOne)):
      "matched " +& [mto.a, ...] +& " followed by " +& mto.b
  ~repl:
    def '$(mto :: ManyThenOne)' = '1 2 3 4'
    [mto.a, ...]
    describe(mto)
    ~error:
      dynamic(mto).a
    dynamic(mto).b
)

}


@doc(
  ~also_meta
  expr.macro '$ $expr'
){

 Only allowed within a @quotes expression form, escapes so that the value of
 @rhombus(expr) is used in place of the @rhombus($) form.

 The @rhombus(expr) must be either a single term or a sequence of
 @litchar{.}-separated identifiers. To escape only an identifier (or
 @litchar{.}-separated identifier sequence) with an unescaped @litchar{.}
 afterward, use parentheses around the identifier (or sequence).

}


@doc(
  ~also_meta

  bind.macro '$ $stx_pat_bind_term'

  grammar stx_pat_bind_term:
    $id
    #,(@rhombus(_, ~unquote_bind))
    ($ stx_bind)
    '$term ...; ...'
    $other_stx_bind_term

  grammar stx_bind:
    $stx_pat_bind_term
    $id #,(@rhombus(::, ~unquote_bind)) $syntax_class_spec
    $stx_bind #,(@rhombus(&&, ~unquote_bind)) $stx_bind
    $stx_bind #,(@rhombus(||, ~unquote_bind)) $stx_bind
    #,(@rhombus(!, ~unquote_bind)) $stx_bind
    #,(@rhombus(pattern, ~unquote_bind)) $pattern_spec
    $other_stx_bind
){

 Only allowed within a @rhombus('', ~bind) binding pattern, escapes to a
 unquoted binding pattern. Typically, the unquoted pattern has an
 @rhombus(id) that is not bound as a unquote binding
 operator; the @rhombus(id) is then bound to the corresponding portion
 of the syntax object that matches the @rhombus('', ~bind) form.

@examples(
  match '1 + 2 + 3'
  | '$x + $y + $z': [x, y, z]
)

 An @rhombus($, ~bind)@rhombus(id) escape matches a single term, a
 group, or a multi-group sequence, depending on its context. It matches a
 multi-group sequence only when the @rhombus($, ~bind)@rhombus(id) escape
 is alone within its group and the group is along within a block or
 @quotes form. Otherwise, the escape matches a group only when it is
 alone within its group. In all other contexts, a
 @rhombus($, ~bind)@rhombus(id) escape matches a single term. Beware that
 syntax patterns in @rhombus(macro) and similar forms treat certain
 @rhombus($, ~bind)@rhombus(id) escapes specially.

 A @rhombus(_, ~unquote_bind) as a syntax pattern binding
 matches any input, like an identifier does, but without binding an
 identifier.

@examples(
  match '1 + 2 + 3'
  | '$_ + $y + $_': y
)

 A parenthesized escape is the same as the escape itself. Parentheses
 are needed to use the @rhombus(::) operator, since an @rhombus($, ~bind)
 escape must be followed by a single term, and a use of the @rhombus(::)
 operator consistent of three terms.

@examples(
  match '1 + 2 + 3'
  | '$(_) + $((y)) + $(((_)))': y
  match '1 + 2 + 3'
  | '1 + $(y :: Term) + 3': y
)

 Empty parentheses as an escape, @rhombus(#,(@rhombus($, ~bind))()),
 serve as a group pattern that is only useful as a group tail, where it
 matches an empty tail. This escape is primarily intended for use with
 macro-definition forms like @rhombus(macro).

 An escape that contains a @(quotes)-quoted term matches the term as
 a nested syntax-object pattern. In a term context, a multi-term escape is
 spliced into the enclosing group. One
 use of a quoted escape is to match a literal @rhombus($, ~datum) or
 @rhombus(..., ~datum) so that it is not treated as an escape or binding
 repetition in an enclosing pattern.

@examples(
  ~repl:
    match '1 + 2 + 3'
    | '1 + $('2') + $c': c
    match '1 + 2 + 3'
    | '1 $('+ 2 +') $c': c
    match '1 + 2 + 3'
    | '$a $('+ $b') ...': [a, b, ...]
  ~repl:
    def dots = '...'
    dots
    match '1 + 2 + $dots'
    | '1 + $b + $('...')': b
)

 The @rhombus(::, ~unquote_bind) operator is used to associate a
 @tech{syntax class} with an identifier. See
 @rhombus(::, ~unquote_bind) for more information.

 The @rhombus(&&, ~unquote_bind), @rhombus(||, ~unquote_bind), and @rhombus(!, ~unquote_bind)
 operators combine matches. See @rhombus(&&, ~unquote_bind),
 @rhombus(||, ~unquote_bind), and @rhombus(!, ~unquote_bind) for more information.

 The @rhombus(pattern, ~unquote_bind) form is a shorthand for using
 @rhombus(::, ~unquote_bind) with an inline @rhombus(syntax_class) form.
 See @rhombus(pattern, ~unquote_bind) for more information.

 Other syntax pattern binding forms can be defined with
 @rhombus(unquote_bind.macro).

}

@doc(
  ~also_meta
  ~nonterminal:
    stx_bind: $ ~bind
  unquote_bind.macro '_'
  unquote_bind.macro '#%parens ($stx_bind)'
){

 For use within a @rhombus($, ~bind) escape within a syntax pattern. See
 @rhombus($, ~bind).

}

@doc(
  ~also_meta
  unquote_bind.macro '«#%quotes '$term ...; ...'»'
){

 For use within a @rhombus($, ~bind) escape for a nested binding
 pattern. See @rhombus($, ~bind).

}

@doc(
  ~nonterminal:
    stx_bind: def bind ~defn
  unquote_bind.macro '$stx_bind && $stx_bind'
  unquote_bind.macro '$stx_bind || $stx_bind'
  unquote_bind.macro '! $stx_bind'
){

 Unquote binding operators for use with @rhombus($, ~bind) that combine matches:
 @rhombus(&&, ~unquote_bind) matches whether its left- and right-hand
 bindings would both independently match,
 @rhombus(||, ~unquote_bind) matches when either its left- and
 right-hand binding (or both) would match, and
 @rhombus(!, ~unquote_bind) matches when its binding would not match.

 A @rhombus(&&, ~unquote_bind) binds all variables from its arguments,
 while @rhombus(||, ~unquote_bind) and @rhombus(!, ~unquote_bind) bind none of them.

 Independent matching for @rhombus(&&, ~unquote_bind) means that in a
 term context, combining a variable binding with a splicing multi-term
 binding will @emph{not} enable a multi-term splicing match for the
 variable; instead, the pattern will fail to match a multi-term splice.

 A @rhombus(!, ~unquote_bind) can only be used in a term context,
 negating a term binding.

@examples(
  def '$(a && '($_ $_ $_)') done' = '(1 2 3) done'
  a
  def '$('1' || '2') ...' = '1 2 2 1 1'
  def '$(b && '$_ $_ $_')' = '1 2 3' // b in group context
  b
  def '$(!'2') ...' = '1 3 5 7 9'
  def '$(!'($_)') ...' = '[] {}'
  ~error:
    def '$(b && '$_ $_ $_') $end' = '1 2 3 done' // b in term context
)

}

@doc(
  ~nonterminal:
    stx_bind: def bind ~defn
  unquote_bind.macro 'match.cut'
  unquote_bind.macro 'match.delimit $stx_bind'
  unquote_bind.macro 'match.commit $stx_bind'
){

 Unquote binding forms for use with @rhombus($, ~bind) that control
 backtracking search for a match:

@itemlist(

 @item{The @rhombus(match.cut, ~unquote_bind) form prevents backtracking
  in the case that pattern after the cut fails to match, and instead leads
  to an immediate match failure, which typically implies an immediate
  error.}

 @item{The @rhombus(match.delimit, ~unquote_bind) form delimits cuts
  within @rhombus(stx_bind), causing match failure there to backtrack as
  allowed outside the @rhombus(match.delimit, ~unquote_bind) form.}

 @item{The @rhombus(match.commit, ~unquote_bind) form causes the first
  found match to @rhombus(stx_bind) to be the only considered match,
  meaning that backtracking will not try alternative matches to
  @rhombus(stx_bind).}

)

 The @rhombus(match.cut) form can only appear within a term sequence
 pattern. When @rhombus(match.cut) is used within a pattern in a
 @tech{syntax class}, then the syntax class delimits the cut; that is,
 failure implies a non-match of the syntax class, and not necessarily a
 failure of a match context using the syntax class. When
 @rhombus(match.cut) appears within @rhombus(!, ~unquote_bind), the
 @rhombus(!, ~unquote_bind) operator delimits the cut, so that failure
 counts as success for the @rhombus(!, ~unquote_bind) form.

@examples(
  match '1 2'
  | '1 $match.cut 2': "ok"
  ~error:
    match '1 3'
    | '1 $match.cut 2': "ok"
    | '1 3': "does not get here"
  match '1 3'
  | '$(match.delimit '1 $match.cut 2')': "ok"
  | '1 3': "else"
  match '1 1 3'
  | '$(match.commit '1 ...') $x': x
  ~error:
    match '1 1 1'
    | '$(match.commit '1 ...') $x': x
)

}


@doc(
  ~also_meta
  ~nonterminal:
    arg_expr: block expr
    field_id: block id
    pattern_id: block id
    class_clause: syntax_class ~defn
    pattern_case: syntax_class ~defn

  unquote_bind.macro '$id :: $syntax_class_ref $maybe_args'
  unquote_bind.macro '$id :: $syntax_class_ref $maybe_args:
                        $field_expose
                        ...'

  grammar syntax_class_ref:
    $id
    (syntax_class:
       $class_clause
       ...
     | $pattern_case
     | ...)

  grammar maybe_args:
    ($arg, ...)
    #,(epsilon)

  grammar arg:
    $arg_expr
    $keyword: $arg_expr

  grammar field_expose:
    #,(@rhombus(open, ~impo))
    $field_id #,(@rhombus(as, ~impo)) $pattern_id; ...
    $field_id ....
){

 Unquote binding operator for use with @rhombus($, ~bind) that binds
 @rhombus(id) for a match to @rhombus(syntax_class).

 The @rhombus(syntax_class_ref) can be a predefined class such as
 @rhombus(Term, ~stxclass), @rhombus(Identifier, ~stxclass), or
 @rhombus(Group, ~stxclass), among others, it can be a class defined with
 @rhombus(syntax_class), or it can be an parenthesized inline
 @rhombus(syntax_class) form that omits the class name. A class defined
 with @rhombus(syntax_class) may expect arguments, which must be supplied
 after the syntax class name.

 The @rhombus(id) before @rhombus(::, ~unquote_bind) refers to
 the matched input, and it is a repetition if the syntax class has
 classification @rhombus(~sequence). The identifier can be combined with
 @rhombus(.) to access fields (if any) of the syntax class. If
 @rhombus(id) is @rhombus(_, ~unquote_bind), then it is not
 bound.

 A block supplied after @rhombus(syntax_class_ref) exposes fields of match
 as directly bound pattern identifier. For each
 @rhombus(field_id #,(@rhombus(as, ~impo)) pattern_id)
 that is supplied, then @rhombus(pattern_id) is bound directly to
 the to the named field's value. Supplying just an
 @rhombus(field_id) binds using the same identifier. Supplying
 @rhombus(open, ~impo) is a shorthand for listing every field to bind
 using its own name, and it cannot appear multiple times or be combined
 with expose clauses for individual fields.

@examples(
  ~defn:
    syntax_class Wrapped:
      kind: ~term
    | '($content)'
  ~repl:
    match '1 + (2) + 3'
    | '1 + $(y :: Wrapped) + 3': [y, y.content]
    match '1 + (2) + 3'
    | '1 + $(_ :: Wrapped: content) + 3': content
    match '1 + (2) + 3'
    | '1 + $(_ :: Wrapped: content as c) + 3': c
    match '1 + (2) + 3'
    | '1 + $(_ :: Wrapped: open) + 3': content
  ~repl:
    match '(hello there)'
    | '$(whole :: (syntax_class:
                     kind: ~term
                   | '($content)'))':
        [whole, whole.content]
)

}

@doc(
  ~nonterminal:
    class_clause: syntax_class ~defn
    pattern_case: syntax_class ~defn

  bind.macro 'pattern $maybe_id_open:
                $class_clause
                ...
              | $pattern_case
              | ...'
  unquote_bind.macro 'pattern $maybe_id_open:
                        $class_clause
                        ...
                      | $pattern_case
                      | ...'

  grammar maybe_id_open:
    $id ~open
    $id
    #,(epsilon)
){

 The @rhombus(pattern, ~unquote_bind) form acts as a shorthand for
 matching with an inline @rhombus(syntax_class) form. It has the same
 grammar as inline @rhombus(syntax_class), except that an @rhombus(id)
 can be present with the same meaning as in
 @rhombus(::, ~unquote_bind); moreover, when @rhombus(~open) is
 present, the match is @rhombus(open, ~impo)ed in the same sense as in
 @rhombus(::, ~unquote_bind). The @rhombus(id) can also be omitted, in
 which case the match isn't bound but all fields are available.

 When directly used in a binding context, @rhombus(pattern, ~bind)
 acts as a shorthand for a syntax pattern with the
 @rhombus(pattern, ~unquote_bind) form as the only term.

@examples(
  ~defn:
    fun simplify(e):
      match e
      | '($e)': simplify(e)
      | '0 + $e': simplify(e)
      | '$e + 0': simplify(e)
      | (pattern
         | '$a + $b - $c':
             match_when same(simplify(b), simplify(c))):
          simplify(a)
      | (pattern
         | '$a - $b + $c':
             match_when same(simplify(b), simplify(c))):
          simplify(a)
      | ~else: e
  ~defn:
    fun same(b, c):
      b.unwrap() == c.unwrap()
  ~repl:
    simplify('(1 + (2 + 0) - 2)')
    simplify('1 + 2 + 2')
)

}


@doc(
  ~nonterminal:
    syntax_pattern: #%quotes pattern
    pattern_body: syntax_class ~defn
    id_maybe_rep: field ~pattern_clause

  unquote_bind.macro 'group_option_sequence
                      | $pattern_case
                      | ...'
  unquote_bind.macro 'term_option_sequence
                      | $pattern_case
                      | ...'

  grammar pattern_case:
    syntax_pattern
    syntax_pattern:
      $option_pattern_body
      ...

  grammar option_pattern_body:
    pattern_body
    #,(@rhombus(default, ~pattern_clause)) $id_maybe_rep = $expr
    #,(@rhombus(default, ~pattern_clause)) $id_maybe_rep: $body; ...
    #,(@rhombus(description, ~pattern_clause)) $string
    #,(@rhombus(description, ~pattern_clause)): $string
){

 Creates a pattern that matches a sequence of groups in the case of
 @rhombus(group_option_sequence, ~unquote_bind) or term sequences in the
 case of @rhombus(term_option_sequence, ~unquote_bind). Each match in a
 sequence must match a distinct @rhombus(pattern_case); if multiple
 matches to one @rhombus(pattern_case) are found, an error is reported
 (i.e., the pattern does not merely fail to match). The result is similar
 to using @dots after a @rhombus(pattern, ~unquote_bind) with the same
 @rhombus(pattern_case)s, but since each
 @rhombus(pattern_case) matches at most once, it's fields do not
 turn into repetitions. Each individual @rhombus(syntax_pattern) is a
 group pattern within @rhombus(group_option_sequence, ~unquote_bind) or
 sequence pattern within @rhombus(group_option_sequence, ~unquote_bind).

 In a match that does not use a particular @rhombus(pattern_case), the
 pattern variables of that case are bound to either @rhombus(#false) or
 @rhombus([]) by default, the latter when the pattern variable is a
 repetition. A @rhombus(default, ~pattern_clause) clause within a
 @rhombus(pattern_case) can specify a different default; each
 @rhombus(id_maybe_rep) names a variable with its depth in the same way
 as for @rhombus(field, ~pattern_clause). The @rhombus(expr) or
 @rhombus(body) sequence within a @rhombus(default, ~pattern_clause)
 clause has the scope of the enclosing
 @rhombus(group_option_sequence, ~unquote_bind) or
 @rhombus(term_option_sequence, ~unquote_bind) form; it is not in the
 scope of definitions within the @rhombus(pattern_case)  body, because it is used
 for non-matches instead of matches.

 A @rhombus(description, ~pattern_clause) clause can provide a string
 that is used when multiple matches are found for the enclosing
 @rhombus(pattern_case). The string is expected to be a plural noun
 suitable to replace a generic ``options'' in an error message.

@examples(
  ~eval:
    macro.make_macro_eval()
  ~defn:
    expr.macro 'list_proc:
                  $(group_option_sequence
                    | '~min_args: $(min :: Int)':
                        default min = '1'
                    | '~max_args: $(max :: Int)')':
      'fun $(Syntax.make(
               [#'alts,
                & for List (i in min.unwrap() ..= (max || min).unwrap()):
                  let [var, ...] = for List (j in i): Syntax.make_temp_id()
                  ': ($var, ...): [$var, ...]']
             ))'
  ~repl:
    def f_2:
      list_proc:
        ~min_args: 2
    f_2(1, 2)
    ~error:
      f_2(1, 2, 3)
  ~repl:
    def f_2_3:
      list_proc:
        ~min_args: 2
        ~max_args: 3
    f_2_3(1, 2, 3)
  ~repl:
    ~error:
      list_proc:
        ~min_args: 2
        ~min_args: 3
)

}

@doc(
  expr.macro '«Syntax.literal '$term ...; ...'»'
  expr.macro 'Syntax.literal ($term ..., ...)'
){

 Similar to a plain @quotes form, but @rhombus($) escapes or
 @dots_expr repetitions are not
 recognized in the @rhombus(term)s, so that the @rhombus(term)s are
 all treated as literal terms to be quoted.

 There's no difference in result between using @quotes or
 @rhombus(()) after @rhombus(Syntax.literal)---only a difference in
 notation used to describe the syntax object, such as using @litchar{;}
 versus @litchar{,} to separate groups.

 Metadata, such as raw source text, is preserved for the @rhombus(term)
 sequence. Most @tech(~doc: model_doc){scopes} are also preserved, but
 the syntax object's @tech(~doc: model_doc){scope sets} are pruned to
 omit the scope for any binding form that appears between the
 @rhombus(Syntax.literal) form and the enclosing top-level context,
 module body, or @tech(~doc: model_doc){phase level} crossing, whichever
 is closer.

@examples(
  Syntax.literal 'x'
  Syntax.literal (x)
  Syntax.literal '1 ... 2'
  Syntax.literal '$ $ $'
)

}

@doc(
  expr.macro '«Syntax.literal_term '$term'»'
  expr.macro 'Syntax.literal_term ($term)'
  expr.macro '«Syntax.literal_group '$term ...'»'
  expr.macro 'Syntax.literal_group ($term ...)'
){

 Like @rhombus(Syntax.literal), but specialized to a single term or
 single group, which may be useful to statically ensure that the result
 is a term or group syntax object. No metadata is preserved for the group
 containing the term in the case of @rhombus(Syntax.literal_term), which
 can be a small but useful savings in some contexts.

}

@doc(
  expr.macro '«Syntax.literal_local '$term ...; ...'»'
  expr.macro 'Syntax.literal_local ($term ..., ...)'
  expr.macro '«Syntax.literal_local_term '$term'»'
  expr.macro 'Syntax.literal_local_term ($term)'
  expr.macro '«Syntax.literal_local_group '$term ...'»'
  expr.macro 'Syntax.literal_local_group ($term ...)'
){

 Like @rhombus(Syntax.literal_local), @rhombus(Syntax.literal_term), and
 @rhombus(Syntax.literal_group), but without pruning any
 @tech(~doc: model_doc){scopes}.

}

@doc(
  fun Syntax.make(term :: Any,
                  ctx_stx :: maybe(Term) = #false)
    :: Term
){

 Converts an ``unwrapped'' representation of a shrubbery @emph{term}
 into a syntax object. The unwrapped representation may include
 subforms that are already wrapped as syntax objects (including
 @rhombus(term) itself), a long as a syntax object that can be used as
 a term or group is used within a position that represents a term or
 group, respectively, and those syntax objects left as-is within the
 result. Newly-created syntax objects are given the same scopes
 as @rhombus(ctx_stx) if it is a syntax object, an empty set of scopes
 otherwise.

 Lists and other @tech{listable} values in @rhombus(term) are converted
 to compound shrubbery forms, such as a parenthesized term or a sequence
 of alternatives. Each listable value must start with a symbol that
 selects the compound form, such as @rhombus(#'parens) or
 @rhombus(#'alts).

 Alone or within nested listables, only certain kinds of atomic values
 can be converted to syntax: numbers, @tech{strings}, @tech{byte
  strings}, @tech{symbols}, @tech{keywords}, @tech{paths},
 @rhombus(Srcloc, ~annot) values, @rhombus(#true), @rhombus(#false), and
 @rhombus(#void). Mutable strings and byte strings are implicitly coerced
 to immutable variants. Other kinds of values can be converted to syntax
 using @rhombus(Syntax.inject), but with limitations on the use of the
 resulting syntax object.

@examples(
  Syntax.make(1.0)
  Syntax.make([#'parens, '1.0', '2', '"c"'])
  Syntax.make([#'alts, ': result1', ': result2'])
  ~error:
    Syntax.make(['1.0', '2', '"c"'])
)

}

@doc(
  fun Syntax.make_group(terms :: Listable.to_list && NonemptyList,
                        ctx_stx :: maybe(Term) = #false)
    :: Group
){

 Converts a nonempty list of terms, each convertible by @rhombus(Syntax.make),
 into a group syntax object. The @rhombus(ctx_stx) argument's scopes are used
 for new syntax objects, the same as in @rhombus(Syntax.make).

@examples(
  Syntax.make_group([1.0, 2, "c"])
  Syntax.make_group(['if', 'test', [#'alts, ': result1', ': result2']])
  ~error:
    Syntax.make_group(['1 2'])
)

}

@doc(
  fun Syntax.make_sequence(groups :: Listable,
                           ctx_stx :: maybe(Term) = #false)
    :: Syntax
){

 Converts a list of groups, each convertible by
 @rhombus(Syntax.make_group), into a multi-group syntax object.
 The @rhombus(ctx_stx) argument's scopes are used
 for new syntax objects, the same as in @rhombus(Syntax.make).

@examples(
  Syntax.make_sequence(['1 2 3', 'a b'])
)

}

@doc(
  fun Syntax.make_op(name :: Symbol,
                     ctx_stx :: maybe(Term) = #false)
    :: Operator
){

 Convenience to convert a plain symbol to a syntax object for an
 operator, equivalent to @rhombus(Syntax.make([#'op, name])).
 The @rhombus(ctx_stx) argument's scopes are used
 for the new syntax object, the same as in @rhombus(Syntax.make).

@examples(
  Syntax.make_op(#'#{+})
)

}

@doc(
  fun Syntax.make_id(str :: ReadableString,
                     ctx_stx :: maybe(Term) = #false)
    :: Identifier
){

 Composes @rhombus(Syntax.make) and @rhombus(Symbol.from_string).

@examples(
  Syntax.make_id("hello" +& 7, 'here')
)

}


@doc(
  fun Syntax.make_temp_id(name :: Any = #false,
                          ~keep_name: keep_name :: Any = #false)
    :: Identifier
){

 Creates an identifier with a fresh scope, which is useful for creating
 a temporary binding.

 Unless @rhombus(keep_name) is true, the @rhombus(name) argument can be
 any value, and the name of the generated identifier may be derived from
 @rhombus(name) for debugging purposes (especially if it is a string,
 symbol, or identifier). If @rhombus(keep_name) is true, the
 @rhombus(name) argument must be an identifier, symbol, or (readable)
 string, and the result identifier has exactly the given name.

@examples(
  Syntax.make_temp_id("hello")
  Syntax.make_temp_id("hello", ~keep_name: #true)
)

}


@doc(
  fun Syntax.inject(v :: Any,
                    ctx_stx :: maybe(Term) = #false)
    :: Term
){

 Similar to @rhombus(Syntax.make), and the result is the same as from
 @rhombus(Syntax.make) is @rhombus(v) is an allowed atomic value. Other
 values for @rhombus(v) are also treated as ``atomic'' value, even if
 @rhombus(v) a @tech{listable} value.

 The value @rhombus(v) can be recovered via @rhombus(Syntax.unwrap), but
 the syntax object produced by @rhombus(Syntax.inject) is not necessarily
 suitable for use as an expression or as a literal quoted term to be
 included in an expression, because it cannot necessarily be serialized
 in a compiled form. Phase-crossing via syntax quoting can also fail,
 because some values (including @rhombus(List, ~annot) values) have
 distinct run-time and compile-time representations. The intent of
 @rhombus(Syntax.inject) is to support arbitrary values in a syntax
 object that is constructed and used only within a phase, and especially
 at run time.

}


@doc(
  method Syntax.unwrap(stx :: Term) :: Any
){

 Unwraps a single-term syntax object by one layer. The result is a
 list of syntax objects in the case of an operator, parentheses,
 brackets, braces, block, or alternatives, where the first element of
 the list reflects the specific shape.

@examples(
  Syntax.unwrap('1.0')
  Syntax.unwrap('(a, "b", ~c)')
  Syntax.unwrap(': b; c')
  Syntax.unwrap('| a | b')
  ~error:
    Syntax.unwrap('1 2 3')
)

 In the case of a @tech{parsed} term, the result is just the parsed
 term, still as a syntax object. Similarly, in the case of a syntax
 object constructed at the Racket level that does not have the shape of a
 shrubbery form, the result is still the syntax object.

}


@doc(
  method Syntax.unwrap_op(stx :: Operator) :: Symbol
){

 Unwraps a syntax object containing a single operator, returning the
 operator's name as a symbol.

@examples(
  Syntax.unwrap_op('+')
)

}


@doc(
  method Syntax.unwrap_group(stx :: Group) :: List.of(Syntax)
){

 Unwraps a multi-term, single-group syntax object by one layer. The
 result is a list of term syntax objects.

 Following the usual coercion conventions, a term syntax object for
 @rhombus(stx) is acceptable as a group syntax object.

@examples(
  Syntax.unwrap_group('1.0')
  Syntax.unwrap_group('1 2 3')
  Syntax.unwrap_group('a: b; c')
  ~error:
    Syntax.unwrap_group('1; 2; 3')
)

}


@doc(
  method Syntax.unwrap_sequence(stx :: Syntax)
    :: List.of(Group)
){

 Unwraps a multi-group syntax object by one layer. The result is a
 list of group syntax objects.

 Following the usual coercion conventions, a term or group syntax
 object for @rhombus(stx) is acceptable as a multi-group syntax
 object.

@examples(
  Syntax.unwrap_sequence('1.0')
  Syntax.unwrap_sequence('1 2 3')
  Syntax.unwrap_sequence('1; 2; 3')
)

}


@doc(
  method Syntax.unwrap_all(stx :: Syntax) :: Any
){

 Unwraps a syntax object recursively, returning a value that does not
 contain a syntax object (except in limited circumstances) but could be
 passed to @rhombus(Syntax.make).

@examples(
  Syntax.unwrap_all('(1 + 2)')
)

 The result can contain a syntax object only for a @tech{parsed} term, a
 syntax object injected with @rhombus(Syntax.inject), or a syntax object
 that is created at the Racket level and does not represent a shrubbery
 form.

}

@doc(
  method Syntax.srcloc(stx :: Syntax) :: maybe(Srcloc)
){

 Returns the source location, if any, for @rhombus(stx). When
 @rhombus(stx) is not a single-term syntax object, a source location is
 constructed, if possible, from its content's locations.

}


@doc(
  method Syntax.is_original(stx :: Term) :: Boolean
){

 Reports whether the given syntax object is original in the sense that
 it was part of the original source of a program, and not introduced by a
 macro expansion. Originalness is determined by the presence of property
 that is added to the syntax object when it is originally read, plus the
 absence of scopes that indicate macro-introduction. See also
 @rhombus(syntax_meta.flip_introduce).

}


@doc(
  method Syntax.strip_scopes(stx :: Syntax) :: Syntax
){

 Returns a syntax object that is the same as @rhombus(stx), except
 that all binding scopes are removed.

}

@doc(
  method Syntax.replace_scopes(stx :: Syntax,
                               like_stx :: Term)
    :: Syntax
){

 Returns a syntax object that is the same as @rhombus(stx), except that
 scopes on all parts of @rhombus(stx) are changed to match the immediate
 scopes of @rhombus(like_stx).

}


@doc(
  method Syntax.name_to_symbol(stx :: Name) :: Symbol
){

 Unwraps a syntax object containing a single @rhombus(Name, ~annot),
 returning the operator's name as a symbol.

@examples(
  Syntax.name_to_symbol('apple')
  Syntax.name_to_symbol('+')
  Syntax.name_to_symbol('fruit.apple')
  Syntax.name_to_symbol('fruit.(++)')
)

}


@doc(
  method Syntax.relocate(stx :: Term,
                         to :: maybe(Term || Srcloc))
    :: Term
  method Syntax.relocate_group(stx :: Group,
                               to :: maybe(Group || Srcloc))
    :: Group
){

 Returns a syntax object like @rhombus(stx), except that the metadata of
 @rhombus(to) replaces metadata in @rhombus(stx) when @rhombus(to) is a
 syntax object, or just the source location is changed to match
 @rhombus(to) when it is a @rhombus(Srcloc, ~annot). When @rhombus(to) is
 @rhombus(#false), then metadata is removed from @rhombus(stx).

 Syntax-object metadata exists at both term and group layers, and it
 exists separately at each layer for a group that contains a single term.
 The @rhombus(Syntax.relocate) method uses and adjusts term-level
 metadata, while @rhombus(Syntax.relocate_group) method uses and adjusts
 group-level metadata. A group does not have a source location
 independent of its content, so @rhombus(Syntax.relocate_group) does not
 accept a @rhombus(Srcloc, ~annot) as @rhombus(to).

 When a term is a parenthesis, brackets, braces, quotes, block or
 alternatives form, then metadata is specifically associated with the
 leading tag in the underlying representation of the form. In the case of
 a single-term operator, metadata is taken from the operator token, not
 the @tt{op} tag. For a group syntax object, metadata is associated
 @tt{group} tag in its underlying representation.

 See also @rhombus(Syntax.property) and @rhombus(Syntax.group_property)
 for accessing or updating specific properties with in metadata.

}


@doc(
  method Syntax.relocate_span(
    stx :: Term,
    like_stxes :: Listable.to_list && List.of(Syntax)
  ) :: Term
  method Syntax.relocate_group_span(
    stx :: Group,
    like_stxes :: Listable.to_list && List.of(Syntax)
  ) :: Group
  method Syntax.relocate_ephemeral_span(
    stx :: Syntax,
    like_stxes :: Listable.to_list && List.of(Syntax),
  ) :: Syntax
){

 Similar to @rhombus(Syntax.relocate) and
 @rhombus(Syntax.relocate_group), but the metadata of syntax objects in
 @rhombus(like_stxes) is merged to replace the metadata of @rhombus(stx).
 Merging combines raw source text in sequence, and it combines compatible
 source locations to describe a region containing all of the locations.

 The @rhombus(Syntax.relocate_ephemeral_span) function accepts any
 syntax object, which can be a term, group, or multi-group sequence. It
 attaches metadata to the syntax object in way that may get lost if the
 syntax object is deconstructed or adjusted in any way. This mode is
 intended for communicating source information from a macro expansion in
 the case that it cannot be inferred automatically.

 All three functions add an immediate, ephemeral @rhombus(#'relocated)
 syntax property to the result syntax object, which overrides any default
 automatic relocation, such as by @rhombus(expr.macro, ~defn).

}

@doc(
  fun Syntax.relocate_split(
    [stx :: Term, ...] :: NonemptyList,
    like_stx :: Syntax
  ) :: List.of(Term)
){

 Roughly the opposite of @rhombus(Syntax.relocate_span): takes the
 overall source location and raw text of @rhombus(like_stx) and spreads
 it over a sequence of @rhombus(stx) terms. A raw-text prefix on
 @rhombus(like_stx), if any, is attached to the first @rhombus(stx), and
 the main content and suffix is attached to the last @rhombus(stx).

}

@doc(
  method Syntax.property(stx :: Term,
                         key :: Any)
    :: Any
  method Syntax.property(stx :: Term,
                         key :: Any, val :: Any,
                         is_preserved :: Any = #false)
    :: Term
){

 Returns the value of the @rhombus(key) syntax property of @rhombus(stx)
 or returns a syntax object with the property set to @rhombus(val). When
 @rhombus(val) is supplied, the property value is preserved in a compiled
 quoted form of the syntax object only when @rhombus(is_preserved) is
 true.

}


@doc(
  method Syntax.group_property(stx :: Group,
                               key :: Any)
    :: Any
  method Syntax.group_property(stx :: Group,
                               key :: Any, val :: Any,
                               is_preserved :: Any = #false)
    :: Group
){

 Like @rhombus(Syntax.property), but for properties on a group syntax
 object, instead of a term.

}

@doc(
  method Syntax.to_source_string(stx :: Syntax,
                                 ~keep_prefix: keep_prefix = #false,
                                 ~keep_suffix: keep_suffix = #false,
                                 ~as_inner: as_inner = #true)
    :: String
){

 Converts to a string with content similar to @rhombus(print) of
 @rhombus(stx) in @rhombus(#'text) mode, but using source text as available through
 @rhombus(#'raw) and related properties attached to @rhombus(stx).

 A raw-text prefix or suffix is preserved in the result only when
 @rhombus(keep_prefix) or @rhombus(keep_suffix) is true, respectively. If
 @rhombus(as_inner) is true, then an ``inner'' prefix or suffix is
 preserved independent of @rhombus(keep_prefix) or @rhombus(keep_suffix);
 typically, an inner prefix corresponds to @litchar("@") to start
 at-expression notation before a term.

}

@doc(
  method Syntax.source_properties(syntax :: Term)
    :: values(Any, Any, Any, Any)
  method Syntax.source_properties(syntax :: Term,
                                  prefix, content, tail, suffix)
    :: Term
  method Syntax.group_source_properties(syntax :: Group)
    :: values(Any, Any, Any, Any)
  method Syntax.group_source_properties(syntax :: Group,
                                        prefix, content, tail, suffix)
    :: Group
){

 Wrappers on @rhombus(Syntax.property) and
 @rhombus(Syntax.group_property) that query or install source text
 information on a syntax object. Source text is broken into four parts: a
 prefix (typically whitespace and comments), content (or content prefix
 for a contain such as a group or parenthesized syntax object), tail
 (i.e., content suffix, such as a closing parenthesis), and suffix
 (typically whitespace and comments).

 Source is text is represented as a tree built of
 @rhombus(Pair, ~annot)s, @rhombus(PairList.empty), and strings, where
 the in-order concatenation of the string forms the source text.

}
