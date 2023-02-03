#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@(def dots = @rhombus(...))
@(def dots_bind = @rhombus(..., ~bind))

@title(~tag: "stxobj"){Syntax Objects}

A @deftech{syntax object} encapsulates a shrubbery term, group, or
 multi-group sequence with binding scopes and other metadata on
 individual terms, and metadata potentially on individual syntax.cls. See
 @secref(~doc: [#'lib, "shrubbery/scribblings/shrubbery.scrbl"], "top")
 for information on shrubbery notation, and specifically
 @secref(~doc: [#'lib, "shrubbery/scribblings/shrubbery.scrbl"], "parsed-rep")
 for information on representing shrubbery terms as
 Rhombus values. The @rhombus(Syntax.make) function takes such a value
 and wraps it as a syntax object, so that it can accumulate binding
 scopes or hold other metadata.

An quoted sequence of terms using @quotes is parsed as an
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

@dispatch_table(
  "syntax-object"
  @rhombus(Syntax)
  [stx.unwrap(), Syntax.unwrap(stx)]
  [stx.unwrap_group(), Syntax.unwrap_group(stx)]
  [stx.unwrap_sequence(), Syntax.unwrap_sequence(stx)]
  [stx.strip(), Syntax.strip(stx)]
  [stx.relocate(like_stx), Syntax.relocate(stx, like_stx)]
  [stx.relocate_span(like_stxes), Syntax.relocate(stx, like_stxes)]
)

@doc(
  expr.macro '«#%quotes '$term ...; ...'»'
  repet.macro '«#%quotes '$term ...; ...'»'
){

@provided_also_meta()

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

 A @rhombus($) as a @rhombus(term,~var) unquotes (i.e., escapes) the expression
 afteward; the value of that expression replaces the @rhombus($) term and expression. The value
 is normally a syntax object, but except for lists, other kinds of values are coerced
 to a syntax object. Nested @quotes forms are allowed around
 @rhombus($) and do @emph{not} change whether the @rhombus($) escapes.

@examples(
  'x $(if #true | 'y' | 'why') z'
  'x $(1 + 2) z'
  '« x '$(1 + 2)' z »'
)

 The result of the expression after @rhombus($) can be a list, in which
 case and the elements of the list are spliced in place of the
 @rhombus($) term and expression witin the enclosing group. If the result
 is a syntax object, it can be a single-term syntax object or a group
 syntax object; in the latter case, the group terms are spliced in place
 of the escape.

@examples(
  'x $[1, 2, 3] z'
  'x $('1 2 3') z'
)

 A @dots as a @rhombus(term,~var) must follow a
 @rhombus(term,~var) that includes at least one escape, and each of those
 escapes must contain a @tech{repetition} instead of an expression. The
 preceding term is replaced as many times as the repetition supplies
 values, where each value is inserted or spliced into the enclosing sequence.

@examples(
  def [x, ...] = [1, 2, 3]
  '(1 + $x) ...'
  '0 $('+ $x') ...'
  '0 $['+', x] ...'
)

 Multiple escapes can appear in the term before @dots, in which the
 repetitions are drawn in parallel (assuming that they are at the same
 repetition depth), repetition @dots can be nested around escapes,
 consecutive @dots splice deeper repetitions, and
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
  bind.macro '«#%quotes '$term ...; ...'»'
){

@provided_also_meta()

 Matches a syntax object consistent with @rhombus(term, ~var)s.
 Identifiers and operators are matched symbolically (unrelatd to
 binding), and other atomic terms are matched using @rhombus(==) on
 unwrapped syntax objects.

 A @rhombus($, ~bind) within @rhombus(term)
 escapes to a subsequent unquoted binding that is matched against the corresponding
 portion of a candidate syntax object.
 A @dots_bind in @rhombus(term,~var) following a subpattern matches any number
 of instances of the preceding subpattern, and escapes in the pattern
 are bound as @tech{repetitions}. Unlike binding forms such as @rhombus(List),
 @dots_bind can appear before the end of a sequence, and
 multiple @dots_bind can be used in the same group; when matching
 is ambiguous, matching prefers earlier @dots_bind repetitions to
 later ones.

 A @rhombus($, ~bind) or @dots_bind as the only @rhombus(term) matches
 each of those literally. To match @rhombus($, ~datum) or
 @rhombus(..., ~datum) literally within a larger sequence of @rhombus(term)s,
 use @rhombus($, ~bind) to escape to a nested pattern, such as
 @rhombus(#,(@rhombus($, ~bind))('#,(@rhombus($))')).

 To match identifier or operators based on binding instead of
 symbolically, use @rhombus($, ~bind) to escape, and then use
 @rhombus(bound_as, ~unquote_bind) within the escape.

 @see_implicit(@rhombus(#%quotes, ~bind), @quotes, "binding")

@examples(
  match '1 + 2'
  | '$n + $m': [n, m]
  match '(1/1) (2/1) (3/1)'
  | '($x/1) ...': [x, ...]
  match '1 + 2 * 3'
  | '$x ... * 3': [x, ...]
  match '1 + 2 * 3'
  | '$x ... * $y ...': values([x, ...], [y, ...])
)

}

@doc(
  annot.macro 'Syntax'
){

  Matches syntax objects.

}

@doc(
  annot.macro 'Identifier'
){

  Matches a syntax object that contains a single identifier term.

}

@doc(
  annot.macro 'Operator'
){

  Matches a syntax object that contains a single operator term.

}

@doc(
  expr.macro '$ $expr'
){

@provided_also_meta()

 Only allowed within a @quotes expression form, escapes so that the value of
 @rhombus(expr) is used in place of the @rhombus($) form.

 The @rhombus(expr) must be either a single term or a sequence of
 @rhombus(.)-separated identifiers. To escape only an identifier (or
 @rhombus(.)-separated identifier sequence) with an unescaped @rhombus(.)
 afterward, use parentheses around the identifier (or sequence).

}


@doc(
  bind.macro '$ $stx_pat_bind_term'

  grammar stx_pat_bind_term:
    $identifier
    #,(@rhombus(_, ~unquote_bind))
    ($ stx_bind)
    '$term ...; ...'
    $other_stx_bind_term

  grammar stx_bind:
    $stx_pat_bind_term
    $identifier #,(@rhombus(::, ~unquote_bind)) $syntax_class_spec
    $stx_bind #,(@rhombus(&&, ~unquote_bind)) $stx_bind
    $stx_bind #,(@rhombus(||, ~unquote_bind)) $stx_bind
    #,(@rhombus(pattern, ~unquote_bind)) $pattern_cases
    $other_stx_bind
){

@provided_also_meta()

 Only allowed within a @rhombus('', ~bind) binding pattern, escapes to a
 unquoted binding pattern. Typically, the unquoted pattern has an
 @rhombus(identifier) that is not bound as a unquote binding
 oerator; the @rhombus(identifier) is then bound to the corresponding portion
 of the syntax object that matches the @rhombus('', ~bind) form.

@examples(
  match '1 + 2 + 3'
  | '$x + $y + $z': [x, y, z]
)

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
 matches an empty tail. This escape is primariy intended for use with
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

 The @rhombus(&&, ~unquote_bind) and @rhombus(||, ~unquote_bind)
 operators combine matches. See @rhombus(&&, ~unquote_bind) and
 @rhombus(||, ~unquote_bind) for more information.

 The @rhombus(pattern, ~unquote_bind) form is a shorthand for using
 @rhombus(::, ~unquote_bind) with an inline @rhombus(syntax_class) form.
 See @rhombus(pattern, ~unquote_bind) for more information.

 Other syntax pattern binding forms can be defined with
 @rhombus(unquote_bind.macro).

}

@doc(
  unquote_bind.macro '_'
  unquote_bind.macro '#%parens ($stx_bind)'
){

@provided_also_meta()

 For use within a @rhombus($, ~bind) escape within a syntax pattern. See
 @rhombus($, ~bind).

}

@doc(
  unquote_bind.macro '«#%quotes '$term ...; ...'»'
){

@provided_also_meta()

 For use within a @rhombus($, ~bind) escape for a nested binding
 pattern. See @rhombus($, ~bind).

}

@doc(
  unquote_bind.macro '$stx_bind && $stx_bind'
  unquote_bind.macro '$stx_bind || $stx_bind'
){


 Unquote binding operators for use with @rhombus($, ~bind) that combine matches:
 @rhombus(&&, ~unquote_bind) matches whether its left- and right-hand
 bindings would both independetly match, and
 @rhombus(||, ~unquote_bind) matches when either its left- and
 right-hand binding (or both) would match.

 A @rhombus(&&, ~unquote_bind) binds all variables from its arguments,
 while @rhombus(||, ~unquote_bind) binds none of them.

 Independent matching for @rhombus(&&, ~unquote_bind) means that in a
 term context, combinding a variable binding with a splcing multi-term
 binding will @emph{not} enable a multi-term splicing match for the
 variable; instead, the pattern will fail to match a multi-term splice.

@examples(
  def '$(a && '($_ $_ $_)') done' = '(1 2 3) done'
  a
  def '$('1' || '2') ...' = '1 2 2 1 1'
  def '$(b && '$_ $_ $_')' = '1 2 3' // b in group context
  b
  ~error:
    def '$(b && '$_ $_ $_') done' = '1 2 3 done' // b in term context
)

}


@doc(
  unquote_bind.macro '$identifier :: $syntax_class $maybe_args'
  unquote_bind.macro '$identifier :: $syntax_class $maybe_args:
                        $field_expose
                        ...'

  grammar syntax_class:
    $identifier
    (syntax_class | $pattern_case | ...)
    (syntax_class: $class_clause; ...)

  grammar maybe_args:
    ($arg, ...)
    #,(epsilon)

  grammar arg:
    $arg_expr
    $keyword: $arg_expr

  grammar field_expose:
    #,(@rhombus(open, ~impo))
    $field_identifier #,(@rhombus(as, ~impo)) $pattern_identifier; ...
    $field_identifier ....
){

@provided_also_meta()

 Unquote binding operator for use with @rhombus($, ~bind) that binds
 @rhombus(identifier) for a match to @rhombus(syntax_class).

 The @rhombus(syntax_class) can be a predefined class such as
 @rhombus(Term, ~stxclass), @rhombus(Id, ~stxclass), or
 @rhombus(Group, ~stxclass), among others, it can be a class defined with
 @rhombus(syntax_class), or it can be an parenthesized inline
 @rhombus(syntax_class) form that omits the class name. A class defined
 with @rhombus(syntax_class) may expect arguments, which must be supplied
 after the syntax class name.

 The @rhombus(identifier) before @rhombus(::, ~unquote_bind) refers to
 the matched input, and it is a repetition if the syntax class has
 classification @rhombus(~sequence). The identifier can be combined with
 @rhombus(.) to access fields (if any) of the syntax class. If
 @rhombus(identifier) is @rhombus(_, ~unquote_bind), then it is not
 bound.

 A block supplied after @rhombus(syntax_class) exposes fields of match
 as directly bound pattern identifier. For each
 @rhombus(field_identifier #,(@rhombus(as, ~impo)) pattern_identifier)
 that is supplied, then @rhombus(pattern_identifier) is bound directly to
 the to the named field's value. Suppling just an
 @rhombus(field_identifier) binds using the same identifier. Supplying
 @rhombus(open, ~impo) is a shorthand for listing every field to bind
 using its own name, and it cannot appear multiple times or be combined
 with expose clauses for individual fields.

@examples(
  ~defn:
    syntax_class Wrapped:
      kind: ~term
      pattern '($content)'
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
                     pattern '($content)'))':
        [whole, whole.content]
)

}

@doc(
  unquote_bind.macro 'pattern $pattern_cases'
){

 Unquote binding operator for use with @rhombus($, ~bind) that is like
 the @rhombus(pattern, ~bind) binding form---which, in turn, has the
 same syntax and matching rules as a
 @rhombus(pattern, ~syntax_class_clause) form in @rhombus(syntax_class).
 See @rhombus(pattern, ~bind).

}

@doc(
  unquote_bind.macro '«bound_as $space: '$identifier_or_operator'»'
){

 Unquote binding operator for use with @rhombus($, ~bind). It matches a
 syntax object for an identifier or operator, where the identifier or
 operator's binding is the same as @rhombus(identifier_or_operator) in
 the @tech{space} identified by @rhombus(space) (e.g.,
 @rhombus(expr, ~space)).

@examples(
  ~defn:
    import:
      rhombus/meta open
      rhombus:
        rename: + as plus
        expose: plus
  ~defn:
    fun simplify(stx):
      match stx
      | '$(a :: Integer) $(bound_as expr: '+') $(b :: Integer)':
          '$(a.unwrap() + b.unwrap())'
      | ~else:
          stx
  ~repl:
    simplify('1 + 2')
    simplify('1 plus 2')
    simplify('1 * 2')
)

}



@doc(
  expr.macro '«Syntax.literal '$term ...; ...'»'
  expr.macro 'Syntax.literal ($term ..., ...)'
){

 Similar to a plain @quotes form, but @rhombus($) escapes or
 @dots repetitions are not
 recognized in the @rhombus(term)s, so that the @rhombus(term)s are
 all treated as literal terms to be quoted.

 There's no difference in result between using @quotes or
 @rhombus(()) after @rhombus(literal_syntax)---only a difference in
 notation used to describe the syntax object, such as using @litchar{;}
 versus @litchar{,} to separate groups.

 Metadata, such as raw source text, is preserved for the
 @rhombus(term) sequence, but not any metadat that might be on the
 group as a whole when the @rhombus(term)s form a single group.

@examples(
  Syntax.literal 'x'
  Syntax.literal (x)
  Syntax.literal '1 ... 2'
  Syntax.literal '$ $ $'
)

}

@doc(
  expr.macro '«Syntax.literal_group '$term ...'»'
  expr.macro 'Syntax.literal_group ($term ...)'
){

 Similar to a plain @quotes form that has multiple terms in one
 group, but like @rhombus(Syntax.literal) in that there are no
 escapes. Unlike @rhombus(Syntax.literal), the @rhombus(term)s must
 form a single group, and the result is always a group syntax object.

 Metadata, such as raw source text, is preserved for the
 @rhombus(term) sequence, but not any metadata that might be on the
 group as a whole when the @rhombus(term)s form a single group.

}

@doc(
  fun Syntax.make(term) :: Syntax
){

 Converts an ``unwrapped'' representation of a shrubbery @emph{term}
 into a syntax object. The unwrapped representation may include
 subforms that are already wrapped as syntax objects (including
 @rhombus(term) itself), a long as a syntax object that can be used as
 a term or group is used within a position that represents a term or
 group, respectively, and those syntax objects left as-is within the
 result.

@examples(
  Syntax.make(1.0)
  Syntax.make([#'parens, '1.0', '2', '"c"'])
  Syntax.make([#'alts, ': result1', ': result2'])
  ~error: Syntax.make(['1.0', '2', '"c"'])
)

}

@doc(
  fun Syntax.make_group(terms :: List) :: Syntax
){

 Converts a nonempty list of terms, each convertible by @rhombus(Syntax.make),
 into a group syntax object.

@examples(
  Syntax.make_group([1.0, 2, "c"])
  Syntax.make_group(['if', 'test', [#'alts, ': result1', ': result2']])
  ~error: Syntax.make_group(['1 2'])
)

}

@doc(
  fun Syntax.make_sequence(groups :: List) :: Syntax
){

 Converts a list of groups, each convertible by
 @rhombus(Syntax.make_group), into a multi-group syntax object.

@examples(
  Syntax.make_sequence(['1 2 3', 'a b'])
)

}

@doc(
  fun Syntax.unwrap(stx :: Syntax)
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
  ~error: Syntax.unwrap('1 2 3')
)

}

@doc(
  fun Syntax.unwrap_group(stx :: Syntax) :: List.of(Syntax)
){

 Unwraps a multi-term, single-group syntax object by one layer. The
 result is a list of term syntax objects.

 Following the usual coercion conventions, a term syntax object for
 @rhombus(stx) is acceptable as a group syntax object.

@examples(
  Syntax.unwrap_group('1.0')
  Syntax.unwrap_group('1 2 3')
  Syntax.unwrap_group('a: b; c')
  ~error: Syntax.unwrap_group('1; 2; 3')
)

}

@doc(
  fun Syntax.unwrap_sequence(stx :: Syntax) :: List.of(Syntax)
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
  fun Syntax.strip(stx :: Syntax) :: Syntax
){

 Returns a syntax object that is the same as @rhombus(stx), except
 that all binding scopes are removed.

}

@doc(
  fun Syntax.relocate(stx :: Syntax, like_stx :: Syntax) :: Syntax
){

 Returns a syntax object like @rhombus(stx), except that the metadata
 of @rhombus(like_stx) replaces metadata in @rhombus(stx).

 The specific source of metadata from @rhombus(like_stx) depends on
 its shape. If it is a single-term parenthesis, brackets, braces,
 quotes, block or alternatives form, then metadata is taken from the
 leading tag in the representation of the form. In the case of a
 single-term operator, metadata is taken from the operator token, not
 the @tt{op} tag. In the case of a group syntax object, metadata is
 taken from the @tt{group} tag.

 In the same way, metadata is applied to @rhombus(stx) based on its
 shape. Transferring metadata thus makes the most sense when
 @rhombus(stx) and @rhombus(like_stx) have the same shape.

}


@doc(
  fun Syntax.relocate_span(stx :: Syntax,
                           like_stxes :: List.of(Syntax)) :: Syntax
){

 Similar to @rhombus(Syntax.relocate), but that the metadata of syntax
 objects in @rhombus(like_stxes) are merged to replace the metadata of
 @rhombus(stx). Merging combines raw source text in sequence, and it
 combines compatible source locations to describe a region containing
 all of the locations.

}
