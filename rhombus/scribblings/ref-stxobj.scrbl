#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@(def dots = @rhombus(...))

@title(~tag: "stxobj"){Syntax Objects}

A @deftech{syntax object} encapsulates a shrubbery term, group, or
 multi-group sequence with binding scopes and other metadata on
 individual terms, and metadata potentially on individual groups. See
 @secref(~doc: [symbol(lib), "shrubbery/scribblings/shrubbery.scrbl"], "top")
 for information on shrubbery notation, and specifically
 @secref(~doc: [symbol(lib), "shrubbery/scribblings/shrubbery.scrbl"], "parsed-rep")
 for information on representing shrubbery terms as
 Rhombus values. The @rhombus(Syntax.make) function takes such a value
 and wraps it as a syntax object, so that it can accumulate binding
 scopes or hold other metadata.

An quoted sequence of terms using @rhombus('') is parsed as an
 implicit use of the @rhombus(#{#%quotes}) form, which is normally
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
  expr.macro '«#{#%quotes} '$term ...; ...'»'
  repet.macro '«#{#%quotes} '$term ...; ...'»'
){

 Constructs a syntax object. When a single @rhombus(term) is present,
 the result is a single-term syntax object. When a single
 @rhombus(term ...) group is present with multiple @rhombus(term)s,
 the result is a group syntax object. The general case is a
 multi-group syntax object.

 @see_implicit(@rhombus(#{#%quotes}), @rhombus(''), "expression")

@examples(
  '1'
  'pi'
  '1 + 2'
  '1 + 2
   3 + 4'
)

 A @rhombus($) as a @rhombus(term,~var) escapes a following expression
 whose value replaces the @rhombus($) term and expression. The value
 is normally a syntax objects, but other kinds of values are coerced
 to a syntax object. Nested @rhombus('') forms are allowed around
 @rhombus($) and do @emph{not} change whether the @rhombus($) escapes.

@examples(
  'x $(if #true | 'y' | 'why') z'
  'x $(1 + 2) z'
  '« x '$(1 + 2)' z »'
)

 A @dots as a @rhombus(term,~var) must follow a
 @rhombus(term,~var) that includes at least one escape, and each of those
 escapes must contain a @tech{repetition} instead of an expression. The
 preceding term is replaced as many times as the repetition supplies
 values.

@examples(
  def [x, ...] = [1, 2, 3]
  '(1 + $x) ...'
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
  bind.macro '«#{#%quotes} '$term ...; ...'»'
){

 Matches a syntax object consistent with @rhombus(term,~var)s. A
 @rhombus($, ~bind) within @rhombus(form) escapes to an binding that
 is matched against the corresponding portion of a candidate syntax
 object. A @rhombus(..., ~bind) following a subpattern matches any number
 of instances of the preceding subpattern, and escapes in the pattern
 are bound as @tech{repetitions}. Unlike binding forms such as @rhombus(List),
 @rhombus(..., ~bind) can appear before the end of a sequence, and
 multiple @rhombus(..., ~bind) can be used in the same group; when matching
 is ambiguous, matching prefers earlier @rhombus(..., ~bind) repetitions to
 later ones.

 @see_implicit(@rhombus(#{#%quotes}, ~bind), @rhombus(''), "binding")

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
  expr.macro '$ $expr'
){

 Only allowed within a @rhombus('') form, escapes so that the value of
 @rhombus(expr) is used in place of the @rhombus($) form.

}


@doc(
  bind.macro '$ $identifier'
  bind.macro '$ ($identifier :: $syntax_class)'
){

 Only allowed within a @rhombus('', ~bind) binding pattern, escapes so that
 @rhombus(identifier) is bound to the corresponding portion of the syntax
 object that matches the @rhombus('', ~bind) form. If @rhombus(identifier)
 is @rhombus(_), then no identifier is bond to matching syntax.

 The @rhombus(syntax_class) can be @rhombus(Term, ~stxclass), @rhombus(Id, ~stxclass),
 or @rhombus(Group, ~stxclass), among other built-in classes, or it can be a class defined
 with @rhombus(syntax.class).

}

@doc(
  syntax.class Term
  syntax.class Id
  syntax.class Op
  syntax.class Id_Op
  syntax.class Keyw
  syntax.class Group
  syntax.class Multi
  syntax.class Block
){

 Syntax classes, all of which imply a single-term match except for
 @rhombus(Group, ~stxclass), @rhombus(Multi, ~stxclass), and
 @rhombus(Block, ~stxclass).

 The @rhombus(Group, ~stxclass) syntax class can be used only for a
 pattern identifier that is the sole term of its group in a pattern. The
 identifier is bound to a match for the entire group as a group syntax
 object.

 The @rhombus(Multi, ~stxclass) syntax class can be used only for a
 pattern identifier that is the sole term where a sequence of groups is
 allowed, such as in the body of a block. The identifier is bound to a
 match for the entire sequence of groups.

 The @rhombus(Block, ~stxclass) syntax class can be used only for a
 pattern identifier that is the sole term of a block. The identifier is
 bound to a match for the entire block as a single term (i.e., as a
 single-term syntax object that has a block term, and not as a
 multi-group syntax object).

}


@doc(
  expr.macro '«Syntax.literal '$term ...; ...'»'
  expr.macro 'Syntax.literal ($term ..., ...)'
){

 Similar to a plain @rhombus('') form, but @rhombus($) escapes or
 @dots repetitions are not
 recognized in the @rhombus(term)s, so that the @rhombus(term)s are
 all treated as literal terms to be quoted.

 There's no difference in result between using @rhombus('') or
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

 Similar to a plain @rhombus('') form that has multiple terms in one
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
  Syntax.make([symbol'parens', '1.0', '2', '"c"'])
  Syntax.make([symbol'alts', ': result1', ': result2'])
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
  Syntax.make_group(['if', 'test', [symbol'alts', ': result1', ': result2']])
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
