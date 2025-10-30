#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@(def macro_meta_eval = make_rhombus_eval())
@examples(
  ~eval: macro_meta_eval
  ~hidden:
    import:
      meta -1:
        rhombus/meta open
)

@(def dollar = @rhombus($))

@title{Expression Macros}

@doc(
  space.enforest expr
){

 The @tech{space} for bindings of identifiers and operators that can be
 used in expression, definition, and declaration positions.

}


@doc(
  ~meta
  def expr_meta.space :: SpaceMeta
){

 A compile-time value that identifies the same space as
 @rhombus(expr, ~space). See also @rhombus(SpaceMeta, ~annot).

}


@doc(
  ~nonterminal:
    macro_pattern: macro ~defn
    option: macro ~defn

  defn.macro 'expr.macro $macro_patterns'

  grammar macro_patterns:
    $macro_case
    Z| $macro_case
     | ...
    $op_or_id_name:
      $option; ...
    | $macro_case
    | ...

  grammar macro_case:
    $macro_pattern:
      $option; ...
      $body
      ...
){

 Like @rhombus(macro), but arbitrary compile-time code can appear in the
 body after each @rhombus(macro_pattern).

 If a @rhombus(macro_pattern) ends with
 @rhombus(#,(@rhombus($, ~bind))()),
 @rhombus(#,(@rhombus($, ~bind))#,(@rhombus(id, ~var)) #,(@rhombus(..., ~bind))),
 a block pattern, or an alternatives pattern, then a match
 covers all subsequent terms in the enclosing group of a use of the
 macro. In that case, the @rhombus(body) after the pattern can return two
 values: an expansion for the consumed part of the input match, and a
 tail for the unconsumed part. Returning a single value corresponds to
 returning an empty tail.

 The result of @rhombus(body) is ultimately relocated using
 @rhombus(Syntax.relocate_ephemeral_span) when a single value is returned
 from the macro @rhombus(body); the relocation is applied after the result
 is fully expanded. Returning a tail (i.e., a second result)
 for unconsumed terms obliges and enables a macro to perform its own relocation for
 its expansion (i.e., the first result). See also @secref("stxobj-track").

 The @rhombus(expr.macro) form does not define an @tech{assignment operator}
 that works with mutable targets. To define an assignment operator, use
 @rhombus(assign.macro), instead.

}


@doc(
  ~meta
  syntax_class expr_meta.Parsed:
    kind: ~group
    fields:
      group
  syntax_class expr_meta.AfterPrefixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class expr_meta.AfterInfixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class expr_meta.NameStart:
    kind: ~group
    fields:
      name
      [head, ...]
      [tail, ...]
){

 Syntax classes that match by parsing expressions. The value of the
 binding in each case is an opaque syntax object that represents the @tech(~doc: ref_doc){parsed}
 expression form, while the @rhombus(group) field holds a syntax object
 for the original terms that were parsed.

 The @rhombus(expr_meta.AfterPrefixParsed, ~stxclass) and
 @rhombus(expr_meta.AfterInfixParsed, ~stxclass) syntax classes expect an operator
 name that is bound as a prefix or infix operator, respectively. Parsing
 precedes as if immediately after the given operator---stopping when an
 infix operator of weaker precedence is encountered, for example. The
 result is in pattern variable's value plus a @rhombus(tail) field
 that contains the remaining unparsed input.

@examples(
  ~eval: macro_eval
  ~defn:
    :
      // an infix `no_fail` that works without a right-hand side
      // expression, and that has weak precedence
      expr.macro no_fail:
        ~weaker_than: ~other
      | '$left no_fail $()':
          'try: $left; ~catch _: #false'
      | '$left no_fail $(right :: expr_meta.AfterInfixParsed('no_fail')) $()':
           values('try: $left; ~catch _: $right', '$right.tail ...')
  ~repl:
    1/0 no_fail
    1/0 no_fail 0
    1/0 no_fail 0 + 1
    1+0 no_fail 0 + 1
    (1+0 no_fail 0) + 1
)

 The @rhombus(expr_meta.NameStart, ~stxclass) syntax class matches a
 group that starts with a name, potentially in dotted form. It converts an
 operator or dotted name into an identifier with the same binding;
 that identifier or an original one is the @rhombus(name, ~datum) field. The
 @rhombus(head, ~datum) field contains the terms that were combined to
 form @rhombus(name, ~datum), and the @rhombus(tail, ~datum) field
 contains the remaining terms of the group.

}


@doc(
  ~meta
  fun expr_meta.parse_all(group :: Syntax) :: (Syntax, Syntax)
){

 Parses @rhombus(group) as an expression and forces expansion
 of all nested forms. Avoid this function, and use it only when control
 over expansion order is absolutely necessary.

 Two results are returned: the expanded expression in its normal opaque
 form, and the expanded expression in an form that can be reused only in
 the current macro's expansion context but without a further expansion
 traversal (which may be necessary to avoid exponential expansion work).

}


@doc(
  ~meta
  fun expr_meta.parse_dot(left :: Syntax, op_and_tail :: Group,
                          ~as_static: as_static = #false,
                          ~disable_generic: disable_generic = #false)
    :: (maybe(Syntax), maybe(Syntax))
){

 Dispatches to a @tech(~doc: guide_doc){dot provider} in the same way as
 @rhombus(.). The @rhombus(left) syntax object must represent a parsed
 expression to the left of @rhombus(.), while @rhombus(op_and_tail)
 contains the operator and rest of the group to parse. The result is
 two values: a parsed expression that consumes @rhombus(left) and part of
 @rhombus(op_and_tail), and an unparsed remainder of
 @rhombus(op_and_tail). The result can be @rhombus(#false) and
 @rhombus(#false) only when @rhombus(disable_generic) is a true value.

 To select a specific dot provider, use @rhombus(statinfo_meta.replace)
 to adjust the static information of @rhombus(left), where
 @rhombus(statinfo_meta.dot_provider_key) is the relevant
 static-information key.

 The @rhombus(as_static) argument determines whether a dot provider is
 considered in static or dynamic mode, independent of the mode that would
 be selected by the operator at the start of @rhombus(op_and_tail).

 If no dot provider is found or if a dot provider declines to provide a
 conversion, then a generic fallback expression is produced unless
 @rhombus(disable_generic) is a true value. Note that a resolution
 failure (when no dot provider is found) is an error in static mode only
 when @rhombus(disable_generic) is @rhombus(#false).

}


@doc(
  ~meta
  fun expr_meta.pack_s_exp(tree :: Any) :: Syntax
){

 Converts a tree of terms, which can include @tech(~doc: ref_doc){parsed} terms, into a
 new parsed term representing a Racket parenthesized form. The intent is
 that the result is a Racket form to be used in a Rhombus context.

 A ``tree'' is a syntax object or a list of trees (i.e., syntax objects
 and arbitrary nestings of them within lists). Each syntax object must
 contain a single term.

 Any @tech(~doc: ref_doc){parsed} form as a leaf of @rhombus(tree) is exposed directly
 in the parenthesized sequence, which enables the construction of parsed
 terms that combine other parsed terms.

@examples(
  ~eval: macro_meta_eval
  expr_meta.pack_s_exp(['lambda', ['x'], 'x'])
  expr_meta.pack_s_exp(['lambda', ['x'], expr_meta.pack_expr('x + 1')])
)

}

@doc(
  ~meta
  fun expr_meta.pack_expr(group :: Syntax) :: Syntax
){

 Converts a syntax object, which can be a multi-term syntax object, into
 an @tech(~doc: ref_doc){parsed} term, but one that represents a run-time expression with
 delayed parsing. The function is intended for use in combination with
 @rhombus(expr_meta.pack_s_exp).

@examples(
  ~eval: macro_meta_eval
  expr_meta.pack_expr('x + 1')
)

}


@doc(
  ~meta
  fun expr_meta.pack_meta_expr(group :: Syntax) :: Syntax
  fun expr_meta.pack_and_meta_expr(group :: Syntax) :: Syntax
){

 Like @rhombus(expr_meta.pack_expr), but for an expression to be used in
 a compile-time position or @emph{both} a run-time and compile-time
 position.

 Although there are cases where @rhombus(expr_meta.pack_and_meta_expr)
 is the right choice, either @rhombus(expr_meta.pack_expr) or
 @rhombus(expr_meta.pack_meta_expr) is almost always more appropriate.

}

@doc(
  fun expr_meta.relative_precedence(
    left_mode :: matching(#'prefix || #'infix),
    left_op :: Name,
    right_infix_op :: Name
  ) :: matching(#'weaker || #'stronger || #false)
  fun expr_meta.ends_parse(
    left_mode :: matching(#'prefix || #'infix),
    left_op :: Name,
    tail :: Group
  ) :: Boolean
){

 The @rhombus(expr_meta.relative_precedence) function reports relative
 precedence between two expression operators, where the left operator may
 be prefix or infix, but the latter is always infix. Note that
 associativity for an operator can be inferred through a precedence
 comparison with itself.

 The @rhombus(expr_meta.ends_parse) function determines whether parsing
 for the argument of @rhombus(left_op) would end before @rhombus(tail).
 The end of a parse can be determined by inspecting @rhombus(tail) and
 using @rhombus(expr_meta.relative_precedence), but there are many cases
 to consider, including an empty tail and a tail that triggers implicit
 bindings; those cases are handled consistently by
 @rhombus(expr_meta.ends_parse).

}

@(macro.close_eval(macro_eval))
@(macro.close_eval(macro_meta_eval))
