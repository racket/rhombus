#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dollar = @rhombus($))

@title{Simple Expression Macros}

@doc(
  ~nonterminal:
    right_parsed_id: macro parsed_id ~defn
    left_parsed_id: macro parsed_id ~defn

  defn.macro 'macro $macro_case'
  defn.macro 'macro
              | $macro_case
              | ...'
  defn.macro 'macro $op_or_id_name:
                $option; ...
              | $macro_case
              | ...'

  expr.macro 'macro $macro_case'
  expr.macro 'macro
              | $macro_case
              | ...'

  entry_point.macro 'macro $macro_case'
  entry_point.macro 'macro
                     | $macro_case
                     | ...'

  grammar macro_case:
    $macro_pattern:
      $option; ...
      '$template'

  grammar macro_pattern:
    '$defined_name $ $right_parsed_id'
    '$defined_name $pattern ...'
    '$ $left_parsed_id $defined_name $ $right_parsed_id'
    '$ $left_parsed_id $defined_name $pattern ...'
  grammar defined_name:
    $id
    $op
    #,(dollar)('#,(dollar)')
    ($id_name)
    ($op_name)
    ()
  grammar parsed_id:
    $id
    ($parsed_id)
  grammar option:
    ~stronger_than $other ...
    ~stronger_than: $other ...; ...
    ~weaker_than $other ...
    ~weaker_than: $other ...; ...
    ~same_as $other ...
    ~same_as: $other ...; ...
    ~same_on_left_as $other ...
    ~same_on_left_as: $other ...; ...
    ~same_on_right_as $other ...
    ~same_on_right_as: $other ...; ...
    ~associativity $assoc
    ~associativity: $assoc
    ~op_stx $id
    ~op_stx: $id
    ~all_stx $id
    ~all_stx: $id
  grammar other:
    $id
    $op
    ~other
  grammar assoc:
    ~left
    ~right
    ~none
){

 As a definition form, @rhombus(macro) defines the @rhombus(defined_name)
 (which is an operator or identifier) within @rhombus(macro_pattern) as a
 pattern-based macro whose expansion is described by a
 @rhombus(template). When @rhombus(defined_name) is a plain
 @rhombus(op), it cannot be @rhombus($), but the form
 @rhombus($('$')) can be used to define @rhombus($). A
 @rhombus(defined_name) cannot be @rhombus(()) for a @rhombus(macro)
 definition. The @rhombus(defined_name) is bound in the
 @rhombus(expr, ~space) @tech{space}.

 As an expression or @tech{entry point}, @rhombus(macro) is a shorthand
 for a function that expects a syntax object to match, as explained
 @elemref("macro-entry-point"){further below}. The @rhombus(expr.macro)
 form is similar to @rhombus(macro); it allows more general compile-time
 code, but it also requires that @rhombusmodname(rhombus/meta) is
 imported.

 For a macro defined with @rhombus(macro), @rhombus(macro_pattern) is
 matched to a sequence within a group. Within the @quotes of each
 @rhombus(macro_pattern), either the first term is a
 @rhombus(defined_name) to be defined as a prefix macro, or the first
 term is a single @rhombus($, ~bind) escape followed by a
 @rhombus(defined_name) to be defined as an infix macro.

 In the case of an infix macro, the left-hand @rhombus($, ~bind) escape
 must be an identifier. It stands for a match to preceding terms that
 have been parsed as an expression, and the identifier is bound to an
 opaque representation of the expression. The right-hand side of the
 macro (for either a prefix or infix form) can be either
 @rhombus($, ~bind) followed by an identifier or an arbitrary pattern:

@itemlist(

 @item{When the right-hand side is @rhombus($, ~bind) followed by an
  identifier (optionally parenthesized), then the macro is applied with an
  already-parsed term after the macro name in a use of the macro. That
  parse heeds precedence and associativity declarations for other macros
  and for operators defined with @rhombus(op).}

 @item{Otherwise, the right-hand side is an arbitrary pattern that is
  matched to a sequence of terms after the macro name in its enclosing
  group. Unless the pattern ends with @rhombus(#,(@rhombus($, ~bind))()),
  the use of the macro can be followed by additional terms in the same
  group. If the pattern ends with @rhombus(#,(@rhombus($, ~bind))()), then
  all terms after the macro operator must match the right-hand pattern.
  The position before @rhombus(#,(@rhombus($, ~bind))()) is itself treated
  as a group position.}

)

 Using @vbar alternatives, a single definition can have any number of
 @rhombus(macro_pattern)s. The patterns describe any number of prefix and
 infix variants that are (presumably) distinguished by patterns that are
 tried in order. The name to define must be the same across all
 @rhombus(macro_pattern)s. If the right-hand side in a
 @rhombus(macro_pattern) of an infix or prefix form implies a parsed
 match, then it must be the only infix or prefix @rhombus(macro_pattern)
 among the alternatives.

 The body after each @rhombus(macro_pattern) must be an immediate
 @quotes template, and any @rhombus($) escape within the template
 can only refer to an input pattern variable or a literal syntax
 object, optionally parenthesized, or an operator (e.g.,
 @rhombus($('$')) to generate a literal @rhombus($)). More general
 compile-time expressions are not allowed; use @rhombus(expr.macro)
 or @rhombus(expr.macro), instead, to enable compile-time expressions.

 Before the @rhombus(template) body of a @rhombus(macro_pattern),
 @rhombus(option) keywords can appear. The options
 @rhombus(~weaker_than), @rhombus(~stronger_than), @rhombus(~same_as),
 @rhombus(~same_on_left_as), and @rhombus(~same_on_right_as) declare
 an name's precedence relative to other names, where @rhombus(~other)
 stands for any operator not otherwise mentioned. The
 @rhombus(~associativity) option is allowed only with an infix
 @rhombus(macro_pattern). The @rhombus(~op_stx) option binds an
 identifier to an identifier or operator syntax object as it appears
 in a use of the macro (which cannot be
 matched directly in the @rhombus(macro_pattern), since that position
 is used for the name that @rhombus(expr.macro) binds).
 The @rhombus(~all_stx) option binds an
 identifier to the input that is matched by the @rhombus(macro_pattern),
 which includes the identifier or operator that  @rhombus(~op_stx)
 would capture.
 In a definition with @litchar{|} alternatives,
 most @rhombus(option)s are allowed only in the first case, but
 @rhombus(~op_stx) and @rhombus(~all_stx) are allowed and separate in each case.

 When multiple cases are written with @vbar, they can appear after a block
 that supplies @rhombus(option)s that apply to all
 cases, the same as in @rhombus(operator).

@examples(
  ~defn:
    macro 'thunk: $(body :: Block)':
      'fun () $body'
  ~repl:
    thunk: "ok"
    (thunk: "ok")()
)

 An @elemtag("macro-entry-point"){expression or entry-point}
 @rhombus(macro) describes a function that takes a syntax object and
 returns two syntax objects: the results produced by
 @rhombus(template) and the tail of an enclosing group that follows the
 match to @rhombus(pattern). The @rhombus(defined_name) position must be
 @rhombus(()), the pattern form must be a prefix (not infix) pattern,
 and the only allowed @rhombus(option)s are @rhombus(~op_stx) and
 @rhombus(~all_stx). For the purpose of matching a
 syntax object passed to the function produced by @rhombus(macro), the
 leading @rhombus(()) in each pattern is replaced by @rhombus(_, ~bind).
 In addition, unless the pattern ends with @rhombus(#,(@rhombus($, ~bind))())
 or @rhombus($#,(@rhombus(id, ~var)) ..., ~bind), a @rhombus($tail ..., ~bind) pattern
 is added to the end; and the result
 values are the syntax object produced by @rhombus(template) and
 @rhombus('$tail ...').

@examples(
  ~defn:
    def converter:
      macro '() [$x, ...]':
        'handle_list([$x, ...])'
  ~repl:
    converter('ListLike[1, 2, 3].more')
)

 The @rhombus(macro) form does not define an @tech{assignment operator}
 that works with mutable targets. To define an assignment operator, use
 @rhombus(assign.macro), instead.

}
