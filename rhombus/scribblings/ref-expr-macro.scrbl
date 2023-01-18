#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Expression Macros}

@doc(
  defn.macro '«expr.rule $rule_pattern:
                 $option; ...
                 '$template'»'
  defn.macro '«expr.rule
               | $rule_pattern:
                   $option; ...
                   '$template'
               | ...»'
  grammar rule_pattern:
    '$defined_name $pattern ...'
    '$defined_name #,(dollar)(~parsed $right_identifier)'
    '$ $left_pattern $defined_name $pattern ...'
    '$ $left_pattern $defined_name #,(dollar)(~parsed right_identifier)'
  grammar defined_name:
    $identifier
    $operator
    #,(dollar)('#,(dollar)')
    ($identifier_path)
    ($operator_path)
  grammar left_pattern:
    identifier
    (~parsed $identifier)
  grammar option:
    ~stronger_than: $identifier_or_operator ...
    ~weaker_than: $identifier_or_operator ...
    ~same_as: $identifier_or_operator ...
    ~same_on_left_as: $identifier_or_operator ...
    ~same_on_right_as: $identifier_or_operator ...
    ~associativity: $assoc
    ~op_stx: $identifier
  grammar assoc:
    ~left
    ~right
    ~none
){

 Defines the name (which is an operator or identifier) within
 @rhombus(defined_name) as a pattern-based macro whose expansion is
 described by a @rhombus(template) that can refer to pattern variables
 bound in the @rhombus(rule_pattern). The @rhombus(operator) within
 @rhombus(defined_name) cannot be @rhombus($), but the form
 @rhombus($('$')) can be used to define @rhombus($).

 A @rhombus(rule_pattern) is matched to a portion of its enclosing
 group. Within the @rhombus('') of each @rhombus(rule_pattern), either
 the first term is the name to be defined as a prefix macro, or the first
 term is a single @rhombus($, ~bind) escape followed by the name to be
 defined as an infix macro.

 In the case of a prefix macro, the left-hand @rhombus($, ~bind) escape
 can be an identifier or one that is wrapped with parentheses and
 @rhombus(~parsed). Declaring @rhombus(~parsed) is redundant, because the
 left-hand side must always be parsed to discover an infix operator, but
 it is allowed for cosistency with the right-hand side. The right-hand
 side of the macro (for either a prefix or infix form) can be an
 arbitrary pattern or a @rhombus(~parsed) escape:

@itemlist(

 @item{When the right-hand side is represented by an arbitrary pattern,
  the pattern can match any sequence of terms after the macro name in its
  enclosing group. The match need not extend to the end of the group (in
  which case the group might continue with an infix operator).}
  
 @item{When @rhombus(~parsed) is used for the the right-hand side, then
  the macro is applied with an already-parsed term after the macro name in
  a use of the macro. That parse heeds precedence and associativity
  declarations for other macros and for operators defined with
  @rhombus(operator).}

)

 Using @litchar{|} alternatives, a single definition can have any number
 of @rhombus(rule_pattern)s. The patterns describe any number of prefix
 and infix variants that are (presumably) distinguished by patterns that
 are tried in order. The name to define must be the same across all
 @rhombus(rule_pattern)s. If @rhombus(~parsed) is used for the the
 right-hand side in a @rhombus(rule_pattern) of an infix or prefix form,
 then it must be the only infix or prefix @rhombus(rule_pattern) among
 the alternatives.

 The body after each @rhombus(rule_pattern) must be an immediate
 @rhombus('') template, and any @rhombus($) escape within the template
 can only refer to an input pattern variable or a literal syntax
 object, optionally parenthesized, for an operator (e.g.,
 @rhombus($('$')) to generate a literal @rhombus($)). More general
 compile-time expressions are not allowed; use @rhombus(expr.macro),
 instead, to enable compile-time expressions.

 Before the @rhombus(template) body of a @rhombus(rule_pattern),
 @rhombus(option) keywords can appear. The options
 @rhombus(~weaker_than), @rhombus(~stronger_than), @rhombus(~same_as),
 @rhombus(~same_on_left_as), and @rhombus(~same_on_right_as) declare
 an name's precedence relative to other names. The
 @rhombus(~associativity) option is allowed only with an infix
 @rhombus(rule_pattern). The @rhombus(~op_stx) option binds an
 identifier or operator as it appears in a use of the macro (which cannot be
 matched directly in the @rhombus(rule_pattern), since that position
 is used for the name that @rhombus(expr.rule) binds).
 In a defined with @litchar{|} alternatives,
 most @rhombus(option)s are allowed only in the first case, but
 @rhombus(~op_stx) is allowed and separate in each case.

 See @secref("namespaces") for information on @rhombus(identifier_path)
 and @rhombus(operator_path).

@examples(
  ~eval: macro_eval
  expr.rule 'thunk: $body':
    'fun (): $body'
  thunk: "ok"
  (thunk: "ok")()
)

}


@doc(
  defn.macro 'expr.macro $rule_pattern:
                $option; ...
                $body
                ...'
  defn.macro 'expr.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...'
){

 Like @rhombus(expr.rule), but

@itemlist(
  
 @item{with arbitrary compile-time code in the body after each
  @rhombus(rule_pattern),},
  
 @item{for each @rhombus(rule_pattern) without @rhombus(~parsed) for its
  right-hand side, the pattern must match to the entire remainder of a
  group where the macro is used; and},

 @item{the body after a @rhombus(rule_pattern) without a right-hand
  @rhombus(~parsed) can return two values: an expansion for the consumed
  part of the input group, and a tail for the unconsumed part; if a single
  value is returned, the tail is assumed to be empty.}

)

}


@doc(
  defn.macro 'expr.only.rule $rule_decl'
  defn.macro 'expr.only.macro $macro_decl'
){

 Like @rhombus(expr.rule) and @rhombus(expr.macro), but the identifier
 or operator is bound only in the @rhombus(rhombus/expr, ~datum) @tech{space}.

}


@doc(
  syntax.class expr_meta.Group:
    kind: ~group
    field parsed
  syntax.class expr_meta.AfterPrefixGroup(op_name):
    kind: ~group
    field parsed
    field [tail, ...]
  syntax.class expr_meta.AfterInfixGroup(op_name):
    kind: ~group
    field parsed
    field [tail, ...]
){

 @provided_meta()

 Syntax classes that match by parsing expressions. The @rhombus(parsed)
 field in each case is an opaque syntax object that represents the parsed
 expression form.

 The @rhombus(expr_meta.AfterPrefixGroup) and
 @rhombus(expr_meta.AfterInfixGroup) syntax classes expect an operator
 name that is bound as a prefix or infix operator, respectively. Parsing
 procedes as if immediately after the given operator---stopping when an
 infix operator of weaker precencence is encountered, for example. The
 result is in both a @rhombus(parsed) field and a @rhombus(tail) field
 that contains the remaining unparsed input.

@examples(
  ~eval: macro_eval
  ~defn:
    :
      // an infix `choose` that works without a right-hand side
      // expression, and that has precendence between `+` and `*`
      expr.macro '$left choose $tail ...':
        ~weaker_than: *
        ~stronger_than: +
        match '$tail ...'
        | '': values('factorial($left)', '')
        | '$(right :: expr_meta.AfterInfixGroup('choose'))':
             values('factorial($left)/factorial($right)', '$right.tail ...')
      fun | factorial(0) : 1 | factorial(n): n*factorial(n-1)
  ~repl:
    4 choose
    4 choose 2
    4 choose 2 + 1
    4 choose 1*2
    (4 choose 1) * 2
)


}


@«macro.close_eval»(macro_eval)
