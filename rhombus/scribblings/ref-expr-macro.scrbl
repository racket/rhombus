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
    '$identifier_or_operator $pattern ...'
    '$ $term_pattern $identifier_or_operator $pattern ...'
  grammar identifier_or_operator:
    $identifier
    $operator
    $$(dollar)('$$(dollar)')
    ($identifier_path)
    ($operator_path)
  grammar term_pattern:
    $term_identifier
    ($term_identifier :: $syntax_class)
  grammar option:
    ~stronger_than: $identifier_or_operator ...
    ~weaker_than: $identifier_or_operator ...
    ~same_as: $identifier_or_operator ...
    ~same_on_left_as: $identifier_or_operator ...
    ~same_on_right_as: $identifier_or_operator ...
    ~associativity: $assoc
    ~op_stx: $identifier
    ~parsed_right
  grammar assoc:
    ~left
    ~right
    ~none
){

 Defines @rhombus(identifier), @rhombus(operator), @rhombus($),
 @rhombus(identifier_path), or @rhombus(operator_path) as a pattern-based
 macro whose expansion is described by a @rhombus(template) that can
 refer to pattern variables bound in the @rhombus(rule_pattern). A
 @rhombus(rule_pattern) is matched to a portion of its enclosing group,
 and need not extend to the end of the group to match. A defined
 @rhombus(operator) cannot be @rhombus($), but the form @rhombus($('$'))
 can be used to define @rhombus($).

 Each @rhombus(rule_pattern) starts with @rhombus(''). Within
 @rhombus(''), either the first term is an identifier or operator to be
 defined as a prefix macro, or a single @rhombus($, ~bind) escape is
 followed by the identifier or operator to be defined as an infix macro.

 Using @litchar{|} alternatives, a single definition can have any number
 of prefix and infix variants (presumably) distinguished by patterns that
 are tried in order.

 The body after each @rhombus(rule_pattern) must be an immediate
 @rhombus('') template, and any @rhombus($) escape within the template
 can only refer to an input pattern variable or a literal syntax
 object, optionally parenthesized, for an operator (e.g.,
 @rhombus($('$')) to generate a literal @rhombus($)). More general
 compile-time expressions are not allowed; use @rhombus(expr.macro),
 instead, to enable compile-time expressions.

 Before the body of a @rhombus(rule_pattern), operator
 @rhombus(option) keywords can appear. The options
 @rhombus(~weaker_than), @rhombus(~stronger_than), @rhombus(~same_as),
 @rhombus(~same_on_left_as), and @rhombus(~same_on_right_as) declare
 an operator's precedence relative to other operators. The
 @rhombus(~associativity) option is allowed only with a infix-operator
 @rhombus(rule_pattern). The @rhombus(~op_stx) option binds an
 identifier or operator for a use of the macro (which cannot be
 matched directly in the @rhombus(rule_pattern), since that position
 is used for the name that @rhombus(expr.rule) binds). If the
 @rhombus(~parsed_right) option is present, then the
 @rhombus(rule_pattern) should match a single term after the macro
 name, and it will match an already-parsed term after the macro name
 in a use of the macro. In a defined with @litchar{|} alternatives,
 most @rhombus(option)s are allowed only in the first case, but
 @rhombus(~op_stx) is allowed in each case.

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
  
  @item{each @rhombus(rule_pattern) is matched to the entire remainder
   of a group where the macro is used; and},

  @item{a body returns value values: an expansion for the consumed part
   of the input group, and a tail for the unconsumed part.}

)

}

@«macro.close_eval»(macro_eval)
