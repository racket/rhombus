#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Expression Macros}

@doc(
  space.enforest expr
){

 The @tech{space} for bindings of identifiers and operators that can be
 used in expression, definition, and declaration positions.

}

@doc(
  defn.macro 'expr.macro $macro_pattern:
                $option; ...
                $body
                ...'
  defn.macro 'expr.macro
              | $macro_pattern:
                  $option; ...
                  $body
                  ...
              | ...'
){

 Like @rhombus(macro), but arbitrary compile-time code can appear in the
 body after each @rhombus(macro_pattern).

 If a @rhombus(macro_pattern) ends with
 @rhombus(#,(@rhombus($, ~bind))()) or
 @rhombus($#,(@rhombus(identifier, ~var)) ..., ~bind), then a match
 covers all subsequent terms in the enclosing group of a use of the
 macro. In that case, the @rhombus(body) after the pattern can return two
 values: an expansion for the consumed part of the input match, and a
 tail for the unconsumed part. Returning a single value is the same as
 return an empty tail.

}


@doc(
  syntax_class expr_meta.Group:
    kind: ~group
    field parsed
  syntax_class expr_meta.AfterPrefixGroup(op_name):
    kind: ~group
    field parsed
    field [tail, ...]
  syntax_class expr_meta.AfterInfixGroup(op_name):
    kind: ~group
    field parsed
    field [tail, ...]
){

 @provided_meta()

 Syntax classes that match by parsing expressions. The @rhombus(parsed)
 field in each case is an opaque syntax object that represents the parsed
 expression form.

 The @rhombus(expr_meta.AfterPrefixGroup, ~stxclass) and
 @rhombus(expr_meta.AfterInfixGroup, ~stxclass) syntax classes expect an operator
 name that is bound as a prefix or infix operator, respectively. Parsing
 procedes as if immediately after the given operator---stopping when an
 infix operator of weaker precencence is encountered, for example. The
 result is in both a @rhombus(parsed) field and a @rhombus(tail) field
 that contains the remaining unparsed input.

@examples(
  ~eval: macro_eval
  ~defn:
    :
      // an infix `no_fail` that works without a right-hand side
      // expression, and that has weak precendence
      expr.macro
      | '$left no_fail $()':
          ~weaker_than: ~other
          'try: $left; ~catch _: #false'
      | '$left no_fail $(right :: expr_meta.AfterInfixGroup('no_fail')) $()':
           values('try: $left; ~catch _: $right.parsed', '$right.tail ...')
  ~repl:
    1/0 no_fail
    1/0 no_fail 0
    1/0 no_fail 0 + 1
    1+0 no_fail 0 + 1
    (1+0 no_fail 0) + 1
)


}


@«macro.close_eval»(macro_eval)
