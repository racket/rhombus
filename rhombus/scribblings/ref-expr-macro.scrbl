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
