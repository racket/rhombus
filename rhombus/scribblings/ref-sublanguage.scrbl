#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@title(~tag: "ref-sublanguage"){Sublanguages}

@doc(
  defn.macro 'sublanguage.enforest $space_path:
                $option
                ...'

  grammar option:
    ~syntax_class $identifier
    ~syntax_class_prefix_more $identifier
    ~syntax_class_infix_more $identifier
    ~rule $identifier
    ~only_rule $identifier
    ~macro $identifier
    ~only_macro $identifier
    ~macro_result: $filter_expr
    ~identifier_transformer: $transformer_expr
    ~desc $string
    ~operator_desc $string

  grammar space_path:
    $identifier
    $space_path / $identifier
){

 Defines syntax classes and macro-definition forms that reference and
 bind in the @tech{space} specified by @rhombus(space_path). Normally,
 the options @rhombus(~syntax_class) and @rhombus(~macro) are declared,
 at a minimum, so that a @tech{syntax class} is defined to initiate
 parsing for the sublanguage, and a macro-definition form exists to add
 bindings that will be used by that parser for the sublanguage.

 The identifier supplied for @rhombus(~syntax_class) is defined as a
 @rhombus(~group) syntax class with a @rhombus(parsed) attribute. The
 syntax class triggers parsing of a group using macros bindings, whcih
 are defined using the identifier bound by @rhombus(~macro) or similar
 options. For a pattern variable using the syntax class bound by
 @rhombus(~syntax_class), its @rhombus(parsed) attribute is the result of
 parsing. No constraints are imposed on the result of parsing, except
 that it must be represented as a syntax object. (If a
 @rhombus(~check_result) option is supplied, then it can impose a check
 on the result for every macro in the sublanguage.)

@itemlist(

 @item{@rhombus(~syntax_class): names an identifier to be bound as a
  @rhombus(~group) syntax class with a @rhombus(parsed) attribute.}

 @item{@rhombus(~syntax_class_prefix_more): names an identifier to be
  bound as a @rhombus(~group) syntax class with @rhombus(parsed) and
  @rhombus(tail) attributes. The given group should start with a prefix
  operator or identifier that is bound for the sublanguage. Parsing
  proceeds as if for the argument of the operator, which means that
  parsing can stop with a tail sequence remaining. The parsed ``argument''
  is returned as the @rhombus(parsed) attribute, while the reminaing tail
  is a @rhombus(tail) attribute.}

 @item{@rhombus(~syntax_class_infix_more): names an identifier like
  @rhombus(~syntax_class_prefix_more), but the syntax class expects a
  group that starts with an infix operator or identifier. Parsing can stop
  when reaching an infix operator in the group whose precedence is weaker
  than the starting one.}

 @item{@rhombus(~rule): names an identifier to be bound to a
  macro-definition form analogous to @rhombus(expr.rule), but for defining
  rule-based macros for the sublanguage.}

 @item{@rhombus(~only_rule): names an identifier like @rhombus(~rule), but
  analogous to @rhombus(expr.only.rule).}

 @item{@rhombus(~macro): names an identifier like @rhombus(~rule), but
  analogous to @rhombus(expr.macro).}

 @item{@rhombus(~only_macro): names an identifier like @rhombus(~rule), but
  analogous to @rhombus(expr.only.macro).}

 @item{@rhombus(~macro_result): supplies a compile-time function that is
  applied to two arguments: the result of any macro defined for the
  sublanguage, and a procedure implementing the macro transformer (which
  is useful for reporting errors); the result is a syntax object,
  typically the one that was given, but possibly adjusted.}

 @item{@rhombus(~identifier_transformer): supplies a compile-time
  function that is applied to an identifier that is not bound in the
  sublanguage. By default, a syntax error is reported for unbound
  identifiers.}

 @item{@rhombus(~desc): supplies a string that describes the sublanguage;
   the string is used for reporting syntax errors.}

 @item{@rhombus(~operator_desc): supplies a string that describes
  operators in the sublanguage; the string is used for reporting syntax
  errors.}

)

}


@doc(
  defn.macro 'sublanguage.transform $space_path:
                $option
                ...'

  grammar option:
    ~syntax_class $identifier
    ~macro $identifier
    ~only_macro $identifier
    ~macro_result: $expr
    ~desc $string

  grammar space_path:
    $identifier
    $space_path / $identifier
){

 Like @rhombus(sublanguage.enforest) but for a simpler form of
 sublanguage that has only prefix-triggered forms, like definition
 contexts.

}

