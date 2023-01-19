#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title(~tag: "ref-space"){Spaces}

An identifier can have different meanings in different contexts, such as
expression versus binding, because an identifier can be bound in a
specific @deftech{space}. Exampe spaces include
@rhombus(expr, ~space) and @rhombus(bind, ~space), which
generally correspond to a @tech{namespace} that provides a binding
form for the space. There is also a default space, which effectively
binds in all spaces. For example, in an expression context, an
identifier is resolved by first checking the
@rhombus(expr, ~datum) space and then the default space.

Binding forms like @rhombus(def), @rhombus(expr.macro), and
@rhombus(bind.macro) bind an identifier in the default space. Variants
like @rhombus(expr.only.macro) bind in a specific space like
@rhombus(expr, ~datum).
Normally, when an identifier has a binding in some space, it should also
have a binding in the default space. Otherwise, local binding of the
same identifier name can lead to ambigious references. So, binding an
identifier with @rhombus(def) and then @rhombus(bind.only.macro) is a
good way to give an identifier meaning in both expression and binding
contexts, but using only @rhombus(expr.only.macro) plus
@rhombus(bind.only.macro) tends to be a bad idea.

Expressions, definitions, and declarations use the same space,
@rhombus(expr, ~datum), since those contexts tend to overlap.
Most other contexts have their own spaces, even though some of them also
overlap with expression positions, such as class and interface clauses.

The @rhombus(space.enforest) and @rhombus(space.transform)
forms create a new space along with its associated parser
driver and macro-definitions forms.

@doc(
  defn.macro 'space.enforest $space_identifier:
                ~space_path $space_path
                $option
                ...
                ~meta_namespace $meta_namespace_identifier:
                  $meta_option
                  ...'

  grammar space_path:
    $identifier
    $space_path / $identifier

  grammar option:
    ~macro $identifier
    ~only_macro $identifier

  grammar meta_option:
    ~syntax_class $identifier
    ~syntax_class_prefix_more $identifier
    ~syntax_class_infix_more $identifier
    ~macro_result: $filter_expr
    ~identifier_transformer: $transformer_expr
    ~desc $string
    ~operator_desc $string
){

 Defines @rhombus(space_identifier) as a @tech{space} with syntax
 classes and macro-definition forms that reference and bind in the space
 specified by @rhombus(space_path). The @rhombus(space_path) should be
 globally unique, typically based on the module path of the enclosing
 module, but the intent is that this identifying path is always
 referenced as @rhombus(space_identifier).

 The identifiers bound by @rhombus(option)s are defined and exported
 from @rhombus(space_identifier) as a @tech{namespace}. The identifiers
 bound in @rhombus(meta_option)s are defined and exported from
 @rhombus(meta_namespace_identifier) as a compile-time namespace.

 Normally, the options @rhombus(~syntax_class) and @rhombus(~macro) are
 declared, at a minimum, so that a @tech{syntax class} is defined to
 initiate parsing for the sublanguage, and a macro-definition form exists
 to add bindings that will be used by that parser for the sublanguage.

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

 @item{@rhombus(~macro): names an identifier to be bound to a
  macro-definition form analogous to @rhombus(expr.macro), but for defining
  rule-based macros for the sublanguage.}

 @item{@rhombus(~only_macro): names an identifier like @rhombus(~macro), but
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
  defn.macro 'space.transform $space_identifier:
                ~space_path space_path
                $option
                ...
                ~meta_namespace $meta_namespace_identifier:
                  $meta_option
                  ...'

  grammar space_path:
    $identifier
    $space_path / $identifier

  grammar option:
    ~macro $identifier
    ~only_macro $identifier

  grammar meta_option:
    ~syntax_class $identifier
    ~macro_result: $expr
    ~desc $string
){

 Like @rhombus(space.enforest) but for a simpler form of
 sublanguage that has only prefix-triggered forms, like definition
 contexts.

}

