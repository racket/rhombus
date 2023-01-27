#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@title(~tag: "ref-space"){Spaces}

An identifier can have different meanings in different contexts, such as
expression versus binding, because an identifier can be bound in a
specific @deftech{space}. Example spaces include
@rhombus(expr, ~space) and @rhombus(bind, ~space), which
generally correspond to a @tech{namespace} that provides a binding
form for the space.
Binding forms like @rhombus(def), @rhombus(expr.macro), and
@rhombus(bind.macro) bind an identifier in the corresponding space.

Forms that bind in different spaces can be used on the same name to give
that name a meaning in multiple contexts. The @rhombus(class) form binds
the class name in many spaces at once, so that a class name works as a
constructor in expressions, as an attribute, as a pattern form in
bindings, and so on. New bindings in new spaces can always be added
alongside existing bindings. The @rhombus(import) form supports
space-specific operations through the @rhombus(only_space, ~impo) and
@rhombus(except_space, ~impo) modifiers, so existing bindings can be
suppressed and then, perhaps, replaced on re-export.

Expressions, definitions, and declarations use the same space,
@rhombus(expr, ~space), since those contexts tend to overlap.
Most other contexts have their own spaces, even though some of them also
overlap with expression positions, such as class and interface clauses.
The space for expression is special in another way: a binding in that
space hides any binding for another space in an enclosing scope (but
not bindings in other spaces in the same scope).

The @rhombus(space.enforest) and @rhombus(space.transform)
forms create a new space along with its associated parser
driver and macro-definitions forms.

@doc(
  defn.macro 'space.enforest $space_identifier:
                $space_clause_or_body
                ...'

  grammar space_clause_or_body:
    #,(@rhombus(space_path, ~space_clause)) $space_identifier_path
    #,(@rhombus(macro_definer, ~space_clause)) $identifier
    #,(@rhombus(meta_namespace, ~space_clause)) $meta_namespace_identifier:
      $space_meta_clause_or_body
      ...
    $body
    $export
                  
  grammar space_identifier_path:
    $identifier
    $space_identifier_path / $identifier

  grammar space_meta_clause_or_body:
    #,(@rhombus(parse_syntax_class, ~space_meta_clause)) $identifier
    #,(@rhombus(parse_prefix_more_syntax_class, ~space_meta_clause)) $identifier
    #,(@rhombus(parse_infix_more_syntax_class, ~space_meta_clause)) $identifier
    #,(@rhombus(identifier_parser, ~space_meta_clause)) $expr
    #,(@rhombus(parse_checker, ~space_meta_clause)) $expr
    #,(@rhombus(description, ~space_meta_clause)) $expr
    #,(@rhombus(operator_description, ~space_meta_clause)) $expr
    $body
    $export    
){

 Defines @rhombus(space_identifier) as a @tech{space} with syntax
 classes and macro-definition forms that reference and bind in the space
 specified by @rhombus(space_path, ~space_clause). The
 @rhombus(space_identifier_path) declared with
 @rhombus(space_path, ~space_clause) should be globally unique, typically
 based on the module path of the enclosing module, but the intent is that
 this identifying path is always referenced as
 @rhombus(space_identifier).

 Besides being defined as a space, the @rhombus(space_identifier) is
 defined as a @tech{namespace}. Among the
 @rhombus(space_clause_or_body)s, @rhombus(body) and @rhombus(export)
 forms can add definitions and exports to the namespace, the same as for
 @rhombus(namespace). However, the namespace is particularly intended to
 export the name specified by @rhombus(macro_definer, ~space_clause).
 That name is conventionally @rhombus(macro, ~datum). If
 @rhombus(macro_definer, ~space_clause) is not declared, then there is no
 way to bind in the new namespace except by using lower-level mechanisms.

 Also typically among the @rhombus(space_clause_or_body)s, a
 @rhombus(meta_namespace, ~space_clause) declares the name of a
 compile-time namespace, typically used in macros. The meta namespace's
 name is conventionally @rhombus(_meta, ~datum) appended to the end of
 the main namespace's name. The
 @rhombus(space_meta_clause_or_body) forms in the body of a
 @rhombus(meta_namespace, ~space_clause) clause are implicitly shifted to
 compile time, as if wrapped by @rhombus(meta). The meta namespace is
 particularly intended to export @tech{syntax class}es with names
 specified by @rhombus(parse_syntax_class, ~space_meta_clause),
 @rhombus(parse_prefix_more_syntax_class, ~space_meta_clause), and
 @rhombus(parse_infix_more_syntax_class, ~space_meta_clause) clauses. If
 no name is declared with
 @rhombus(parse_syntax_class, ~space_meta_clause) or similar, then there
 is no way to parse terms in the new space except by lower-level
 mechanisms.

 All together, a typical use of @rhombus(space.enforest) includes at
 least @rhombus(space_path, ~space_clause),
 @rhombus(macro_definer, ~space_clause), and
 @rhombus(meta_namespace, ~space_clause) containing
 @rhombus(parse_syntax_class, ~space_meta_clause).

@examples(
  ~eval: macro_eval
  ~defn:
    space.enforest new_thing:
      space_path my_collection/new_thing
      macro_definer macro
      meta_namespace new_thing_meta:
        parse_syntax_class Group
)

 These pieces might be used by an expression macro for a form that has a
 ``new thing'' position, while an operator is meanwhile defined to work
 in the ``new thing'' space.

@examples(
  ~eval: macro_eval
  ~defn:
    expr.macro 'print_new_thing: $(thing :: new_thing_meta.Group)':
      'println($thing.parsed)'
  ~defn:
    new_thing.macro 'the':
      '"the new thing"'
)

 The identifier supplied for
 @rhombus(parse_syntax_class, ~space_meta_clause) is defined as a
 @rhombus(~group) syntax class with a @rhombus(parsed, ~datum) field.
 The syntax class triggers parsing of a group using macros bindings,
 which are defined using the identifier bound by
 @rhombus(macro_definer, ~space_clause). For a pattern variable using the
 syntax class bound by @rhombus(parse_syntax_class, ~space_meta_clause),
 its @rhombus(parsed, ~datum) field is the result of parsing. No
 constraints are imposed on the result of parsing, except that it must be
 represented as a syntax object. If a
 @rhombus(parse_checker, ~space_meta_clause) clause is supplied, then it
 can impose a check on the result for every macro in the space.

@itemlist(

 @item{@rhombus(macro_definer, ~space_clause): declares an identifier to
  be bound to a macro-definition form analogous to @rhombus(expr.macro),
  but for defining macros for the space.}

 @item{@rhombus(parse_syntax_class, ~space_meta_clause): declares an
  identifier to be bound as a @rhombus(~group) syntax class with a
  @rhombus(parsed, ~datum) field.}

 @item{@rhombus(parse_prefix_more_syntax_class, ~space_meta_clause):
  declares an identifier to be bound as a @rhombus(~group) syntax class
  that takes one argument and has with @rhombus(parsed, ~datum) and
  @rhombus(tail, ~datum) fields. The argument is a syntax object containg
  a prefix operator or identifier that is bound for the space. Parsing
  proceeds as if for the argument of the operator, which means that
  parsing can stop with a tail sequence remaining. The parsed ``argument''
  is returned as the @rhombus(parsed, ~datum) field, while the reminaing
  tail is a @rhombus(tail, ~datum) field.}

 @item{@rhombus(parse_infix_more_syntax_class, ~space_meta_clause):
  declares an identifier like
  @rhombus(parse_prefix_more_syntax_class, ~space_meta_clause), but the
  syntax class expects a syntax object with an infix operator or
  identifier. Parsing can stop when reaching an infix operator in the
  group whose precedence is weaker than the starting one.}

 @item{@rhombus(parse_checker, ~space_meta_clause): supplies a
  compile-time function that is applied to two arguments: the result of
  any macro defined for the space, and a procedure implementing the macro
  transformer (which is useful for reporting errors); the result is a
  syntax object, typically the one that was given, but possibly adjusted.}

 @item{@rhombus(identifier_parser, ~space_meta_clause): supplies a
  compile-time function that is applied to an identifier that is not bound
  in the space and should return a parsed form for the identifier. By
  default, a syntax error is reported for unbound identifiers.}

 @item{@rhombus(description, ~space_meta_clause): supplies a string that
  describes the space; the string is used for reporting syntax
  errors.}

 @item{@rhombus(operator_description, ~space_meta_clause): supplies a
  string that describes operators in the space; the string is used
  for reporting syntax errors.}

)

}


@doc(
  defn.macro 'space.transform $space_identifier:
                $space_clause_or_body_or_export
                ...'
){

 Like @rhombus(space.enforest) but for a simpler form of space that has
 only prefix-triggered forms with identifier names, like definition
 contexts.

 A @rhombus(space.transform) declaration does not support
 @rhombus(parse_prefix_more_syntax_class, ~space_meta_clause),
 @rhombus(parse_infix_more_syntax_class, ~space_meta_clause),
 @rhombus(identifier_parser, ~space_meta_clause), or
 @rhombus(operator_description, ~space_meta_clause) clauses.

}

@doc(
  space_clause.macro 'space_path $space_path'
  space_clause.macro 'macro_definer $identifier'
  space_clause.macro 'meta_namespace $meta_namespace_identifier:
                        $space_meta_clause_or_body_or_export
                          ...'
){

 Clause forms for use within a @rhombus(space.enforest) or
 @rhombus(space.transform) form. See @rhombus(space.enforest) for more
 information.

}

@doc(
  space_meta_clause.macro 'parse_syntax_class $identifier'
  space_meta_clause.macro 'parse_prefix_more_syntax_class $identifier'
  space_meta_clause.macro 'parse_infix_more_syntax_class $identifier'
  space_meta_clause.macro 'identifier_parser $expr'
  space_meta_clause.macro 'parse_checker $expr'
  space_meta_clause.macro 'description $expr'
  space_meta_clause.macro 'operator_description $expr'
){

 Clause forms for use within a @rhombus(meta_namespace, ~space_clause)
 clause within a @rhombus(space.enforest) or @rhombus(space.transform)
 form. See @rhombus(space.enforest) for more information.

}

@macro.close_eval(macro_eval)
