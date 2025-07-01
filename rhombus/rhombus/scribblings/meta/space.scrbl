#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@title(~tag: "ref-space"){Spaces}

An identifier can have different meanings in different contexts, such as
expression versus binding, because an identifier can be bound in a
specific @deftech{space}. Example spaces include
@rhombus(expr, ~space) and @rhombus(bind, ~space), which
generally correspond to a @tech(~doc: ref_doc){namespace} that provides a binding
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

Most contexts have their own spaces, even though some of them also
overlap with expression positions, such as definitions, declarations,
@rhombus(class) clauses, and @rhombus(for) clauses. The parsing process
for such overlapping spaces will check non-expression spaces for
bindings, first. The space for expression is special in another way: a
binding in that space hides any binding for another space in an
enclosing scope (but not bindings in other spaces in the same scope).

The @rhombus(space.enforest) and @rhombus(space.transform)
forms create a new space along with its associated parser
driver and macro-definitions forms.

@doc(
  ~nonterminal:
    space_id: block id
    meta_namespace_id: block id
    meta_expr: block expr

  decl.macro 'space.enforest $space_id:
                $space_clause_or_body_or_export
                ...'

  grammar space_clause_or_body_or_export:
    #,(@rhombus(space_path, ~space_clause)) $space_id_path
    #,(@rhombus(macro_definer, ~space_clause)) $id
    #,(@rhombus(macro_definer, ~space_clause)) $id:
      $keyword
      ...
    #,(@rhombus(bridge_definer, ~space_clause)) $id
    #,(@rhombus(private, ~space_clause)) $clause
    #,(@rhombus(meta_namespace, ~space_clause)) $meta_namespace_id:
      $space_meta_clause_or_body
      ...
    $nestable_body

  grammar space_id_path:
    $id
    $space_id_path / $id

  grammar space_meta_clause_or_body:
    #,(@rhombus(parse_syntax_class, ~space_meta_clause)) $id
    #,(@rhombus(parse_syntax_class, ~space_meta_clause)) $id($id, ...)
    #,(@rhombus(parse_prefix_more_syntax_class, ~space_meta_clause)) $id
    #,(@rhombus(parse_infix_more_syntax_class, ~space_meta_clause)) $id
    #,(@rhombus(name_start_syntax_class, ~space_meta_clause)) $id
    #,(@rhombus(identifier_parser, ~space_meta_clause)) $meta_expr
    #,(@rhombus(parse_checker, ~space_meta_clause)) $meta_expr
    #,(@rhombus(parsed_packer, ~space_meta_clause)) $id
    #,(@rhombus(parsed_unpacker, ~space_meta_clause)) $id
    #,(@rhombus(description, ~space_meta_clause)) $meta_expr
    #,(@rhombus(operator_description, ~space_meta_clause)) $meta_expr
    #,(@rhombus(reflection, ~space_meta_clause)) $id
    #,(@rhombus(private, ~space_meta_clause)) $meta_clause
    $nestable_body
){

 Defines @rhombus(space_id) as a @tech{space} with syntax
 classes and macro-definition forms that reference and bind in the space
 specified by @rhombus(space_path, ~space_clause). The
 @rhombus(space_id_path) declared with
 @rhombus(space_path, ~space_clause) should be globally unique, typically
 based on the module path of the enclosing module, but the intent is that
 this identifying path is normally referenced as
 @rhombus(space_id) or through the identifier bound by
 @rhombus(reflection, ~space_meta_clause).

 Besides being defined as a space, @rhombus(space_id) is
 defined as a @tech(~doc: ref_doc){namespace}. Among the
 @rhombus(space_clause_or_body_or_export)s, @rhombus(nestable_body)
 forms can add definitions and exports to the namespace, the same as for
 @rhombus(namespace) and @rhombus(class) (and definitions among the
 @rhombus(space_clause_or_body_or_export)s are ordered as in a @rhombus(class)
 body with respect to the definition of the space itself). However, the namespace is particularly intended to
 export the name specified by @rhombus(macro_definer, ~space_clause).
 That name is conventionally @rhombus(macro, ~datum).
 As a somewhat lower-level mechanism, @rhombus(bridge_definer, ~space_clause)
 exports a name for binding arbitrary compile-time values analogous to
 @rhombus(meta.bridge). If @rhombus(macro_definer, ~space_clause) and
 @rhombus(bridge_definer, ~space_clause) are not declared, then there is
 no way to bind in the new namespace except by using lower-level mechanisms.
 A @rhombus(macro_definer, ~space_clause) or @rhombus(bridge_definer, ~space_clause)
 can be prefixed with @rhombus(private, ~space_clause) to bind a name
 without implicitly exporting it from the @rhombus(space_id) namespace.

 Also typically among the @rhombus(space_clause_or_body_or_export)s, a
 @rhombus(meta_namespace, ~space_clause) declares the name of a
 compile-time namespace, typically used in macros. The meta namespace's
 name is conventionally @rhombus(_meta, ~datum) appended to the end of
 the main namespace's name. The
 @rhombus(space_meta_clause_or_body) forms in the body of a
 @rhombus(meta_namespace, ~space_clause) clause are implicitly shifted to
 compile time, as if wrapped by @rhombus(meta). The meta namespace is
 particularly intended to export @tech(~doc: ref_doc){syntax class}es with names
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
        parse_syntax_class Parsed
)

 These pieces might be used by an expression macro for a form that has a
 ``new thing'' position, while an operator is meanwhile defined to work
 in the ``new thing'' space.

@examples(
  ~eval: macro_eval
  ~defn:
    expr.macro 'print_new_thing: $(thing :: new_thing_meta.Parsed)':
      'println($thing)'
  ~defn:
    new_thing.macro 'the':
      '"the new thing"'
  ~repl:
    print_new_thing: the
)

 The identifier supplied for
 @rhombus(parse_syntax_class, ~space_meta_clause) is defined as a
 @rhombus(~group) syntax class with a @rhombus(group, ~datum) field.
 The syntax class triggers parsing of a group using macros bindings,
 which are defined using the identifier bound by
 @rhombus(macro_definer, ~space_clause). For a pattern variable using the
 syntax class bound by @rhombus(parse_syntax_class, ~space_meta_clause),
 its value is the result of parsing, while the @rhombus(group, ~datum)
 field holds the terms that were parsed. No
 constraints are imposed on the result of parsing, except that it must be
 represented as a syntax object.

 The @rhombus(parse_syntax_class, ~space_meta_clause) is used with a
 name followed by a parenthesized sequence of names, the syntax class
 requires arguments when used in a syntax pattern. Those arguments are
 propagated to a macro for the new space, which can receive the arguments
 through declarations that use the keywords listed with
 @rhombus(macro_definer, ~space_clause). The number of keywords listed
 with @rhombus(macro_definer, ~space_clause) must match the number of arguments
 specified with @rhombus(parse_syntax_class, ~space_meta_clause).

 When just @rhombus(macro_definer, ~space_clause) and
 @rhombus(parse_syntax_class, ~space_meta_clause) are declared, then each
 macro effectively must produce a fully parsed term. To enable macros in
 the space that expand to uses of other macros in the space, a
 distinction is needed between fully parsed and still-to-be-expanded
 terms. Use @rhombus(parsed_packer, ~space_meta_clause) and
 @rhombus(parsed_unpacker, ~space_meta_clause) to introduce that
 distinction, use the packer name in macros that produce fully parsed
 terms, and use the unpacker name to access parsed content. Meanwhile,
 the syntax classes bound by
 @rhombus(parse_syntax_class, ~space_meta_clause) and similar recognize
 terms constructed via @rhombus(parsed_packer, ~space_meta_clause) as
 already parsed.

@examples(
  ~eval: macro_eval
  ~defn:
    space.enforest newer_thing:
      space_path my_collection/new_thing
      macro_definer macro
      meta_namespace newer_thing_meta:
        parse_syntax_class Parsed
        parsed_packer pack
        parsed_unpacker unpack
  ~defn:
    expr.macro 'print_newer_thing: $(thing :: newer_thing_meta.Parsed)':
      'println($(newer_thing_meta.unpack(thing)))'
  ~defn:
    newer_thing.macro 'the':
      newer_thing_meta.pack('"the newer thing"')
    newer_thing.macro 'THE':
      'the'
  ~repl:
    print_newer_thing: the
    print_newer_thing: THE
)


 When a @rhombus(parse_checker, ~space_meta_clause) clause is supplied,
 then it can impose a check and/or conversion on the result for every
 macro in the space. That conversion should include recursively expanding
 when the result is not yet fully expanded, which can be detected by
 supplying a second argument to the function bound by
 @rhombus(parsed_unpacker, ~space_meta_clause). The default parse checker
 performs this recursive parsing step.

 More details on space clauses and meta clauses:

@itemlist(

 @item{@rhombus(macro_definer, ~space_clause): Declares an identifier to
  be bound to a macro-definition form analogous to @rhombus(expr.macro),
  but for defining macros for the space. Keywords listed after the
  identifier serve as options along the same lines as @rhombus(~op_stx) in
  @rhombus(expr.macro), and they receive arguments passed to the syntax
  class as declared by @rhombus(parse_syntax_class, ~space_meta_clause).
  This clause form can prefixed with @rhombus(private, ~space_clause).}

 @item{@rhombus(bridge_definer, ~space_clause): Declares an identifier to
  be bound to a meta-definition form analogous to @rhombus(meta.bridge),
  but for defining bridges in the space.
  This clause form can prefixed with @rhombus(private, ~space_clause).}

 @item{@rhombus(parse_syntax_class, ~space_meta_clause): Declares an
  identifier to be bound as a @rhombus(~group) syntax class with a
  @rhombus(group, ~datum) field; the value of a match is a parsed term,
  while the @rhombus(group, ~datum) field holds the matched unparsed
  terms. Identifiers listed in parentheses after the syntax class name are
  arguments to the syntax class, and they are received by macro
  implementations through keywords listed in
  @rhombus(macro_definer, ~space_clause). The number of arguments declared
  in @rhombus(parse_syntax_class, ~space_meta_clause) must match the
  number of keywords listed in @rhombus(macro_definer, ~space_clause), and
  the arguments and keywords are correlated by position.
  This clause form can prefixed with @rhombus(private, ~space_meta_clause).}

 @item{@rhombus(parse_prefix_more_syntax_class, ~space_meta_clause):
  Declares an identifier to be bound as a @rhombus(~group) syntax class
  that takes one argument and has @rhombus(group, ~datum) and
  @rhombus(tail, ~datum) fields. The argument is a syntax object containing
  a prefix operator or identifier that is bound for the space. Parsing
  proceeds as if after the argument of the operator, which means that
  parsing can stop with a tail sequence remaining. The parsed ``argument''
  is is the matched result, the consumed terms are in a
  @rhombus(group, ~datum) field, and the remaining tail is a
  @rhombus(tail, ~datum) repetition field.
  This clause form can prefixed with @rhombus(private, ~space_meta_clause).}

 @item{@rhombus(parse_infix_more_syntax_class, ~space_meta_clause):
  Declares an identifier like
  @rhombus(parse_prefix_more_syntax_class, ~space_meta_clause), but the
  syntax class expects a syntax object with an infix operator or
  identifier. Parsing can stop when reaching an infix operator in the
  group whose precedence is weaker than the starting one.
  This clause form can prefixed with @rhombus(private, ~space_meta_clause).}

 @item{@rhombus(name_start_syntax_class, ~space_meta_clause): Declares
  an identifier to be bound as a @rhombus(~group) syntax class, which has
  @rhombus(name, ~datum), @rhombus(head, ~datum), and
  @rhombus(tail, ~datum) fields. The syntax class matches a group that
  starts with a dotted name that is bound in the space, and the name is
  converted to a single identifier term as the @rhombus(name, ~datum)
  field. It also matches an unbound name as the start of the group. The
  @rhombus(head, ~datum) field is a repetition that contains the leading
  terms of the group that we used to compute the @rhombus(name, ~datum),
  and the @rhombus(tail, ~datum) field is a repetition that contains the
  remaining terms of the group.
  This clause form can prefixed with @rhombus(private, ~space_meta_clause).}

 @item{@rhombus(parse_checker, ~space_meta_clause): Supplies a
  compile-time function that is applied to two arguments: the result of
  any macro defined for the space, and a function implementing the macro
  transformer (which is useful for reporting errors or recursively
  expanding); the result is a syntax object, typically the one that was
  given, but possibly adjusted. The default checker recursively expands
  when either @rhombus(parsed_packer, ~space_meta_clause) or
  @rhombus(parsed_unpacker, ~space_meta_clause) is supplied.}

 @item{@rhombus(parsed_packer, ~space_meta_clause): Declares an
  identifier to be bound to a function that takes a syntax term and
  returns a syntax object representing a parsed term. A parsed term parses
  as itself, and it is opaque except as unpacked via a function declared
  with @rhombus(parsed_unpacker, ~space_meta_clause).
  This clause form can prefixed with @rhombus(private, ~space_meta_clause).}

 @item{@rhombus(parsed_unpacker, ~space_meta_clause): Declares an
  identifier to be bound to a function that takes a syntax term and
  optionally either @rhombus(#false) or a function of one argument. If
  the first argument is a parsed term, the declared unpacker acts as the
  inverse of the function declared with
  @rhombus(parsed_packer, ~space_meta_clause). For any other value, if a
  second argument is provided as a function, then the function is called
  and the first argument is passed along; otherwise, an error is reported.
  This clause form can prefixed with @rhombus(private, ~space_meta_clause).}

 @item{@rhombus(identifier_parser, ~space_meta_clause): Supplies a
  compile-time function that is applied to an identifier that is not bound
  in the space and should return a parsed form for the identifier. By
  default, a syntax error is reported for unbound identifiers.}

 @item{@rhombus(description, ~space_meta_clause): Supplies a string that
  describes the space; the string is used for reporting syntax
  errors.}

 @item{@rhombus(operator_description, ~space_meta_clause): Supplies a
  string that describes operators in the space; the string is used
  for reporting syntax errors.}

 @item{@rhombus(reflection, ~space_meta_clause): Declares an identifier to be
  bound to a @rhombus(SpaceMeta, ~annot) that refers to the defined space.
  This name is useful in combination with @rhombus(syntax_meta.value), for
  example.
  This clause form can prefixed with @rhombus(private, ~space_meta_clause).}

)

}


@doc(
  ~nonterminal:
    space_id: block id
    space_clause_or_body_or_export: space.enforest ~decl
  decl.macro 'space.transform $space_id:
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
  ~nonterminal:
    space_id_path: space.enforest ~decl
    meta_namespace_id: block id
    space_meta_clause_or_body: space.enforest ~decl
  space_clause.macro 'space_path $space_id_path'
  space_clause.macro 'macro_definer $id'
  space_clause.macro 'macro_definer $id:
                        $keyword
                        ...'
  space_clause.macro 'bridge_definer $id'
  space_clause.macro 'meta_namespace $meta_namespace_id:
                        $space_meta_clause_or_body
                        ...'
){

 Clause forms for use within a @rhombus(space.enforest) or
 @rhombus(space.transform) form. See @rhombus(space.enforest) for more
 information.

}

@doc(
  ~meta
  space_meta_clause.macro 'parse_syntax_class $id'
  space_meta_clause.macro 'parse_syntax_class $id($id, ...)'
  space_meta_clause.macro 'parse_prefix_more_syntax_class $id'
  space_meta_clause.macro 'parse_infix_more_syntax_class $id'
  space_meta_clause.macro 'name_start_syntax_class $id'
  space_meta_clause.macro 'identifier_parser $expr'
  space_meta_clause.macro 'parse_checker $expr'
  space_meta_clause.macro 'parsed_packer $id'
  space_meta_clause.macro 'parsed_unpacker $id'
  space_meta_clause.macro 'description $expr'
  space_meta_clause.macro 'operator_description $expr'
  space_meta_clause.macro 'reflection $id'
){

 Clause forms for use within a @rhombus(meta_namespace, ~space_clause)
 clause within a @rhombus(space.enforest) or @rhombus(space.transform)
 form. See @rhombus(space.enforest) for more information.

}

@doc(
  space_clause.macro 'private $clause'
  space_meta_clause.macro 'private $meta_clause'
){

 Clauses prefixed with @rhombus(private, ~space_clause) or meta clauses
 prefixed with @rhombus(private, ~space_meta_clause) define names without
 automatically exporting them from the enclosing namespace.

 Only specific @rhombus(clause) or
 @rhombus(meta_clause) forms are recognized:
 @rhombus(macro_definer, ~space_clause),
 @rhombus(bridge_definer, ~space_clause),
 @rhombus(parse_syntax_class, ~space_meta_clause),
 @rhombus(parse_prefix_more_syntax_class, ~space_meta_clause),
 @rhombus(parse_infix_more_syntax_class, ~space_meta_clause),
 @rhombus(name_start_syntax_class, ~space_meta_clause),
 @rhombus(parsed_packer, ~space_meta_clause),
 @rhombus(parsed_unpacker, ~space_meta_clause), and
 @rhombus(reflection, ~space_meta_clause).

}


@doc(
  ~meta
  annot.macro 'SpaceMeta'
){

 A @rhombus(SpaceMeta, ~annot) compile-time value reflects a space that
 would be referenced in a run-time position by the space name. For
 example, @rhombus(expr_meta.space) for use with a compile-time function
 like @rhombus(syntax_meta.value) refers to the same space as
 @rhombus(expr, ~space) as used with @rhombus(only_space, ~expo).

}


@(macro.close_eval(macro_eval))
