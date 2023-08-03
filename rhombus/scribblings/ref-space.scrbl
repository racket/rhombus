#lang scribble/rhombus/manual
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

  decl.macro 'space.enforest $space_id:
                $space_clause_or_body_or_export
                ...'

  grammar space_clause_or_body_or_export:
    #,(@rhombus(space_path, ~space_clause)) $space_id_path
    #,(@rhombus(macro_definer, ~space_clause)) $id
    #,(@rhombus(bridge_definer, ~space_clause)) $id
    #,(@rhombus(meta_namespace, ~space_clause)) $meta_namespace_id:
      $space_meta_clause_or_body
      ...
    $nestable_body
                  
  grammar space_id_path:
    $id
    $space_id_path / $id

  grammar space_meta_clause_or_body:
    #,(@rhombus(parse_syntax_class, ~space_meta_clause)) $id
    #,(@rhombus(parse_prefix_more_syntax_class, ~space_meta_clause)) $id
    #,(@rhombus(parse_infix_more_syntax_class, ~space_meta_clause)) $id
    #,(@rhombus(identifier_parser, ~space_meta_clause)) $expr
    #,(@rhombus(parse_checker, ~space_meta_clause)) $expr
    #,(@rhombus(parsed_packer, ~space_meta_clause)) $id
    #,(@rhombus(parsed_unpacker, ~space_meta_clause)) $id
    #,(@rhombus(description, ~space_meta_clause)) $expr
    #,(@rhombus(operator_description, ~space_meta_clause)) $expr
    #,(@rhombus(reflection, ~space_meta_clause)) $id
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
 defined as a @tech{namespace}. Among the
 @rhombus(space_clause_or_body_or_export)s, @rhombus(nestable_body)
 forms can add definitions and exports to the namespace, the same as for
 @rhombus(namespace). However, the namespace is particularly intended to
 export the name specified by @rhombus(macro_definer, ~space_clause).
 That name is conventionally @rhombus(macro, ~datum).
 As a somewhat lower-level mechanism, @rhombus(bridge_definer, ~space_clause)
 exports a name for binding arbitrary compile-time values analogous to
 @rhombus(meta.bridge). If @rhombus(macro_definer, ~space_clause) and
 @rhombus(bridge_definer, ~space_clause) are not declared, then there is
 no way to bind in the new namespace except by using lower-level mechanisms.

 Also typically among the @rhombus(space_clause_or_body_or_export)s, a
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
 represented as a syntax object.

 When just @rhombus(macro_definer, ~space_clause) and
 @rhombus(parse_syntax_class, ~space_meta_clause) are declared, then each
 macro effectively must produce a fully expanded term. When a
 @rhombus(parse_checker, ~space_meta_clause) clause is supplied, then it
 can impose a check and/or conversion on the result for every macro in
 the space, and that conversion can include recursively expanding when
 the result is not yet fully expanded. To aid in the distinction of terms
 that are fully expanded, use @rhombus(parsed_packer, ~space_meta_clause)
 and @rhombus(parsed_unpacker, ~space_meta_clause). Then, macros that
 generate expanded terms should pack them using (a function based on) the
 function declared by @rhombus(parsed_packer, ~space_meta_clause), while
 the @rhombus(parse_checker, ~space_meta_clause) function can check for a
 parsed term using the function declared by
 @rhombus(parsed_unpacker, ~space_meta_clause). Meanwhile, the syntax
 classes bound by @rhombus(parse_syntax_class, ~space_meta_clause) and similar
 recognize terms constructed via @rhombus(parsed_packer, ~space_meta_clause)
 as already parsed.

@itemlist(

 @item{@rhombus(macro_definer, ~space_clause): declares an identifier to
  be bound to a macro-definition form analogous to @rhombus(expr.macro),
  but for defining macros for the space.}

 @item{@rhombus(bridge_definer, ~space_clause): declares an identifier to
  be bound to a meta-definition form analogous to @rhombus(meta.bridge),
  but for defining bridges in the space.}

 @item{@rhombus(parse_syntax_class, ~space_meta_clause): declares an
  identifier to be bound as a @rhombus(~group) syntax class with a
  @rhombus(group, ~datum) field; the value of a match is a parsed term,
  while the @rhombus(group, ~datum) field holds the matched unparsed
  terms.}

 @item{@rhombus(parse_prefix_more_syntax_class, ~space_meta_clause):
  declares an identifier to be bound as a @rhombus(~group) syntax class
  that takes one argument and has with @rhombus(group, ~datum) and
  @rhombus(tail, ~datum) fields. The argument is a syntax object containg
  a prefix operator or identifier that is bound for the space. Parsing
  proceeds as if after the argument of the operator, which means that
  parsing can stop with a tail sequence remaining. The parsed ``argument''
  is is the matched result, the consumed terms are in a
  @rhombus(group, ~datum) field, and the remainaing tail is a
  @rhombus(tail, ~datum) field.}

 @item{@rhombus(parse_infix_more_syntax_class, ~space_meta_clause):
  declares an identifier like
  @rhombus(parse_prefix_more_syntax_class, ~space_meta_clause), but the
  syntax class expects a syntax object with an infix operator or
  identifier. Parsing can stop when reaching an infix operator in the
  group whose precedence is weaker than the starting one.}

 @item{@rhombus(parse_checker, ~space_meta_clause): supplies a
  compile-time function that is applied to two arguments: the result of
  any macro defined for the space, and a procedure implementing the macro
  transformer (which is useful for reporting errors or recursively expanding); the result is a
  syntax object, typically the one that was given, but possibly adjusted.}

 @item{@rhombus(parsed_packer, ~space_meta_clause): declares an
  identifier to be bound to a function that takes a syntax term and
  returns a syntax object representing a parsed term. A parsed term parses
  as itself, and it is opaque except as unpacked via a function declared
  with @rhombus(parsed_unpacker, ~space_meta_clause).}

 @item{@rhombus(parsed_unpacker, ~space_meta_clause): declares an
  identifier to be bound to a function that takes a syntax term and
  optionally either @rhombus(#false) or a procedure of one argument. If
  the first argument is a parsed term, the declared unpacker acts as the
  inverse of the function declared with
  @rhombus(parsed_packer, ~space_meta_clause). For any other value, if a
  second argument is provided as a procedure, then the procedure is called
  and the first argument is passed along; otherwise, an error is reported.}

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

 @item{@rhombus(reflection, ~space_meta_clause): declares an identifier to be
  bound to a @rhombus(SpaceMeta, ~annot) that refers to the defined space.
  This name is useful in combination with @rhombus(syntax_meta.value), for
  example.}

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
    meta_namespace_id: block id
    space_id_path: space.enforest ~decl
  space_clause.macro 'space_path $space_id_path'
  space_clause.macro 'macro_definer $id'
  space_clause.macro 'bridge_definer $id'
  space_clause.macro 'meta_namespace $meta_namespace_id:
                        $space_meta_clause_or_body_or_export
                          ...'
){

 Clause forms for use within a @rhombus(space.enforest) or
 @rhombus(space.transform) form. See @rhombus(space.enforest) for more
 information.

}

@doc(
  space_meta_clause.macro 'parse_syntax_class $id'
  space_meta_clause.macro 'parse_prefix_more_syntax_class $id'
  space_meta_clause.macro 'parse_infix_more_syntax_class $id'
  space_meta_clause.macro 'identifier_parser $expr'
  space_meta_clause.macro 'parse_checker $expr'
  space_meta_clause.macro 'parsed_packer $id'
  space_meta_clause.macro 'parsed_unpacker $id'
  space_meta_clause.macro 'description $expr'
  space_meta_clause.macro 'operator_description $expr'
  space_meta_clause.macro 'reflection $id'
){

 @provided_meta()

 Clause forms for use within a @rhombus(meta_namespace, ~space_clause)
 clause within a @rhombus(space.enforest) or @rhombus(space.transform)
 form. See @rhombus(space.enforest) for more information.

}

@doc(
  annot.macro 'SpaceMeta'
){

@provided_meta()

 A @rhombus(SpaceMeta, ~annot) compile-time value reflects a space that
 would be referenced in a run-time position by the space name. For
 example, @rhombus(expr_meta.space) for use with a compile-time function
 like @rhombus(syntax_meta.value) refers to the same space as
 @rhombus(expr, ~space) as used with @rhombus(only_space, ~expo).

}


@macro.close_eval(macro_eval)
