#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{General Forms}

@doc(
  ~nonterminal_key: begin
  grammar id
  grammar op
  grammar id_or_op:
    $id
    $op
){

 An @rhombus(id) or @rhombus(op) is a
 @seclink(~doc: [#'lib, "shrubbery/scribblings/shrubbery.scrbl"], "top"){shrubbery}
 identifier or operator, respectively. Some syntactic contexts require
 specifically an identifier, some require specifically an operator, and
 some allow either, as represented by @rhombus(id_or_op).

 Unless otherwise specified, a name like @rhombus(something_id, ~var) is
 an alias for @rhombus(id), and the @rhombus(something_, ~var) prefix
 merely serves to suggest the intent of the identifier and allow a
 reference to a specific identifier position within a grammar.

}


@doc(
  ~nonterminal_key: begin
  grammar expr
){

 In syntax descriptions, @rhombus(expr) stands for any expression form.
 Function calls, arithmetic, @rhombus(begin), and @rhombus(match) are
 some examples of expression forms, but @rhombusmodname(rhombus) provides
 many more.

 Besides all of the expression forms provided by
 @rhombusmodname(rhombus), new ones can be defined with @rhombus(macro)
 or @rhombus(expr.macro, ~expr).

 Unless otherwise specified, a name like @rhombus(fun_expr, ~var) is an
 alias for @rhombus(expr), similar to the rule for @rhombus(id).

}

@doc(
  ~nonterminal_key: begin
  grammar repet
){

 In syntax descriptions, @rhombus(repet) stands for any
 @tech{repetition} form. Identifiers (especailly ones bound as
 repetitions), function calls, and arithmetic are some examples of
 repetition forms, but @rhombusmodname(rhombus) provides many more.

 Besides all of the expression forms provided by
 @rhombusmodname(rhombus), new ones can be defined with
 @rhombus(repet.macro, ~expr).

}


@doc(
  ~nonterminal_key: begin
  grammar entry_point
){

 In syntax descriptions, @rhombus(entry_point) stands for an @tech{entry
  point}, which is syntactically an immediate function. The @rhombus(fun, ~entry_point)
 and @rhombus(macro, ~entry_point) expression forms also work as entry-point forms.

}


@doc(
  ~nonterminal_key: begin
  grammar defn
){

 In syntax descriptions, @rhombus(defn) stands for any definition
 form. The @rhombus(def), @rhombus(let), and @rhombus(class) forms are
 examples of definition forms.

 Besides all of the expression forms provided by
 @rhombusmodname(rhombus), new ones can be defined with
 @rhombus(defn.macro, ~expr).

}


@doc(
  ~nonterminal_key: begin
  grammar body:
    $expr
    $defn
){

 In syntax descriptions, @rhombus(body) is always used with ellipses
 afterward, meaning that definitions and expressons can be interleaved.
 In some cases, the expanded form of the @rhombus(body) equence must ends
 with an expression to provide a result value.

}

@doc(
  ~nonterminal_key: begin
  grammar decl
  grammar nestable_decl
  grammar nestable_body:
    $body
    $nestable_decl
){

 In syntax descriptions, @rhombus(decl) is a form that can appear
 immediately in a module, and a @rhombus(nestable_decl) is a form like
 @rhombus(export) that can appear in a @rhombus(namespace) or in a
 module.

 Besides forms provided by
 @rhombusmodname(rhombus), new ones can be defined with
 @rhombus(decl.macro, ~expr) and @rhombus(decl.nestable_macro, ~expr).

}


@doc(
  ~nonterminal_key: def
  grammar bind
){

 In syntax descriptions, @rhombus(bind) refers to any binding form,
 which might be simply an identifier, a binding form annotated with
 @rhombus(::, ~bind) or @rhombus(:~, ~bind), or a larger binding pattern.

 Besides all of the binding forms provided by @rhombusmodname(rhombus),
 new ones can be defined with @rhombus(bind.macro, ~bind).

}


@doc(
  ~nonterminal_key: ::
  grammar annot
){

 In syntax descriptions, @rhombus(annot) stands for any
 @tech{annotation} form.

 Besides all of the expression forms provided by
 @rhombusmodname(rhombus), new ones can be defined with
 @rhombus(annot.macro, ~expr).

}


@doc(
  ~nonterminal_key: namespace

  grammar id_path:
    $id
    $id_path . $id

  grammar op_path:
    $op
    $id_path . ($op)

  grammar op_or_id_path:
    $op_path
    $id_path
){

 Refers to a defined identifier or operator within a namespace, or used
 in a binding position to extend an existing namespace with a binding for
 the identifier or operator. In either context, in an
 @rhombus(id_path, ~var) or @rhombus(op_path, ~var), each
 @rhombus(id, ~var) before a dot must refer to a namespace.

}


@doc(
  ~nonterminal_key: #%quotes
  grammar pattern
  grammar template
){

 In syntax descriptions, @rhombus(pattern) and @rhombus(template) each
 stand for an arbitrary shrubbery form that is used for syntax matching
 or syntax generation, respectively.

 See the @rhombus(#%quotes, ~bind) binding form for information on
 syntax matching, and see the @rhombus(#%quotes) expression form for
 information on syntax generation.

}
