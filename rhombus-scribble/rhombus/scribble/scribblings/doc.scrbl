#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      scribble/doc_meta open)

@title(~tag: "doc"){Documenting Bindings}

@(nonterminal:
    expr: block expr)

@doc(
  decl.macro 'docmodule($option, ...,
                        $mod_path)'
  grammar option:
    ~lang
    ~no_declare
    ~use_sources:
      $mod_path
      ...
){

 A declaration to set the context of @rhombus(doc) documentation forms
 that appear later in the same section or nested subsections. The
 bindings will be documented as exports from @rhombus(mod_path).

 If the @rhombus(~lang) option is specified, then the module is
 documented as a language module for use with @hash_lang().

 If the @rhombus(~no_declare) option is specified, then the module
 declaration is rendered in the document, but it does not set context for
 @rhombus(doc) forms.

 The @rhombus(~use_sources) option declares modules that should be
 treated as exporting modules for hyperlinking purposes, even though the
 rendered documentation says that the bindings originate from the main
 @rhombus(mod_path). Use @rhombus(~use_sources) when bindings are
 reexported from multiple documented modules, and where bindings accessed
 via all of those modules should be linked to the @rhombus(docs) in this
 section.

 The @rhombus(docmodule) form must be used only once without
 @rhombus(~no_declare) within a section, but a section's
 @rhombus(docmodule) can be shadowed by a declaration in a subsection.

}

@doc(
  ~nonterminal:
    op_or_id_name: namespace ~defn
    nonterm_op_or_id_name: namespace op_or_id_name ~defn
    builtin_space: rhombus

  expr.macro 'doc ($prep,
                   ...,
                   $entry,
                   ...,
                   [$description, ...])'

  grammar entry:
    #,(@rhombus(fun, ~doc)) $fun_spec
    #,(@rhombus(operator, ~doc)) $op_spec
    #,(@rhombus(def, ~doc)) $def_spec
    #,(@rhombus(enum, ~doc)) $enum_spec
    #,(@rhombus(expr.macro, ~doc)) $macro_spec
    #,(@rhombus(defn.macro, ~doc)) $macro_spec
    #,(@rhombus(decl.macro, ~doc)) $macro_spec
    #,(@rhombus(annot.macro, ~doc)) $macro_spec
    #,(@rhombus(bind.macro, ~doc)) $macro_spec
    #,(@rhombus(repet.macro, ~doc)) $macro_spec
    #,(@rhombus(class, ~doc)) $class_spec
    #,(@rhombus(interface, ~doc)) $interface_spec
    #,(@rhombus(grammar, ~doc)) $grammar_spec
    $other_doc_entry_form
    ~include $mod_path:
      $id ...
      ...

  grammar prep:
    ~nonterminal:
      $id: $nt_key_ref
      ...
    ~nonterminal_key: $nt_key
    ~literal:
      id ...
      ...
    ~meta
    ~also_meta
  grammar nt_key_ref:
    $op_or_id_name
    $op_or_id_name $nonterm_op_or_id_name
    $op_or_id_name $space
    $op_or_id_name $nonterm_op_or_id_name $space
  grammar nt_key:
    $op_or_id_name
    $op_or_id_name $space
  grammar space:
    $builtin_space
    ~at $space_name
){

 Documents a set of bindings. A documented binding needs to be imported
 into the document's implementation using @rhombus(meta_label, ~impo).
 meanwhile, an enclosing section needs to include a @rhombus(docmodule)
 to select the module that is documented to export the
 binding---typically a module that reexports from the original
 implementing module. A binding should be documented by at most one
 @rhombus(doc) form, but multiple @rhombus(entry)s in the same
 @rhombus(doc) form can describe the same binding.

 Each @rhombus(entry) clause is typical a @deftech{doc entry} form. For
 example, use the @rhombus(fun, ~doc) doc entry form to document a
 function. An @rhombus(entry) can also be @rhombus(~include) to extract
 documentation attached to a definition (e.g., extracted from a
 @rhombus(fun) or @rhombus(enum) definition that includes
 @rhombus(~doc)). New doc entry forms can be defined with
 @rhombus(doc.bridge, ~defn). All of the documented forms are shown together as one
 documentation box in the rendered document.

 Each pre-defined doc entry form has the same name and basic shape as a
 Rhombus definition form. For example, the @rhombus(fun, ~doc) doc entry
 form can be used to document functions that are implemented using
 @rhombus(fun). A doc entry form may not have exactly the same syntax as
 the definition form, however. For example, the @rhombus(fun, ~doc) doc
 entry form supports only a single function case, and multiple cases are
 meant to be documented with multiple uses of the @rhombus(fun, ~doc) doc
 entry form. A doc entry form typically also does not require the body
 part of the corresponding definition form, but it will typically
 @emph{allow} the body to be written and typeset that body literally. The
 set of doc entry forms listed in @rhombus(entry) is merely
 representative, because new forms can be added (and not even all of the
 predefined ones are listed there).

 The list of @rhombus(description)s to describe the documented bindings
 is usually written inside @litchar{{}} using @litchar("@") notation. For
 example, the following use of @rhombus(doc) defines @rhombus(Widget),
 @rhombus(ok_button), and @rhombus(make_panel) all together:

@rhombusblock(
  @doc(
    annot.macro 'Widget'
    def ok_button :: Widget
    fun make_panel(label :: String,
                   [child :: Widget, ...]) :: Widget
  ){
    A @rhombus(Widget, ~annot) is an interactive thing, and the
    constant @rhombus(ok_button) is a stock widget. Use @rhombus(make_panel)
    to combine several widgets into a titled panel, where @rhombus(label)
    provide the panel's title.
  }
)

 Doc entry forms like @rhombus(fun, ~doc) and @rhombus(annot.macro) are
 responsible for extracting the relevant name that is being documented
 via @rhombus(doc). They also extract names that should be treated as
 metavariables in the documentation, such as the argument @rhombus(label)
 to @rhombus(make_panel), so that when @tt|{@rhombus(label)}| is used in
 the description, it renders as a metavariable there.

 Each @rhombus(entry) is otherwise typeset as in @rhombus(rhombusblock)
 content, which means that @rhombus(#,, ~datum) can be used to escape
 from literal typesetting and write an expression that produces a
 Scribble element. to replace the
 @rhombus(#,(@rhombus(#,, ~datum))(#,(@nontermref(expr)))) escape.

 Before doc entry forms, a @rhombus(prep) declaration can bind
 identifiers to customize their treatment in typesetting:

@itemlist(

 @item{A @rhombus(~nonterminal) declaration causes the associated
  @rhombus(id) as a nonterminal to refer to a nonterminal declared with
  @rhombus(grammar) in another binding's documentation. The
  @rhombus(op_or_id_name) is the other binding, and
  @rhombus(nonterm_op_or_id_name) is the name defined there, which
  defaults to @rhombus(id) if not supplied. A @rhombus(builtin_space)
  specification indicates the binding's space, and it's needed if the
  binding space of @rhombus(op_or_id_name) is distinct from the expression
  binding space.}

 @item{A @rhombus(~nonterminal_key) declaration selects the way that
  nonterminals defined in this @rhombus(doc) form can be referenced by
  other @rhombus(doc) forms. By default, the first binding defined among
  the @rhombus(entry)s is used, but @rhombus(~nonterminal_key) can select
  among others.}

 @item{A @rhombus(~literal) form specified @rhombus(id)s that are to be
  terated as literals (no metavariables) in @rhombus(entry)s. This form is
  normally not needed, since identifiers used without a @rhombus($, ~bind)
  in @rhombus(entry)s are generally treated as literal.}

 @item{A @rhombus(~meta) form indicates that the documented bindings are
  provided @rhombus(meta, ~expo), while @rhombus(~also_meta) indicates
  that the documented bindings are provided both normally and
  @rhombus(meta, ~expo). At most one of @rhombus(~meta) and
  @rhombus(~also_meta) can be used. }

)

}


@doc(
  ~nonterminal:
    nt_key_ref: doc
  defn.macro 'nonterminal:
                $id: $nt_key_ref
                ...'
  expr.macro 'nontermref($id)'
){

 The @rhombus(nonterminal) and @rhombus(nontermref) forms provide a way
 to bind nonterminal metavariables outside of a @rhombus(doc) form. That
 is, using @rhombus(nonterminal) is analogous to using
 @rhombus(~nonterminal) within @rhombus(doc).

 Because nonterminal names tend to be short and easily confused with
 other uses, however, nonterminal names bound with @rhombus(nonterminal)
 are not recognized by @rhombus(rhombus). Instead, @rhombus(nontermref)
 must be used to explicitly refer to the nonterminal (with a link to its
 definition).

}


@doc(
  ~nonterminal:
    id_name: namespace ~defn
  doc 'fun $id_name($arg, ...) $term ...'
  doc 'operator ($op_or_id_name $arg) $term ...'
  doc 'operator ($arg_term $op_or_id_name $arg_term) $term ...'
){

 A @tech{doc entry} form to document a function @rhombus(id_name) or
 operator @rhombus(op_or_id_name). Extra @rhombus(term)s can be anything,
 but would typically show a return annotation, if any.

 Each @rhombus(arg) is parsed like a @rhombus(fun) argument to infer
 argument bindings as metavariables. Referencing a metavariable in the
 description body of @rhombus(doc) with @rhombus(rhombus) typesets the
 name in the same way as the argument variable. Similarly, each
 @rhombus(arg_term) is parsed as for an @rhombus(operator) argument.

 To document a function or operator with multiple cases, uses
 @rhombus(fun, ~doc) or @rhombus(operator, ~doc) multiple times with the
 same @rhombus(id_name) or @rhombus(op_or_id_name) in the same
 @rhombus(doc) form.

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
  doc 'def $id_name $term ...'
){

 A @tech{doc entry} form to document @rhombus(id_name). Extra
 @rhombus(term)s can be anything, but would typically show a return
 annotation, if any.

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
    enum_clause: enum ~defn
  doc 'enum $id_name:
         $enum_clause
         ...'
){

 A @tech{doc entry} form to document an enumeration. In addition to
 @rhombus(id_name) itself as an annotation and namespace, each binding
 within the namespace described by @rhombus(enum_clause) is also
 documented.

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
    op_or_id_name: namespace ~defn
  doc '«expr.macro '$op_or_id_name $quoted_term ...' $term ...»'
  doc '«expr.macro '#,(rhombus($, ~bind)) $op_or_id_name $id_name $quoted_term ...' $term ...»'
  doc '«defn.macro '$id_name $quoted_term ...'»'
  doc '«decl.macro '$id_name $quoted_term ...'»'
  doc '«decl.nestable_macro '$id_name $quoted_term ...'»'
  doc '«annot.macro '$op_or_id_name $quoted_term ...' $maybe_fallback»'
  doc '«annot.macro '#,(rhombus($, ~bind)) $op_or_id_name $id_name $quoted_term ...' $maybe_fallback»'
  doc '«bind.macro '$op_or_id_name $quoted_term ...'»'
  doc '«bind.macro '#,(rhombus($, ~bind)) $op_or_id_name $id_name $quoted_term ...'»'
  doc '«repet.macro '$op_or_id_name $quoted_term ...'»'
  doc '«repet.macro '#,(rhombus($, ~bind)) $op_or_id_name $id_name $quoted_term ...'»'

  grammar maybe_fallback:
    $(epsilon)
    ~method_fallback $id
    ~method_fallback: $id
){

 A @tech{doc entry} form to documents an expression, definition,
 declaration, annotation, binding, or repetition macro
 @rhombus(op_or_id_name) or @rhombus(id_name).

 In the @rhombus(quoted_term)s, a use of @rhombus($, ~bind) indicates
 that the subsequent identifier is a metavariable, as opposed to a
 literal. The @rhombus($, ~bind) itself will not be rendered, but the
 identifier will be rendered as a metavariable. Furthermore, it may refer
 to a nonterminal that is defined using @rhombus(grammar, ~doc) or bound
 with a @rhombus(~nonterminal) preparation in the enclosing @rhombus(doc)
 form.

 In the @rhombus(annot.macro) case, @rhombus(maybe_fallback) can provide
 an alternate annotation name that is used when hyperlinking method names
 in @rhombus(rhombus) and @rhombus(rhombusblock) forms. When the context
 of a method name indicates the documented @rhombus(op_or_id_name) but
 the method cannot be found within that name as a namespace, then the
 @rhombus(id) after @rhombus(~method_fallback) is tried as a namespace.
 Typically, @rhombus(id) implies @rhombus(op_or_id_name), but not
 necessarily; for example, @rhombus(ReadableString) falls back to
 @rhombus(String) due to the way that @rhombus(String) method accept
 @rhombus(ReadableString)s.

 The following example demonstrates documenting a @rhombus(widget)
 expression macro with a metavariable @rhombus(direction) that is
 specified via @rhombus(grammar). Meanwhile, @rhombus(~nonterminal) is
 used to make @rhombus(widget_expr) hyperlink to the @nontermref(expr)
 nonterminal that is defined in the Rhombus manual.

@rhombusblock(
  @doc(
    ~nonterminal:
      widget_expr: block expr
    expr.macro 'widget:
                  ~label: $expr
                  $direction
                  $widget_expr
                  ...'
    grammar direction:
      ~horiz
      ~vert
  ){
    Creates a widget, where each @rhombus(widget_expr) supplies a
    child widget for the combined widget.
  }
)

}


@doc(
  ~nonterminal:
    id_name: namespace ~defn
  doc '«reducer.macro '$id_name $quoted_term ...' $term ...»'
  doc '«unquote_bind.macro '$id_name $quoted_term ...' $term ...»'
  doc '«for_clause.macro '$id_name $quoted_term ...' $term ...»'
  doc '«class_clause.macro '$id_name $quoted_term ...' $term ...»'
  doc '«interface_clause.macro '$id_name $quoted_term ...' $term ...»'
  doc '«veneer_clause.macro '$id_name $quoted_term ...' $term ...»'
  doc '«syntax_class_clause.macro '$id_name $quoted_term ...' $term ...»'
  doc '«pattern_clause.macro '$id_name $quoted_term ...' $term ...»'
  #//doc '«space_clause.macro '$id_name $quoted_term ...' $term ...»'
  #//doc '«space_meta_clause.macro '$id_name $quoted_term ...' $term ...»'
  doc '«entry_point.macro '$id_name $quoted_term ...' $term ...»'
  doc '«immediate_callee.macro '$id_name $quoted_term ...' $term ...»'
  doc '«impo.macro '$id_name $quoted_term ...' $term ...»'
  doc '«expo.macro '$id_name $quoted_term ...' $term ...»'
  doc '«impo.modifier '$id_name $quoted_term ...' $term ...»'
  doc '«expo.modifier '$id_name $quoted_term ...' $term ...»'
  #//doc '«modpath.macro '$id_name $quoted_term ...' $term ...»'
){

 More @tech{doc entry} forms along the same lines as
 @rhombus(defn.macro, ~doc).

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
    method_id: block id
    property_id: block id
    dot_id: block id
    in_id_name: namespace id_name ~defn
  doc 'class $id_name($arg, ...) $term ...'
  doc 'interface $id_name $term ...'
  doc 'method ($id :: $in_id_name) . $method_id($arg, ...) $term ...'
  doc 'property ($id :: $in_id_name) . $property_id $term ...'
  doc 'dot ($id :: $in_id_name) . $dot_id $term ...'
){

 A @tech{doc entry} forms to document a class @rhombus(id_name),
 interface @rhombus(id_name), method @rhombus(method_id) in
 @rhombus(in_id_name), property @rhombus(property_id) in
 @rhombus(in_id_name), or dot-accessed @rhombus(dot_id) in
 @rhombus(in_id_name).

 Note that methods and properties are defined standalone, as opposed to
 being inside @rhombus(class, ~doc) or @rhombus(interface, ~doc). That's
 why a @rhombus(in_id_name) is needed to identify the enclosing class or
 interface. The @rhombus(id) before @rhombus(in_id_name) is treated as a
 metavariable.

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
  doc 'syntax_class $id_name $term ...'
){

 A @tech{doc entry} form to document a syntax class.

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
  doc 'operator_order.def $id_name'
  doc 'operator_order.def $id_name: $option; ...'
  doc 'operator_order.def_set $id_name: $option; ...'
  doc 'operator_order: $option; ...'
){

 The @rhombus(operator_order.def, ~doc) and
 @rhombus(operator_order.def_set, ~doc) forms are @tech{doc entry} forms
 to document an operator order like @rhombus(addition, ~operator_order)
 or operator order set like @rhombus(arithmetic, ~operator_order).

 The @rhombus(operator_order, ~doc) form does not document a binding
 itself, but is meant to be used alongside @rhombus(expr.macro, ~doc) and
 similar when documenting a prefix or infix operator. Each
 @rhombus(option) is typically @rhombus(~order: #,(@rhombus(name, ~var)))
 to document the operator's order or a @rhombus(~stronger_than) or
 @rhombus(~weaker_than) form to document its precedence directly.

}


@doc(
  ~nonterminal:
    id_name: namespace ~defn
  doc '«doc '$id_name $quoted_term ...' $term ...»'
){

 A @tech{doc entry} form to document @tech{doc entry} forms.

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
  doc 'space.enforest $id_name:
         $term ...
         ...'
  doc 'space.transform $id_name:
         $term ...
         ...'
){

 @tech{Doc entry} forms to document spaces.

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
  doc 'key_comp.def $id_name $term ...'
  doc 'Parameter.def $id_name $term ...'
){

 More @tech{doc entry} forms.

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
  doc 'grammar $id:
         $quoted_term ...
         ...'
){

 Defines a metavariable @rhombus(id) as a nonterminal. Each
 @rhombus(quoted_term) sequence is typeset as in
 @rhombus(expr.macro, ~doc), where @rhombus($, ~bind) can prefix an
 identifier to make it a metavariable instead of a literal.

}
