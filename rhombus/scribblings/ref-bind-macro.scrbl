#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@title{Binding Macros}

@doc(
  space.enforest bind
){

 The @tech{space} for bindings of identifiers and operators that can be
 used in binding positions such as the left-hand side of @rhombus(def)
 or the formal arguments of @rhombus(fun).

}


@doc(
  def bind_meta.space :: SpaceMeta
){

@provided_meta()

 A compile-time value that identifies the same space as
 @rhombus(bind, ~space). See also @rhombus(SpaceMeta, ~annot).

}


@doc(
  ~nonterminal:
    macro_patterns: expr.macro ~defn

  defn.macro 'bind.macro $macro_patterns'
){

 Like @rhombus(expr.macro), but defines an identifier or operator as a
 binding form in the @rhombus(bind, ~space) @tech{space}.
 The result of the macro expansion can be a low-level
 binding description created with @rhombus(bind_meta.pack).

@examples(
  ~eval: macro_eval
  ~defn:
    bind.macro 'many $ann as $id':
      '$id && [_ :: $ann, $('...')]'
  ~repl:
    def many Int as tickets: [1, 2, 3]
    tickets
    ~error: def many String as names: "oops"
)

 See @secref("bind-macro-protocol") for examples using the low-level
 protocol.

}

@doc(
  fun bind_meta.pack(stx :: Syntax) :: Syntax
){

 @provided_meta()

 Packs binding information that is represented by a syntax object with
 two parts: @rhombus(infoer_id, ~var) and @rhombus(data, ~var),
 combined in the form

 @rhombusblock(
  '(#,(@rhombus(infoer_id, ~var)), #,(@rhombus(data, ~var)))')

 The @rhombus(infoer_id, ~var) identifier must be bound to a transformer
 with @rhombus(bind.infoer), and @rhombus(data, ~var) is propagated to
 that transformer to get initial binding information and further
 ``continuations'' for generating matching and variable-defining forms.

 The representation of packed information as a syntax object is
 unspecified and meant to be opaque.

 See @secref("bind-macro-protocol") for more explanation and for
 examples.

}

@doc(
  fun bind_meta.unpack(stx :: Syntax) :: Syntax
){

 @provided_meta()

 The inverse of @rhombus(bind_meta.pack), normally used only internally in
 the expander.

}

@doc(
  defn.macro '«bind.infoer '$id($static_info_pattern, $data_pattern)':
                 $body
                 ...»'
){

 Defines @rhombus(id) as the infoer ``continuation'' of a
 binding macro's implementation. The inforer is invoked with ``upward''
 static information provided by the context of a use of the binding form,
 plus the same data syntax object that was supplied as part of the
 argument to @rhombus(bind_meta.pack). The transformer's result is
 automatically packed via @rhombus(bind_meta.pack_info), so it should be
 a syntax object that is suitable to pack---which means that it encodes
 information about identifiers to be bound as well as further
 ``continuations'' in the form of a matcher transformer defined with
 @rhombus(bind.matcher), a committer transformer defined with
 @rhombus(bind.committer), and binder transformer defined with
 @rhombus(bind.binder).

 See @secref("bind-macro-protocol") for more explanation and for
 examples.

}

@doc(
  fun bind_meta.pack_info(stx :: Syntax) :: Syntax
){

 @provided_meta()

 Packs binding information that specific to a use of a binding form,
 which means that ``upward'' has been provided to an infoer, and the
 infoer converts that to initial information about bindings plus
 ``continuations'' to generate the matching expression and variable
 definitions.

 The syntax object @rhombus(stx) must have the following shape:

 @rhombusblock(
  '(#,(@rhombus(ann_string, ~var)),
    #,(@rhombus(name_id, ~var)),
    ((#,(@rhombus(static_key, ~var)), #,(@rhombus(static_value, ~var))), ...),
    ((#,(@rhombus(defined_id, ~var)),
      [#,(@rhombus(var_use, ~var)), ...],
      ((#,(@rhombus(var_static_key, ~var)), #,(@rhombus(var_static_value, ~var))), ...)),
     ...),
    #,(@rhombus(matcher_id, ~var)),
    #,(@rhombus(committer_id, ~var)),
    #,(@rhombus(binder_id, ~var)),
    #,(@rhombus(data, ~var)))'
   )

 The @rhombus(ann_string, ~var) term is for error repoting when a value
 fails to match the binding. It describes the binder's match requires in
 the form of an annotation.

 The @rhombus(name_id, ~var) term is for error reporting and
 reflection in the sense that it is used as the inferred name for a value
 matched to the binder, in case such a name is relevant.

 The @rhombus(static_key, ~var)--@rhombus(static_value, ~var) pairs
 describe ``upward'' static information for anything that successfully
 matches the binding. The ``upward'' information can be useful to an
 enclosing binding form.

 The @rhombus(defined_id, ~var)s are the identifiers that are
 ultimately bound by the binding form. Each identifier to be defined has
 associated uses through @rhombus(var_use, ~var) values, and each
 identifier has ``downward'' static information through the
 @rhombus(var_static_key, ~var)--@rhombus(var_static_value, ~var) pairs.
 Like @rhombus(var_static_key, ~var)s, the meaning of
 @rhombus(var_use, ~var)s is up to cooperating parts in general, but
 two values are recognized by built-in forms:

@itemlist(

  @item{an exact non-negative integer indicates that the variable can be
  used as an expression (in the case of @rhombus(0)) or repetition at a
  certain depth (in the case of @rhombus(k, ~var) greater than
  @rhombus(0)); and}

  @item{@rhombus(~no_stx) indicates that the variable's is not
  compatible with @rhombus(let), because it needs to be bound early (such
  as through @rhombus(when, ~bind)).}

 )

 The @rhombus(matcher_id, ~var), @rhombus(committer_id, ~var), and
 @rhombus(binder_id, ~var) identifiers provide
 the ``continuation'' of the binder's expansion to generate a matching
 expression and a definition sequence.

 The @rhombus(data, ~var) term is propoagated to the use of
 @rhombus(matcher_id, ~var), @rhombus(committer_id, ~var), and
 @rhombus(binder_id, ~var), providing a communication
 channel from an infoer to a matcher and binder.
 
 The representation of packed information as a syntax object is
 unspecified and meant to be opaque.

 See @secref("bind-macro-protocol") for more explanation and for
 examples.
}

@doc(
  fun bind_meta.unpack_info(stx :: Syntax) :: Syntax
){

 @provided_meta()

 The inverse of @rhombus(bind_meta.pack_info), which is useful for
 unpacking information about the expansion of nested binding forms
 as produced by @rhombus(bind_meta.get_info).

}

@doc(
  fun bind_meta.get_info(bind_stx :: Syntax,
                         static_info :: Syntax) :: Syntax
){

 @provided_meta()

 Initiates the expansion of the binding form represented by
 @rhombus(bind_stx) in a context that supplies thet ``upward'' static
 information represented by @rhombus(static_info).

 Static information is represented by a syntax object that has the
 following shape:

 @rhombusblock(
  '((#,(@rhombus(key_id, ~var)), #,(@rhombus(val, ~var))), ...)')
 
 The result is a syntax object that represents the initial expansion of
 the binding form as a packed syntax object, whose form is unspecified
 and intended to be opaque. Use @rhombus(bind_meta.unpack_info) to convert
 the packed syntax object and expose information about the binding's
 expansion.

 See @secref("bind-macro-protocol") for an example.

}


@doc(
  fun bind_meta.is_immediate(info :: Syntax) :: Boolean
){

 @provided_meta()

 Takes the initialized binding-form expansion produced by
 @rhombus(bind_meta.get_info) and restports whether the binding is
 immediate in the same sense as just a variable: the binding always
 matches, and no work is required to convert or unpack the matched value.

}


@doc(
  defn.macro '«bind.matcher '$id($id_pattern, $data_pattern,
                                 $IF_pattern, $success_pattern, $fail_pattern)':
                 $body
                 ...»'
){

 Defines @rhombus(id) as the matcher ``continuation'' of a
 binding form's expansion. The result is an expression that inspects the
 value of the identifier matched to @rhombus(id_pattern) and uses the
 pieces matched by @rhombus(IF_pattern), @rhombus(success_pattern), and
 @rhombus(fail_pattern) to dispatch on match success or failure. The term
 matched to @rhombus(data_pattern) is whatever data the infoer included
 at the end of its result.

 When using a matcher unpacked via @rhombus(bind_meta.unpack_info), the
 group given for @rhombus(success_pattern) can be an immediate block
 formed with @colon (without a preceding term), and in that case, the
 groups of the block are spliced into a definition context where the
 binding is used.

 See @secref("bind-macro-protocol") for more explanation and for
 examples.

}

@doc(
  defn.macro '«bind.committer '$id($id_pattern, $data_pattern)':
                 $body
                 ...»'
){

 Defines @rhombus(id) as the committer ``continuation'' of a
 binding form's expansion. The result is an sequence of definitions for
 intermediate variables, where
 @rhombus(id_pattern) holds the value that was matched by a matcher. If
 the matcher included any additional bindings around the success branch,
 then those bindings are in the environment of the definitions generated
 by this binder. The term matched to @rhombus(data_pattern) is whatever
 data the infoer included at the end of its result.

 The definitions produced by a committer should not use identifiers
 supplied by a user of the binding form, because those names will not be
 adjusted by @rhombus(let). Instead, those definitions should be deferred
 to the binder function's result.

 See @secref("bind-macro-protocol") for more explanation and for
 examples.

}

@doc(
  defn.macro '«bind.binder '$id($id_pattern, $data_pattern)':
                 $body
                 ...»'
){

 Defines @rhombus(id) as the binder ``continuation'' of a
 binding form's expansion. The result is an sequence of definitions for
 the variables that are bound by the expander form, where
 @rhombus(id_pattern) holds the value that was matched by a matcher. If
 the matcher included any additional bindings around the success branch,
 then those bindings are in the environment of the definitions generated
 by this binder, and any intermediate definitions from the commiitter are
 also in the environment. The term matched to @rhombus(data_pattern) is whatever
 data the infoer included at the end of its result.

 The definitions produced by a binder should not refer to each other,
 because they may be adjusted by @rhombus(let) expansion. Bindings that
 need to be referenced by generated bindings should be in the committer
 function's output.

 See @secref("bind-macro-protocol") for more explanation and for
 examples.

}


@doc(
  syntax_class bind_meta.Parsed:
    kind: ~group
    fields:
      group
  syntax_class bind_meta.AfterPrefixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class bind_meta.AfterInfixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
){

 @provided_meta()

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), etc., but for bindings.

}

@doc(
  ~nonterminal:
    bind_maybe_kw_opt: fun ~defn

  syntax_class bind_meta.Argument:
    kind: ~group
    fields:
      parsed
      maybe_keyword
      maybe_expr
){

 @provided_meta()

 Matches forms that combine a binding with an optional kyword and
 optional default-value expression, like @rhombus(bind_maybe_kw_opt) for
 @rhombus(fun).

@itemlist(

  @item{The @rhombus(parsed) field holds a parsed binding form.}

  @item{The @rhombus(maybe_keyword) field is @rhombus(#false) if no
  keyword is present, otherwise it is a keyword syntax object.}

  @item{The @rhombus(maybe_expr) field is @rhombus(#false) if no
  default-value expression is provided, otherwise it is a group syntax
  object for the expression.}

)

}

@doc(
  ~nonterminal:
    maybe_res_annot: fun ~defn

  syntax_class bind_meta.Result:
    kind: ~sequence
    fields:
      count
      is_predicate
      maybe_converter
      static_info
      annotation_string
){

 @provided_meta()

 Matches a sequence of terms (possibly empty) for a result annotation,
 like @rhombus(maybe_res_annot) for @rhombus(fun).

 @itemlist(

  @item{The @rhombus(count) field holds an integer for the number of
  expected results, or @rhombus(#false) if no annotation is declared
  (i.e., any number of results is expected).}

  @item{The @rhombus(is_predicate) field is @rhombus(#true) if all
  annotations are @tech{predicate annotations}, or @rhombus(#false)
  if at least one of them is a @tech{converter annotation}.}

  @item{The @rhombus(maybe_converter) field is @rhombus(#false) if no
  annotation is declared or if it is unchecked, otherwise it is a syntax
  object for a function expression; the resulting function expects
  @rhombus(count) plus two arguments: each original result followed by a
  success procedure of @rhombus(count) arguments and a failure procedure
  of zero arguments.}

  @item{The @rhombus(static_info) field holds static information for the
  result (in unpacked form; see @rhombus(statinfo_meta.pack)); when
  @rhombus(count) is not @rhombus(1), then @rhombus(static_info) has a
  single key @rhombus(statinfo_meta.values_key) whose value is (packed)
  static information for each value (see
  @rhombus(statinfo_meta.pack_group)).}

  @item{The @rhombus(annotation_string) field describes the annotation,
  which is useful for raising an exception when conversion fails.}

)

}

@«macro.close_eval»(macro_eval)
