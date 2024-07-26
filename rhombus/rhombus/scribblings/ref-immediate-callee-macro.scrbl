#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@(def dollar = @rhombus($))

@title{Immediate Callee Macros}

An @deftech{immediate callee} position is one, such as the right-hand
side of @rhombus(|>), that can have a general expression, but where the
result of the expression will be applied immediately to a specific
number of arguments, and static information is available for the
arguments. An immediate callee can thus support more static-information
propagation than would normally take place at a function call.

@doc(
  space.enforest immediate_callee
){

 The @tech{space} for bindings of identifiers that can be used in
 @tech{immediate callee} positions.

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn
    statinfo_id: block id
    in_op_stx_id: block id
    in_op_mode_id: block id

  defn.macro 'immediate_callee.macro $prefix_macro_patterns'

  grammar option:
    ~op_stx: $id
    ~op_stx $id
    ~static_infos: $statinfo_id
    ~static_infos $statinfo_id
    ~in_op_mode: $in_op_mode_id
    ~in_op_mode $in_op_mode_id
    ~in_op_stx: $in_op_stx_id
    ~in_op_stx $in_op_stx_id

){

 Like @rhombus(defn.macro), but defines an identifier as an
 @tech{immediate callee} form in the @rhombus(immediate_callee, ~space)
 @tech{space}. Also, in addition to the @rhombus(~op_stx) option, the
 @rhombus(~static_infos), @rhombus(~in_op_stx), and/or
 @rhombus(~in_op_mode) ``options'' can be specified to receive
 information about the callee context:

@itemlist(

 @item{The value bound to @rhombus(statinfo_id) is a list with as many
  elements as arguments to be supplied to this callee. Each element of the
  list is unpacked static information for that argument.}

 @item{The value bound to @rhombus(in_op_mode_id) is either
  @rhombus(#'prefix) or @rhombus(#'infix), indicating whether
  @rhombus(in_op_stx_id) is being parsed as a prefix or infix operator.}


 @item{The value bound to @rhombus(in_op_stx_id) is a name for the
  expression form or operator that has this immediate callee. For example,
  @rhombus(in_op_stx_id) might be @rhombus('|>'). This operator or
  identifier may be needed with @rhombus(expr_meta.ends_parse) to parse
  the immediate callee form, since a would-be callee may actually be a
  subexpression in a larger expression, so not actually a callee.}

)

 The result of expansion can be either a single group or a group for the
 parsed result and a group for the remaining tail. The resulting parsed
 group can be an immediate callee form or an expression form, where the
 latter is assumed if the result does not start with an identifier
 (possibly implicit) that is bound as an immediate callee macro. If a
 tail is returned, then the parsed result is further parsed as a
 delimited group, otherwise it is (re-)parsed as a continuation of the
 form that contains the immediate callee.

@examples(
  ~eval: macro_eval
  ~defn:
    immediate_callee.macro 'enlist $tail ...':
      ~op_stx: all
      ~static_infos: sis
      ~in_op_mode mode
      ~in_op_stx op
      if expr_meta.ends_parse(mode, op, '$tail ...')
      | // parse as callee
        let res = annot_meta.pack_predicate('fun (x): #true',
                                            sis[0])
        values('fun (x) :~ List.of($res): [x]',
               '$tail ...')
      | // parse as expression
        'dynamic(enlist) $tail ...'
  ~repl:
    use_static
    ("apple" |> enlist)[0][0]
)

}


@doc(
  syntax_class immediate_callee_meta.Parsed(static_infoss, op_mode, op_stx):
    kind: ~group
    fields:
      group
){

 @provided_meta()

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), but for parsing
 immediate callees. The @rhombus(static_infoss) argument should be a list
 of syntax object unpacked static information. The @rhombus(op_mode)
 argument and @rhombus(op_stx) arguments specify the operator that
 triggered the parse, where @rhombus(op_mode) is @rhombus(#'prefix) or
 @rhombus(#'infix) and @rhombus(os_stx) is a name.

}


@(macro.close_eval(macro_eval))
