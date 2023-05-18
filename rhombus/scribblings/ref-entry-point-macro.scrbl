#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm":
      open
      except entry_point
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Entry Point Macros}

@doc(
  space.enforest entry_point
){

 The @tech{space} for bindings of identifiers that can be used in
 @tech{entry point} positions, such as within
 @rhombus(method, ~class_clause).

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro
    mode_id: block id
    adj_id: block id

  defn.macro 'entry_point.macro $prefix_macro_patterns'

  grammar option:
    ~op_stx: $id
    ~op_stx $id
    ~mode: $mode_id
    ~mode $mode_id
    ~adjustment '$adj_id'
    ~adjustment: '$adj_id'

){

 Like @rhombus(defn.macro, ~expr), but defines an identifier as an
 @tech{entry point} form in the @rhombus(entry_point, ~space)
 @tech{space}. Also, in addition to the @rhombus(~op_stx) option, the
 @rhombus(~mode) and/or @rhombus(~adjustment) ``options'' can be
 specified---and they are effectively required to detect the mode mode of
 expansion and receive potential adjustments to the expansion.
 
 An entry-point macro works in two modes, where the symbol provided as
 @rhombus(mode_id) indicates the mode:

@itemlist(

 @item{@rhombus(#'arity): gets an encoding of the arity that the
  generated function will have, not counting any antra arguments that
  might be added through an adjustment. The encoding for an arity is
  explained below. The @rhombus(adj_id) specified by @rhombus(~adjustment)
  is bound to @rhombus(#false). This mode of expansion is sometimes used
  before the @rhombus(#'function) mode.}

 @item{@rhombus(#'function): gets a function to implement the entry
  point. The result can be another entry point, or it can be a procedure
  implementation that is packed via @rhombus(entry_point_meta.pack). In
  the latter case, the function should be potentially adjusted with extra
  leading arguments and a wrapper for the body, where the adjustments are
  provided via a @rhombus(entry_point_meta.Adjustment) value for
  @rhombus(adj_id). Adjusts might add a ``self'' argument for a method,
  for example, and wrap a body to bind names for direct access to object
  fields and methods.}

)

 The result of expansion in @rhombus(#'arity) mode must be either
 @rhombus(#false), an integer, or a list of three elements. A
 @rhombus(#false) means that the arity is not statically known. An
 integer value is an arity mask; for every bit set in the mask, the
 function can receives that many by-position arguments. A list value
 starts with an arity for by-position arguments, but also has a list of
 keywords that are allowed and a list of keywords that are required; the
 list of allowed keywords can be @rhombus(#false) to indicate athat any
 keyword is allowed. The context of an entry point may constrain the
 acceptable arities or use arity information for a more efficient
 expansion.

 When the rule of expansion in @rhombus(#'function) mode is packed via
 @rhombus(entry_point_meta.pack), the generated function shuld accept
 extra initial by-position arguments as listed in
 @rhombus(entry_point_meta.Adjustment.prefix_arguments(adj_id)), and each
 result body (where the function may have multiple bodies in multiple
 cases) should be wrapped with
 @rhombus(entry_point_meta.Adjustment.wrap_body(adj_id)). The wrapping
 function expects an arity encoding that is like the result for
 @rhombus(#'arity) mode expansion, but specific to the body's case within
 a multi-case function.

@examples(
  ~eval: macro_eval
  ~defn:
    entry_point.macro 'identity':
      ~mode mode
      ~adjustment adj
      if (mode == #'arity)
      | [2, [], []]
      | def [id, ...] = adj.prefix_arguments
        entry_point_meta.pack('fun($id, ..., x):
                                 $(adj.wrap_body(2, 'x'))')
  ~repl:
    class C():
      method m: identity
    C().m("ok")
)

}

@doc(
  class entry_point_meta.Adjustment(
    arity :: (False || Int
                || matching([mask :: Int,
                             allowed :: List.of(Keyword) || False,
                             required :: List.of(Keyword)])),
    wrap_body :: Function.of_arity(2),
    is_method :: Boolean
  )
){

 @provided_meta()

 Represents an adjustment to an entry point to add extra arguments and
 wrap the generated function's body. The @rhombus(wrap_body) function
 expects an arity encoding (see @rhombus(entry_point.macro)) and a syntax
 object, and it produce a syntax object. The @rhombus(is_method) field
 indicates whether the existence of a leading argument should be hidden
 in error messages.

}

@doc(
  fun entry_point_meta.pack(stx :: Syntax) :: Syntax
){

 @provided_meta()

 Packs an expression for a function as a result for a
 @rhombus(entry_point.macro) expansion, distinguishing it from an
 unpacked syntax objects that represents expansion to another @tech{entry
  point} form.

}

@doc(
  syntax_class entry_point_meta.Parsed(adj :: Adjustment):
    kind: ~group
    field group
  syntax_class entry_point_meta.Arity:
    kind: ~group
    field group
){

 @provided_meta()

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), but for entry
 points to run in either @rhombus(#'function) mode with
 @rhombus(entry_point_meta.Parsed) or @rhombus(#'arity) mode via
 @rhombus(entry_point_meta.Arity).

}

@«macro.close_eval»(macro_eval)
