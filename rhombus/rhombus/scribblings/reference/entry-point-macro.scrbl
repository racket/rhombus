#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@(def dollar = @rhombus($))

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
    prefix_macro_patterns: defn.macro ~defn
    mode_id: block id
    adj_id: block id

  defn.macro 'entry_point.macro $prefix_macro_patterns'

  grammar option:
    ~op_stx: $id
    ~op_stx $id
    ~mode: $mode_id
    ~mode $mode_id
    ~adjustment: $adj_id
    ~adjustment $adj_id

){

 Like @rhombus(defn.macro), but defines an identifier as an
 @tech{entry point} form in the @rhombus(entry_point, ~space)
 @tech{space}. Also, in addition to the @rhombus(~op_stx) option, the
 @rhombus(~mode) and/or @rhombus(~adjustment) ``options'' can be
 specified---and they are effectively required to detect the mode mode of
 expansion and receive potential adjustments to the expansion.

 An entry-point macro works in two modes, where the symbol provided as
 @rhombus(mode_id) indicates the mode:

@itemlist(

 @item{@rhombus(#'shape): gets an encoding of information about the
  to-be-generated function as a @tech{map}. The @rhombus(adj_id) specified
  by @rhombus(~adjustment) is bound to @rhombus(#false). This mode of
  expansion is sometimes used before the @rhombus(#'function) mode.

  The map can have any or all of the following keys:

  @itemlist(

   @item{@rhombus(#'arity): The arity of the generated function, not
   counting any extra arguments that might be added through an adjustment.
   The encoding for an arity is explained below.}

   @item{@rhombus(#'name): A symbol for the function's name for error
   reporting and other run-time purposes.}

 )}

 @item{@rhombus(#'function): gets a function to implement the entry
  point. The result can be another entry point, or it can be a function
  implementation that is packed via @rhombus(entry_point_meta.pack). In
  the latter case, the function should be potentially adjusted with extra
  leading arguments and a wrapper for the body, where the adjustments are
  provided via a @rhombus(entry_point_meta.Adjustment, ~class) value for
  @rhombus(adj_id). Adjustments might add a ``self'' argument for a method,
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
 list of allowed keywords can be @rhombus(#false) to indicate that any
 keyword is allowed. The context of an entry point may constrain the
 acceptable arities or use arity information for a more efficient
 expansion.

 When the rule of expansion in @rhombus(#'function) mode is packed via
 @rhombus(entry_point_meta.pack), the generated function should accept
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
      match mode
      | #'shape:
          { #'arity: [2, [], []] }
      | #'function:
          let [arg, ...] = adj.prefix_arguments
          entry_point_meta.pack(
            'fun ($arg, ..., x):
               $(adj.wrap_body(2, 'x'))'
          )
  ~repl:
    class C():
      method m: identity
    C().m("ok")
)

}

@doc(
  ~meta
  class entry_point_meta.Adjustment(
    name :: maybe(Symbol),
    prefix_arguments :: Listable.to_list && List.of(Identifier),
    wrap_body :: Function.of_arity(2),
    is_method :: Boolean
  )
){

 Represents an adjustment to an entry point to potentially specify a name
 (to be used for run-time reporting), to add extra arguments, and to wrap
 the generated function's body. The @rhombus(wrap_body) function
 expects an arity encoding (see @rhombus(entry_point.macro)) and a syntax
 object, and it produces a syntax object. The @rhombus(is_method) field
 indicates whether the existence of a leading argument should be hidden
 in error messages.

}

@doc(
  ~meta
  fun entry_point_meta.pack(stx :: Syntax) :: Syntax
  fun entry_point_meta.unpack(stx :: Syntax) :: Syntax
  fun entry_point_meta.pack_shape(shape :: Map) :: Syntax
  fun entry_point_meta.unpack_shape(stx :: Syntax) :: Map
){

 The @rhombus(entry_point_meta.pack) function packs an expression for a
 function as a result for a @rhombus(entry_point.macro) expansion,
 distinguishing it from an unpacked syntax objects that represents
 expansion to another @tech{entry point} form. The
 @rhombus(entry_point_meta.unpack) function is the inverse of
 @rhombus(entry_point_meta.pack).

 The @rhombus(entry_point_meta.pack_shape) and
 @rhombus(entry_point_meta.unpack_shape) functions similarly handle
 encodings of entry-point shape results. An entry point macro should
 @emph{not} explicitly pack its result with
 @rhombus(entry_point_meta.pack_shape), but these functions can be useful
 when using the @rhombus(entry_point_meta.Shape, ~stxclass) syntax class.

}

@doc(
  ~meta
  syntax_class entry_point_meta.Parsed(adj):
    kind: ~group
    fields:
      group
  syntax_class entry_point_meta.Shape:
    kind: ~group
    fields:
      group
){

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), but for entry
 points to run in either @rhombus(#'function) mode with
 @rhombus(entry_point_meta.Parsed, ~stxclass) or @rhombus(#'shape) mode via
 @rhombus(entry_point_meta.Shape, ~stxclass).

}


@(macro.close_eval(macro_eval))
