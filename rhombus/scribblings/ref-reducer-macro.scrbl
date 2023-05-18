#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Reducer Macros}

@doc(
  space.enforest reducer
){

 The @tech{space} for bindings of identifiers that can be used in
 @tech{reducer} positions, such as within @rhombus(for).

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro

  defn.macro 'reducer.macro $prefix_macro_patterns'
){

 Like @rhombus(defn.macro, ~expr), but defines an identifier
 as a reducer form in the @rhombus(reducer, ~space) @tech{space}.
 The result of the macro expansion can be a low-level
 reducer description created with @rhombus(reducer_meta.pack).

@examples(
  ~eval: macro_eval
  ~defn:
    reducer.macro 'Array5':
      'Array ~length 5'
  ~repl:
    for Array5:
      each i: 0..3
      i
  ~defn:
    reducer.macro 'WithCount($(r :: reducer_meta.Parsed))':
      def '($wrap, ($accum_id = $init, ...), $step, $static_info, $data)':
        reducer_meta.unpack(r)
      reducer_meta.pack('build_return',
                        '($accum_id = $init, ..., count = 0)',
                        'build_inc',
                        '()',
                        '[($accum_id, ...), count, $wrap, $step, $data]')
    expr.macro 'build_return [($accum_id, ...), $count, $wrap, $step, $data] $e':
      'block:
         let values($accum_id, ..., c) = $e
         values($wrap $data $accum_id ..., c)'
    expr.macro 'build_inc [($accum_id, ...), $count, $wrap, $step, $data] $e':
      'block:
         let ($accum_id, ...) = $step $data $e
         values($accum_id, ..., $count + 1)'
  ~repl:
    for WithCount(List):
      each i: 0..3
      i
    for WithCount(Map):
      each i: 0..3
      values(i, "val" +& i)
)

}

@doc(
  fun reducer_meta.pack(stx :: Syntax) :: Syntax
){

 @provided_meta()

 Packs reducer information that is represented by a syntax object with
 five parts, which are combined in the form

 @rhombusblock(
  '(#,(@rhombus(complete_id, ~var)),
    (#,(@rhombus(accum_id, ~var)) = #,(@rhombus(accum_expr, ~var)), ...),
    #,(@rhombus(step_id, ~var)),
    ((#,(@rhombus(var_static_key, ~var)), #,(@rhombus(var_static_value, ~var))), ...),
    #,(@rhombus(data, ~var)))')

 As an example, for the @rhombus(List, ~reducer),
 @rhombus(complete_id, ~var) reverses an accumulated list, one
 @rhombus(accum_id, ~var) is initialized to @rhombus([]) and represents
 an accumulated (in reverse) list, @rhombus(step_id, ~var) adds a new
 value to the front of the list, the @rhombus(var_static_key, ~var)s with
 @rhombus(var_static_values, ~var)s provide static information for a
 list, and @rhombus(data, ~var) is @rhombus(accum_id, ~var) (so that
 @rhombus(step_id, ~var) is able to refer to it).

 In detail:

@itemlist(

 @item{The @rhombus(complete_id, ~var) should refer to a macro that
  expects @rhombus(data, ~var) followed by an expression that produces a
  final value for each @rhombus(accum_id, ~var), and the result of that
  macro use is the overall accumulated result (e.g., the result of the
  @rhombus(for) form using the packed reducer).}

 @item{The @rhombus(accum_id, ~var)s correpond to the state of the
  reducer as it receives a stream of input values. As internal state,
  these identifiers generally should not be directly visible to clients of
  the reducer. The @rhombus(accum_expr, ~var)s determine the initial
  values, the @rhombus(step_id, ~var) determins how each is updated for an
  input. When no further inputs are available, @rhombus(complete_id, ~var)
  receives the final state to convert it in to the result value.}

 @item{The @rhombus(step_id, ~var) should refer to a macro that expects
  @rhombus(data, ~var) followed by an expression that produces a value (or
  multiple values) to be acucmulated. It should produce a number of
  results corresponding to the number of @rhombus(accum_id, ~var)s, and
  each result becomes the new value of the corresponding
  @rhombus(accum_id, ~var).}

 @item{The @rhombus(data, ~var) component is effectively the data half
  of a closure for @rhombus(complete_id, ~var) and
  @rhombus(step_id, ~var). It can have any shape that is needed to provide
  information to @rhombus(complete_id, ~var) and @rhombus(step_id, ~var).
  It often will include the @rhombus(accum_id, ~var)s so that
  @rhombus(step_id, ~var) can refer to them.}

)

 See @rhombus(reducer.macro) for an example.

}

@doc(
  fun reducer_meta.unpack(stx :: Syntax) :: Syntax
){

 @provided_meta()

 The inverse of @rhombus(reducer_meta.pack), which can be useful for
 defining a new form that works with reducers or for defining a reducer
 in terms of another reducer.

}

@doc(
  syntax_class reducer_meta.Parsed:
    kind: ~group
    field group
){

 @provided_meta()

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), but for reducers.

}

@«macro.close_eval»(macro_eval)
