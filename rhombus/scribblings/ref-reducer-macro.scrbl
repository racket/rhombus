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
    macro_patterns: expr.macro ~defn

  defn.macro 'reducer.macro $macro_patterns'
){

 Like @rhombus(expr.macro), but defines an identifier or operator
 as a reducer form in the @rhombus(reducer, ~space) @tech{space}.
 The result of the macro expansion can be a low-level
 reducer description created with @rhombus(reducer_meta.pack).

 This first example simply defines a short-cut for reducing
 into an array of length 5:

@examples(
  ~eval: macro_eval
  ~defn:
    reducer.macro 'Array5':
      'Array ~length 5'
  ~repl:
    for Array5 (i: 0..3): i
)

 Here's an example that sums only the positive numbers that result
 from the loop body, and halts as soon as the sum becomes larger
 than 20. This illustrates the use of @rhombus(step_id, ~var) to create a
 binding that can be used by both the @rhombus(final_id, ~var) and
 @rhombus(step_result_id, ~var) macros.

@examples(
  ~eval: macro_eval
  ~defn:
    reducer.macro 'SumPosTo20':
      reducer_meta.pack(
        'sptt_return',
        '(accum = 0)',
        'sptt_step_defs',
        #false,
        'sptt_final',
        'sptt_step_result',
        '()',
        '[new_accum, accum]'
      )
    expr.macro 'sptt_return [$_, $_] $e':
      'cond
       | $e > 20: "bigger than 20"
       | ~else: $e'
    defn.macro 'sptt_step_defs [$new_accum, $accum] $e':
      'def $new_accum:
         cond
         | $e > 0: $accum + $e
         | ~else: $accum'
    expr.macro 'sptt_final [$new_accum, $_]':
      '$new_accum > 20'
    expr.macro 'sptt_step_result [$new_accum, $_]':
      '$new_accum'
  ~repl:
    for SumPosTo20 (a: [6, -4, 3]): a
    for SumPosTo20 (a: 3..): a
)

 This example shows that reducers can be chained; specifically, it
 creates a @rhombus(WithCount) reducer, that chains onto an existing reducer
 and keeps track of the number of elements while allowing the prior
 reducer to operate normally. The bulk of the code in this example
 is in showing how to explicitly fall through to the macros defined
 in the existing reducer.

@examples(
  ~eval: macro_eval
  ~defn:
    reducer.macro 'WithCount($(r :: reducer_meta.Parsed))':
      let '($wrap, ($bind, ...), $step, $break, $final, $finish, $_, $data)':
        reducer_meta.unpack(r)
      reducer_meta.pack(
        'build_return',
        '($bind, ..., count = 0)',
        'build_inc',
        break.unwrap() && 'build_break',
        final.unwrap() && 'build_final',
        'build_finish',
        '()',
        '[count, $wrap, $step, $break, $final, $finish, $data]'
      )
    expr.macro 'build_return [$_, $wrap, $_, $_, $_, $_, $data] $e':
      'block:
         def values(r, c) = $e
         values($wrap $data r, c)'
    defn.macro 'build_inc [$_, $_, $step, $_, $_, $_, $data] $e':
      '$step $data $e'
    expr.macro 'build_break [$_, $_, $_, $break, $_, $_, $data]':
      '$break $data'
    expr.macro 'build_final [$_, $_, $_, $_, $final, $_, $data]':
      '$final $data'
    expr.macro 'build_finish [$count, $_, $_, $_, $_, $finish, $data]':
      'values($finish $data, $count + 1)'
  ~repl:
    for WithCount(List) (i: 0..3): i
    for WithCount(Map) (i: 0..3): values(i, "val" +& i)
)

}

@doc(
  fun reducer_meta.pack(complete_id :: Identifier,
                        binds :: Syntax,
                        step_id :: Identifier,
                        break_id :: maybe(Identifier),
                        final_id :: maybe(Identifier),
                        step_result_id :: Identifier,
                        static_info :: Syntax,
                        data :: Syntax)
    :: Syntax
){

 @provided_meta()

 Packs reducer information that is represented by a syntax object with
 eight parts. The parts are taken separately by
 @rhombus(reducer_meta.pack), but they are combined in the form

 @rhombusblock(
  '(#,(@rhombus(complete_id, ~var)),    // expression macro
    (#,(@rhombus(accum_id, ~var)) = #,(@rhombus(accum_expr, ~var)), ...),
    #,(@rhombus(step_id, ~var)),        // definition macro
    #,(@rhombus(break_id, ~var)),       // optional expression macro
    #,(@rhombus(final_id, ~var)),       // optional expression macro
    #,(@rhombus(step_result_id, ~var)), // expression macro
    ((#,(@rhombus(var_static_key, ~var)), #,(@rhombus(var_static_value, ~var))), ...),
    #,(@rhombus(data, ~var)))')

 These pieces give the reducer control over how elements of the
 iteration are accumulated on each step and a single completion action
 performed for the accumulated values. Configuration of the step is split
 into four parts---@rhombus(step_id), @rhombus(break_id),
 @rhombus(final_id), @rhombus(step_result_id)---to enable early
 termination of the iteration depending on element values or an
 accumulated value.

 As an example, for the @rhombus(List, ~reducer),
 @rhombus(complete_id, ~var) reverses an accumulated list, one
 @rhombus(accum_id, ~var) is initialized to @rhombus([]) and represents
 an accumulated (in reverse) list, @rhombus(step_id, ~var) adds a new
 value to the front of the list and binds it to a fresh variable
 @rhombus(next_accum_id, ~var), @rhombus(break_id) and @rhombus(final_id)
 are false (because early termination is never needed by the reducer),
 @rhombus(step_result_id) returns @rhombus(next_accum_id, ~var), the
 @rhombus(var_static_key, ~var)s with @rhombus(var_static_value, ~var)s
 provide static information for a list, and @rhombus(data, ~var) has
 @rhombus(accum_id, ~var) and @rhombus(next_accum_id, ~var) (so that
 @rhombus(step_id, ~var) and @rhombus(step_result_id, ~var) are able to
 refer to them).

 In detail:

@itemlist(

 @item{The @rhombus(complete_id, ~var) should refer to a macro that
  expects @rhombus(data, ~var) followed by an expression that produces a
  final value for each @rhombus(accum_id, ~var). The result of that
  macro use is the overall accumulated result (e.g., the result of the
  @rhombus(for) form using the packed reducer).}

 @item{The @rhombus(accum_id, ~var)s correpond to the state of the
  reducer as it receives a stream of input values. As internal state,
  these identifiers generally should not be directly visible to clients of
  the reducer. The @rhombus(accum_expr, ~var)s determine the initial
  values, the @rhombus(step_id, ~var) determines how each is updated for an
  input. When no further inputs are available, @rhombus(complete_id, ~var)
  receives the final state to convert it in to the result value.}

 @item{The @rhombus(step_id, ~var) should refer to a macro that expects
  @rhombus(data, ~var) followed by an expression that produces a value (or
  multiple values) to be accumulated. It should expand to definitions that
  bind whatever is needed by @rhombus(break_id, ~var),
  @rhombus(final_id, ~var), and especially @rhombus(step_result_id, ~var).}

 @item{The optional @rhombus(break_id, ~var) identifier should refer to
  a macro that expects @rhombus(data, ~var) and produces a boolean that
  indicates whether to stop the iteration with the value(s) accumulated
  through previous steps, not accumulating in this step. Supplying
  @rhombus(#false) for @rhombus(break_id) is a hint that breaking is never
  needed before the iteration would otherwise complete, which might
  enable a more efficient compilation.}

 @item{The optional @rhombus(final_id, ~var) identifier should refer to
  a macro that expects @rhombus(data, ~var) and produces a boolean that
  indicates whether to stop the iteration after the accumulation of the
  current step. Like @rhombus(break_id), Supplying @rhombus(#false) for
  @rhombus(final_id) serves as a performance hint.}

 @item{The @rhombus(step_result_id, ~var) should refer to a macro that
  expects @rhombus(data, ~var) and produces a number of results
  corresponding to the number of @rhombus(accum_id, ~var)s. Each result
  becomes the new value of the corresponding @rhombus(accum_id, ~var).}

 @item{The @rhombus(var_static_key, ~var)s with
  @rhombus(var_static_value, ~var)s provide static information for the
  reducer's result.}

 @item{The @rhombus(data, ~var) component is effectively the data half
  of a closure for @rhombus(complete_id, ~var) and
  @rhombus(step_id, ~var). It can have any shape that is needed to provide
  information to @rhombus(complete_id, ~var) and @rhombus(step_id, ~var).
  It often will include the @rhombus(accum_id, ~var)s so that
  @rhombus(step_id, ~var) can refer to them.}

)

 See @rhombus(reducer.macro, ~defn) for an example.

}

@doc(
  fun reducer_meta.unpack(stx :: Syntax) :: Syntax
){

 @provided_meta()

 Roughly the inverse of @rhombus(reducer_meta.pack), except that the
 pieces are returned in a combined syntax object instead of as multiple
 values. Unpacking can be useful for defining a new form that works with
 reducers or for defining a reducer in terms of another reducer.

  See @rhombus(reducer_meta.pack) for an example.

}

@doc(
  syntax_class reducer_meta.Parsed:
    kind: ~group
    fields:
      group
  syntax_class reducer_meta.AfterPrefixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class reducer_meta.AfterInfixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
){

 @provided_meta()

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), etc., but for reducers.

}

@(macro.close_eval(macro_eval))
