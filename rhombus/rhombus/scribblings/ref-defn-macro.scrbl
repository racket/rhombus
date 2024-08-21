#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@(def dollar = @rhombus($))

@title{Definition Macros}

@doc(
  space.transform defn
){

 The @tech{space} for bindings of identifiers that can be used in
 definition positions.

}

@doc(
  ~meta
  def defn_meta.space :: SpaceMeta
){

 A compile-time value that identifies the same space as
 @rhombus(defn, ~space). See also @rhombus(SpaceMeta, ~annot).

}

@doc(
  defn.macro 'defn.macro $prefix_macro_patterns'

  grammar prefix_macro_patterns:
    prefix_macro_case
    Z| prefix_macro_case
     | ...

  grammar prefix_macro_case:
    $prefix_macro_pattern:
      $option; ...
      $body
      ...

  grammar prefix_macro_pattern:
    '$defined_name $pattern ...'

  grammar defined_name:
    $id
    $op
    #,(dollar)('#,(dollar)')
    ($id_name)
    ($op_name)

  grammar option:
    ~op_stx: $id
    ~op_stx $id
    ~all_stx: $id
    ~all_stx $id
){

 Defines @rhombus(defined_name) as a macro
 in the @rhombus(expr, ~space) @tech{space}. The macro can be used
 in a definition context, where the compile-time
 @rhombus(body) block returns the expansion result. The macro pattern is
 matched to an entire group in a definition context.

 The expansion result must be a sequence of groups that are inlined in
 place of the definition-macro use.

 The @rhombus(option)s in the macro body must be distinct. The
 @rhombus(~op_stx) option, if present, binds an identifier for a use
 of the macro (which cannot be matched directly in the
 @rhombus(pattern), since that position is used for the name that
 @rhombus(defn.macro) binds). The @rhombus(~all_stx) option binds an
 identifier to the input that is matched by the
 @rhombus(prefix_macro_pattern), which includes the identifier or
 operator that @rhombus(~op_stx) would capture.

 Using @vbar alternatives, a single definition can have any number of
 @rhombus(pattern)s, which are tried in order. The @rhombus(defined_name)
 must be the same across all cases.

@examples(
  ~eval: macro_eval
  defn.macro 'enumerate:
                $lhs
                ...':
    let [n, ...] = List.iota([lhs, ...].length())
    'def $lhs = $n
     ...'
  enumerate:
    a
    b
    c
  b
)

}

@doc(
  ~nonterminal:
    defined_name: defn.macro ~defn
    option: defn.macro ~defn
  defn.macro '«defn.sequence_macro '$defined_name $pattern ...
                                    $pattern
                                    ...':
                 $option; ...
                 $body
                 ...»'
){

 Similar to @rhombus(defn.macro), but defines a macro for a definition
 context that is matched against all of the remaining groups in the
 context, so the pattern is a multi-group pattern.

 The macro result is two values: a sequence of groups to
 splice in place of the sequence-macro use, and a sequence of
 groups that represent the tail of the definition context that was not consumed.

@examples(
  ~eval: macro_eval
  defn.sequence_macro 'reverse_defns
                       $defn1
                       $defn2
                       $tail
                       ...':
    values('$defn2; $defn1', '$tail; ...')
  :
    reverse_defns
    def seq_x = seq_y+1
    def seq_y = 10
    seq_x
)
}

@doc(
  ~meta
  syntax_class defn_meta.Group:
    kind: ~group
){

 Syntax class that matches only groups that start with an identifier
 that is bound as a definition form.

 Unlike @rhombus(expr_meta.Parsed, ~stxclass), @rhombus(defn_meta.Group, ~stxclass) does not
 parse the definition form, because the form normally needs to be parsed
 within a definition context. This syntax class can be used to
 distinguish definitions from other forms that need to be treated
 differently in some context.

}

@doc(
  ~meta
  syntax_class defn_meta.SequenceStartGroup:
    kind: ~group
){

 Syntax class that matches only groups that starts with an identifier
 that is bound as a definition-sequence form.

}


@doc(
  ~meta
  fun defn_meta.pack_s_exp(tree :: Any) :: Syntax
){

 Similar to @rhombus(expr_meta.pack_s_exp), but for definitions.

}


@(macro.close_eval(macro_eval))
