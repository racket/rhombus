#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm":
      except: defn expr
      open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Definition Macros}

@doc(
  space.transform defn
){

 The @tech{space} for bindings of identifiers that can be used in
 definition positions.

}

@doc(
  def defn_meta.space :: SpaceMeta
){

@provided_meta()

 A compile-time value that identifies the same space as
 @rhombus(defn, ~space). See also @rhombus(SpaceMeta, ~annot).

}

@doc(
  defn.macro 'defn.macro $prefix_macro_patterns'

  grammar prefix_macro_patterns:
    '$defined_name $pattern ...':
      $option; ...
      $body
      ...
    Z| '$defined_name $pattern ...':
         $option; ...
         $body
         ...
     | ...

  grammar defined_name:
    $id
    $op
    #,(dollar)('#,(dollar)')
    ($id_name)
    ($op_name)

  grammar option:
    ~op_stx: $id
    ~op_stx $id
){

 Defines @rhombus(defined_name) as a macro
 in the @rhombus(expr, ~space) @tech{space}. The macro can be used
 in a definition context, where the compile-time
 @rhombus(body) block returns the expansion result. The macro pattern is
 matched to an entire group in a definition context.

 The expansion result must be a sequence of groups that are inlined in
 place of the definition-macro use.

 The @rhombus(option)s in the macro body must be distinct; since there
 is only one option currently, either the @rhombus(~op_stx) option is
 present or not. The @rhombus(~op_stx) option, if present, binds an
 identifier for a use of the macro (which cannot be matched directly in
 the @rhombus(pattern), since that position is used for the name
 that @rhombus(defn.macro) binds).

 Using @vbar alternatives, a single definition can have any number of
 @rhombus(pattern)s, which are tried in order. The @rhombus(defined_name)
 must be the same across all cases.

@examples(
  ~eval: macro_eval
  defn.macro 'enum:
                $(id :: Group)
                ...':
    def [n, ...]: List.iota([id, ...].length())
    'def $id: $n
     ...'
  enum:
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

 Similar to @rhombus(defn.macro, ~expr), but defines a macro for a definition
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
    def seq_x: seq_y+1
    def seq_y: 10
    seq_x
)
}

@doc(
  syntax_class defn_meta.Group:
    kind: ~group
){

 @provided_meta()

 Syntax class that matches only groups that start with an identifier
 that is bound as a definition form.

 Unlike @rhombus(expr_meta.Parsed), @rhombus(defn_meta.Group) does not
 parse the definition form, because the form normally needs to be parsed
 within a definition context. This syntax class can be used to
 distinguish definitions from other forms that need to be treated
 differently in some context.

}

@doc(
  syntax_class defn_meta.SequenceStartGroup:
    kind: ~group
){

 @provided_meta()

 Syntax class that matches only groups that starts with an identifier
 that is bound as a definition-sequence form.

}


@doc(
  fun defn_meta.pack_s_exp(tree) :: Syntax
){

@provided_meta()

 Similar to @rhombus(expr_meta.pack_s_exp), but for definitions.


}


@«macro.close_eval»(macro_eval)
