#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dollar = @rhombus($))

@title{Aliases}

@doc(
  ~nonterminal:
    new_op_or_id_name: namespace op_or_id_name ~defn
    orig_op_or_id_name: namespace op_or_id_name ~defn

  defn.macro '«alias '$new_op_or_id_name':
                 $option
                 '$orig_op_or_id_name'»'

  grammar option:
    ~only_space $space ...
    ~only_space:
      $space ...
      ...
    ~except_space $space ...
    ~except_space:
      $space ...
      ...
){

 Defines @rhombus(new_op_or_id_name) to be an alias for
 @rhombus(orig_op_or_id_name). By default, the new name is bound in all
 @tech(~doc: meta_doc){spaces} where the original name is bound (before
 the @rhombus(alias, ~defn) form). The original name must be bound in
 some space.

 If @rhombus(~only_space) is provided, then the new name is bound only
 in the listed spaces, and the original name must be bound in each of
 those spaces. If @rhombus(~except_space) is provided, then the new name
 is not bound in any of the listed spaces. Only one of
 @rhombus(~only_space) or @rhombus(~except_space) can be provided.

@examples(
  ~repl:
    alias 'Roster': 'List'
    Roster[1, 2, 3]
    [1, 2, 3] is_a Roster
  ~repl:
    alias '%': 'mod'
    5 % 2
)

}
