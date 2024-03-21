#lang scribble/rhombus/manual

@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@title{Map Configurations}

@doc(
  ~nonterminal:
    equals_proc_expr: block expr
    hash_code_proc_expr: block expr

  defn.macro '«key_comp.def '$op_or_id_name':
                 $ref_by_clause
                 ...
                 »'
  grammar ref_by_clause:
    ~equals: $equals_proc_expr
    ~hash_code: $hash_code_proc_expr
){

 Defines @rhombus(op_or_id_name) as a map configuration like
 @rhombus(===, ~key_comp) and for use with forms like @rhombus(Map.by).

 The result of @rhombus(equal_proc_expr) should be a procedure that
 takes three arguments: two values to compare, and a function to use for
 recursive comparison.

 The result of @rhombus(hash_code_proc_expr) should be a procedure that
 takes two arguments: the value to hash, and a function to use for
 recursive hashing.

}
