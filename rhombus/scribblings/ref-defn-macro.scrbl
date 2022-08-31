#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(val macro_eval: macro.make_macro_eval())

@title{Definition Macros}

@doc(
  defn.macro '«defn.macro '$identifier_or_operator $pattern ...':
                 $body
                 ...»',

  grammar identifier_or_operator:
    $identifier
    $operator
    ($identifier_path)    
    ($operator_path),

){

 Defines @rhombus(identifier), @rhombus(operator),
 @rhombus(identifier_path), or @rhombus(operator_path) as a macro that
 can be used in a definition context, where the compile-time
 @rhombus(body) block returns the expansion result. The macro pattern is
 matched to an entire group in a definition context.

 The expansion result must be a parenthesized block, and the groups of
 the block are inlined in place of the definition-macro use.

 See @secref("namespaces") for information about
 @rhombus(identifier_path) and @rhombus(operator_path).

@examples(
  ~eval: macro_eval,
  defn.macro 'enum:
                $(id :: Group)
                ...':
    // temporary list library:
    fun | length([]): 0
        | length([a, a1, ...]): 1+length([a1, ...])
    fun | iota_accum(0, acc): acc
        | iota_accum(n, acc): iota_accum(n-1, List.cons(n, acc))
    fun iota(n): iota_accum(n, [])
    // useful example part is here:
    val [n, ...]: iota(length([id, ...]))
    'val $id: $n
     ...',
  enum:
    a
    b
    c,
  b
)

}

@doc(
  defn.macro '«defn.sequence_macro '$identifier_or_operator $pattern ...
                                      $pattern
                                      ...':
                 $body
                 ...»',
  grammar identifier_or_operator:
    $identifier
    $operator
    ($identifier_path)    
    ($operator_path),
){

 Similar to @rhombus(defn.macro), but defines a macro for a definition
 context that is matched against all of the remiaining groups in the
 context, so the pattern is a block pattern.

 The macro result is two values: a parenthesized block of groups to
 splice in place of the sequence-macro use, and a parenthesized block of
 groups that represent the tail of the definition context that was not consumed.

 See @secref("namespaces") for information about
 @rhombus(identifier_path) and @rhombus(operator_path).

@examples(
  ~eval: macro_eval,
  defn.sequence_macro 'reverse_defns
                       $defn1
                       $defn2
                       $tail
                       ...':
    values('$defn2; $defn1', '$tail; ...'),
  :
    reverse_defns
    def seq_x: seq_y+1
    def seq_y: 10
    seq_x
)
}

@«macro.close_eval»(macro_eval)
