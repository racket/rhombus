#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@title{Export Macros}

@doc(
  space.enforest expo
){

 The @tech{space} for bindings of identifiers that can be used within
 @rhombus(export) for exports and export modifiers.

}

@doc(
  ~nonterminal:
    macro_patterns: expr.macro ~defn

  defn.macro 'expo.macro $macro_patterns'
){

 Like @rhombus(expr.macro), but defines a macro for use in
 @rhombus(export).

@examples(
  ~eval: macro_eval
  ~defn:
    expo.macro 'range $(from :: Int) .. $(to :: Int): $id':
      'names:
         $(for List:
             each i: from.unwrap() .. to.unwrap()
             Syntax.make_id(id +& i, id))'
  ~repl:
    namespace ns:
      export: range 1..4: cat
      def cat1 = "Felix"
      def cat2 = "Tom"
      def cat3 = "Scratchy"
    ns.cat3
)

}


@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn

  defn.macro 'expo.modifier $prefix_macro_patterns'

  grammar option:
    ~op_stx: $id
    ~op_stx $id
    ~export: $imp_id
    ~export $imp_id
){

 Like @rhombus(defn.macro), but defines an identifier as an
 export modifier, and an optional @rhombus(~export) declaration provides
 an @rhombus(imp_id) to be bound to the (opaque) export that is being
 modified.

@examples(
  ~eval: macro_eval
  ~defn:
    expo.modifier 'not_greetings':
      'except: hello hi'
  ~repl:
    namespace n:
      def hello = "hello"
      def hi = "hi"
      def bye = "bye"
      export:
        not_greetings: all_defined
    n.bye
    ~error:
      n.hello
)

}


@doc(
  syntax_class expo_meta.Parsed:
    kind: ~group
    fields:
      group
  syntax_class expo_meta.AfterPrefixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class expo_meta.AfterInfixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
){

 @provided_meta()

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass),
 @rhombus(expr_meta.AfterPrefixParsed, ~stxclass), , and
 @rhombus(expr_meta.AfterInfixParsed, ~stxclass), but for exports.

}


@doc(
  syntax_class expo_meta.ParsedModifier(exp):
    kind: ~group
    fields:
      group
){

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), but parses an
 export modifier applied to an export. The result is a parsed modified
 export.

}

@«macro.close_eval»(macro_eval)
