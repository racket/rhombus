#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@title{Import Macros}

@doc(
  space.enforest impo
){

 The @tech{space} for bindings of identifiers that can be used within
 @rhombus(import) for imports and import modifiers.

}

@doc(
  ~nonterminal:
    macro_patterns: expr.macro ~defn

  defn.macro 'impo.macro $macro_patterns'
){

 Like @rhombus(expr.macro), but defines a macro for use in
 @rhombus(import).

@examples(
  ~eval: macro_eval
  ~defn:
    impo.macro 'rkt $(name :: Identifier) $('/ $(next :: Identifier)') ...':
      def str = to_string(name).append("/" +& next, ..., ".rkt")
      'lib($(Syntax.make(str, name)))'
  ~repl:
    import: rkt racket/base
    base.add1(1)
)

}


@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn

  defn.macro 'impo.modifier $prefix_macro_patterns'

  grammar option:
    ~op_stx: $id
    ~op_stx $id
    ~import: $imp_id
    ~import $imp_id
){

 Like @rhombus(defn.macro), but defines an identifier as an
 import modifier, and an optional @rhombus(~import) declaration provides
 an @rhombus(imp_id) to be bound to the (opaque) import that is being
 modified.

@examples(
  ~eval: macro_eval
  ~defn:
    impo.modifier 'as_rkt: $(name :: Identifier) ...':
      'rename:
         $name as $(Syntax.make_id("rkt_" +& name, name))
         ...'
  ~repl:
    import: lib("racket/base.rkt"):
              as_rkt: add1 sub1              
    base.rkt_sub1(base.rkt_add1(1))
  ~defn:
    impo.modifier 'expose_as_rkt: $(name :: Identifier) ...':
      ~import imp
      def '$(ex_imp :: impo_meta.ParsedModifier(imp))':
        'expose: $name ...'
      def '$(rn_imp :: impo_meta.ParsedModifier(ex_imp))':
        'as_rkt: $name ...'
      rn_imp
  ~repl:
    import: lib("racket/base.rkt"):
              expose_as_rkt: add1 sub1              
    rkt_sub1(rkt_add1(1))
)

}


@doc(
  syntax_class impo_meta.Parsed:
    kind: ~group
    fields:
      group
  syntax_class impo_meta.AfterPrefixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class impo_meta.AfterInfixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
){

 @provided_meta()

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass),
 @rhombus(expr_meta.AfterPrefixParsed, ~stxclass), , and
 @rhombus(expr_meta.AfterInfixParsed, ~stxclass), but for imports.

}


@doc(
  syntax_class impo_meta.ParsedModifier(imp):
    kind: ~group
    fields:
      group
){

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), but parses an
 import modifier applied to an import. The result is a parsed modified
 import.

}

@«macro.close_eval»(macro_eval)
