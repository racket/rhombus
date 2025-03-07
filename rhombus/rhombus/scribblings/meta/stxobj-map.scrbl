#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/syntax_map open)

@title{Syntax Object Maps}

@docmodule(rhombus/syntax_map)

@doc(
  fun equal_name(stx1 :: Name, stx2 :: Name) :: Boolean
  key_comp.def 'equal_name'
){

 Reports whether @rhombus(stx1) and @rhombus(stx2) are the same names
 symbolically---that is, whether @rhombus(Syntax.unwrap_all) returns the
 same result---independent of whether bindings are the same.

 @rhombus(Syntax.equal_names, ~key_comp) can also be used as a
 @nontermref(key_comp) for @rhombus(Map.by) and similar forms.

@examples(
  ~hidden:
    import: rhombus/syntax_map open
  ~repl:
    equal_name('apple', 'apple')
    equal_name('fruit.apple', 'apple')
    equal_name('fruit.apple', 'fruit.apple')
  ~version_and_later "8.13.0.2":
    ~repl:
      def snacks = Map.by(equal_name){ 'apple': 1 }
      snacks['apple']
)

}

@doc(
  ~meta
  key_comp.def 'equal_binding'
  key_comp.def 'equal_name_and_scopes'
){

 Equality and hashing configuration for use with @rhombus(Map.by) and
 similar, where keys are compared using
 @rhombus(syntax_meta.equal_binding) and
 @rhombus(syntax_meta.equal_name_and_scopes), respectively.

}
