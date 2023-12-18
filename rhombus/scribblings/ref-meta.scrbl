#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_for_meta_eval())

@title{Meta Definitions and Expressions}

@doc(
  decl.macro 'meta:
                $nestable_body
                ...'
  decl.macro 'meta $nestable_body'
){

 The same as the @rhombus(nestable_body) sequence, but shifted to one phase
 greater. Defintions inside a @rhombus(meta) block can be
 referenced in macro implementations, for example.

 Alternatively, @rhombus(meta) can have a single @rhombus(nestable_body) in a
 group, in which case it is equivalent to a block with the single
 @rhombus(nestable_body).

 See also the @rhombus(meta, ~impo) import modifier.

@examples(
  ~eval: macro_eval
  ~repl:
    meta:
      syntax_class Arithmetic
      | '$x + $y'
      | '$x - $y'
    expr.macro 'right_operand $(exp :: Arithmetic)':
      exp.y
    right_operand 1 + 2
  ~repl:
    meta fun add1(x):
      x+1
    expr.macro 'add1_statically $(n :: Int)':
      '#%literal $(add1(Syntax.unwrap(n)))'
    add1_statically 13
)

}

@doc(
  defn.macro 'meta.bridge $op_or_id_name:
                $nestable_body
                ...'
){

 Binds @rhombus(op_or_id_name) at the enclosing phase, but like a macro,
 where the @rhombus(nestable_body) side is a compile-time block at one phase
 greater than the enclosing phase.

 The result of the @rhombus(nestable_body) block might be a macro transformer
 that is triggered by a use of @rhombus(op_or_id_name), or it might be
 some other kind of value that is accessed with
 @rhombus(syntax_meta.value).

 For example, forms like @rhombus(expr.macro), @rhombus(bind.macro),
 and @rhombus(annot.macro) expand to @rhombus(meta.bridge). In
 those cases, the generated @rhombus(nestable_body) block produces an
 expression transformer, binding transformer, or annotation
 transformer. Some forms that expand to @rhombus(meta.bridge) enrich
 the @rhombus(op_or_id_name) with a scope for a @tech{space} of bindings, which
 enables overloading a @rhombus(op_or_id_name) for different contexts
 like expressions versus bindings. For example,
 @rhombus(annot.macro) enriches its @rhombus(op_or_id_name) with a
 scope for annotation operators.

}


@(macro.close_eval(macro_eval))
