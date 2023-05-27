#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open
    "nonterminal.rhm":
      open
      except: expr
    "macro.rhm")

@title{Meta Definitions and Expressions}

@doc(
  decl.macro 'meta:
                $body
                ...'
){

 The same as the @rhombus(body) sequence, but shifted to one phase
 greater. Defintions inside a @rhombus(meta) block can be
 referenced in macro implementations, for example.

 See also the @rhombus(meta, ~impo) import modifier.

@examples(
  ~eval: macro.make_for_meta_eval()
  meta:
    syntax_class Arithmetic
    | '$x + $y'
    | '$x - $y'
  expr.macro 'right_operand $(exp :: Arithmetic)':
    exp.y
  right_operand 1 + 2
)

}

@doc(
  defn.macro 'meta.bridge $id:
                $body
                ...'
){

 Binds @rhombus(id) at the enclosing phase, but like a macro,
 where the @rhombus(body) side is a compile-time block at one phase
 greater than the enclosing phase.

 The result of the @rhombus(body) block might be a macro transformer
 that is triggered by a use of @rhombus(id), or it might be
 some other kind of value that is accessed with
 @rhombus(syntax_meta.value).

 For example, forms like @rhombus(expr.macro, ~expr), @rhombus(bind.macro, ~expr),
 and @rhombus(annot.macro, ~expr) expand to @rhombus(meta.bridge). In
 those cases, the generated @rhombus(body) block produces an
 expression transformer, binding transformer, or annotation
 transformer. Some forms that expand to @rhombus(meta.bridge) enrich
 the @rhombus(id) with a scope for a @tech{space} of bindings, which
 enables overloading a @rhombus(id) for different contexts
 like expressions versus bindings. For example,
 @rhombus(annot.macro) enriches its @rhombus(id) with a
 scope for annotation operators.

}
