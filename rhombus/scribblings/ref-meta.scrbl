#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
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

 See also the @rhombus(meta, ~impmod) import modifier.

@examples(
  ~eval: macro.make_for_meta_eval(),
  meta:
    syntax.class Arithmetic
    | '$x + $y'
    | '$x - $y',
  expr.macro 'right_operand $(exp :: Arithmetic)':
    values(exp.y, ''),
  right_operand 1 + 2
)

}

@doc(
  defn.macro 'meta.bridge $identifier:
                $body
                ...'
){

 Binds @rhombus(identifier) at the enclosing phase, but like a macro,
 where the @rhombus(body) side is a compile-time block at one phase
 greater than the enclosing phase.

 The result of the @rhombus(body) block might be a macro transformer
 that is triggered by a use of @rhombus(identifier), or it might be
 some other kind of value that is accessed with
 @rhombus(Syntax.meta_value).

 For example, forms like @rhombus(expr.macro), @rhombus(bind.rule),
 and @rhombus(annotation.rule) expand to @rhombus(meta.bridge). In
 those cases, the generated @rhombus(body) block produces an
 expression transformer, binding transformer, or annotation
 transformer. Some forms that expand to @rhombus(meta.bridge) enrich
 the @rhombus(identifier) with a scope for a space of bindings, which
 enables overloading a @rhombus(identifier) for different contexts
 like expressions versus bindings. For example,
 @rhombus(annotation.rule) enriches its @rhombus(identifier) with a
 scope for annotation operators.

}
