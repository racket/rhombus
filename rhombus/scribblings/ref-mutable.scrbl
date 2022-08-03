#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Mutable Variables and Assignment}

@doc(
  bind.macro 'mutable $identifier'
){

 Binds @rhombus(identifier) so that its vaue can be changed using
 @rhombus(:=).

 No static information is associated with @rhombus(identifier), even if
 a surrounding binding pattern would otherwise associate static
 information with it.

}

@doc(
  expr.macro '$identifier := $expr'
){

 Changes the value of @rhombus(identifier) to the result of
 @rhombus(expr). The @rhombus(identifier) must be bound with
 @rhombus(mutable, ~bind).

@examples(
  val mutable count: 0,
  count := count + 1,
  count
)

}
