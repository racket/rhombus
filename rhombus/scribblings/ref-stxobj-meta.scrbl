#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@title(~tag: "stxobj-macro"){Syntax Objects in Macros}

@doc(
  fun Syntax.meta_value(id :: Syntax, fail = ....)
){

 Returns the compile-time value of @rhombus(id), if available. If not
 available, then @rhombus(fail) is called if it is a procedure of 0
 arguments, otherwise @rhombus(fail) is returned.

 The default @rhombus(fail) is a procedure that raises an exception.

}


@doc(
  fun Syntax.error(at_stx :: Syntax),
  fun Syntax.error(message :: String, at_stx :: Syntax),
  fun Syntax.error(message :: String, in_stx :: Syntax, at_stx :: Syntax)
){

 Raises a syntax-error message concerning @rhombus(at_stx). If
 @rhombus(message) is not provided, the message is @rhombus("bad syntax").
 If @rhombus(in_stx) is provided, it should be something like an enclosing
 form of @rhombus(at_stx) to provide more context.

}
