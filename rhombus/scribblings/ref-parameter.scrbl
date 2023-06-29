#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "context-parameters"){Context Parameters}

A @deftech{context parameter} is a way of communicating a value from one
evaluation context to another one that is nested dynamically, but not
necessarily lexically. In other words, a context parameter is similar to
a dyanmic binding or operating-system environment variable---but a
context parameter is a value, not a variable. Create a context parameter
using @rhombus(Parameter.make), get a context parameter's value by
calling the parameter as a function of zero arguments, and set a context
parameter's value using @rhombus(parameterize).

@doc(
  annot.macro 'Parameter'
){

 Matches @tech{context parameters}.

 A context parameter also satisfies @rhombus(Function, ~annot) and
 @rhombus(Function.of_arity(0, 1), ~annot).

}

@doc(
  fun Parameter.make(initial_val,
                     ~name name :: Symbol = #'parameter,
                     ~guard guard :: maybe(Function.of_arity(1)) = #false)
    :: Parameter
){

 Creates a @tech{context parameter} whose initial value is
 @rhombus(initial_val) and whose name for debugging purposes as
 @rhombus(name). If @rhombus(guard) is not @rhombus(#false), then it used
 as a filter on any candidate value for the function, whether that value
 is supplied via @rhombus(parameterize) or by calling the context
 parameter as a function with one argument.

 A context parameter acts a function that returns the parameter's value
 in the current dynamic context when it is called with zero arguments,
 and it mutates the parameter's value when called with one argument.

@examples(
  ~defn:
    def size = Parameter.make(10)
  ~repl:
    size()
    size(11)
    size()
)

}

@doc(
  ~nonterminal:
    parameter_expr: block expr
    val_expr: block expr
    val_body: block body
  expr.macro 'parameterize {  $parameter_expr:
                                $val_body
                                ...,
                              ... }:
                $body
                ...'
){

 Returns the result of the @rhombus(body) block as evaluated in a fresh
 parameterization that binds the @tech{context parameter} produced by
 each @rhombus(parameter_expr) to the value produced by the corresponding
 @rhombus(val_body) sequence.

 If a context parameter for a @rhombus(parameter_expr) is mutated during
 the evaluation of @rhombus(body), the mutation is not preserved
 outside of the evaluation of the @rhombus(body). That is,
 context-parameter mutation affects a particular parameterization, not
 the context parameter more globally.

@examples(
  ~defn:
    def size = Parameter.make(10)
  ~repl:
    size()
    parameterize { size: 11 }:
      size()
    parameterize { size: 12 }:
      size(13)
      size()
    size()
)

}
