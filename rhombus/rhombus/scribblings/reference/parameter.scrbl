#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "context-parameters"){Context Parameters}

A @deftech{context parameter} is a way of communicating a value from one
evaluation context to another one that is nested dynamically, but not
necessarily lexically. In other words, a context parameter is similar to
a dynamic binding or operating-system environment variable---but a
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
  ~nonterminal:
    id_name: namespace ~defn

  fun Parameter.make(
    initial_val :: Any,
    ~name: name :: Symbol = #'parameter,
    ~guard: guard :: maybe(Function.of_arity(1)) = #false
  ) :: Parameter

  defn.macro 'Parameter.def $id_name $maybe_annot = $expr'
  defn.macro 'Parameter.def $id_name $maybe_annot:
                $option
                ...
                $body
                ...'

  grammar maybe_annot:
    #,(@rhombus(::, ~bind)) $annot
    #,(@rhombus(:~, ~bind)) $annot
    #,(epsilon)

  grammar option:
    ~name $id_name
    ~name: $id_name
){

 The @rhombus(Parameter.make) function creates a @tech{context parameter} whose initial value is
 @rhombus(initial_val) and whose name for debugging purposes as
 @rhombus(name). If @rhombus(guard) is not @rhombus(#false), then it used
 as a filter on any candidate value for the function, whether that value
 is supplied via @rhombus(parameterize) or by calling the context
 parameter as a function with one argument. The @rhombus(guard) filter
 is @emph{not} applied to @rhombus(initial_val).

 A context parameter acts as a function that returns the parameter's value
 in the current dynamic context when it is called with zero arguments,
 and it mutates the parameter's value when called with one argument.

 The @rhombus(Parameter.def) form defines @rhombus(id_name) to a new
 @tech{context parameter} whose initial value is produced by
 @rhombus(expr) or a @rhombus(body) sequence. If an annotation is provided
 using @rhombus(::, ~bind), then the annotation's predicate or conversion
 is used for a filter argument like the @rhombus(guard) function for
 @rhombus(Parameter.make). If an annotation is provided with either
 @rhombus(::, ~bind) or @rhombus(:~, ~bind), static information from
 the annotation is associated with the result of calling @rhombus(id_name).

 The @rhombus(~name) option in @rhombus(Parameter.def) is like the
 @rhombus(~name) option for @rhombus(fun, ~defn).

@examples(
  ~defn:
    def size = Parameter.make(10)
  ~repl:
    size()
    size(11)
    size()
  ~defn:
    Parameter.def color :: String = "red"
  ~repl:
    use_static
    color().length()
    ~error:
      color(5)
    ~error:
      parameterize { color: 5 }:
        "ok"
)

}

@doc(
  ~nonterminal:
    parameter_expr: block expr
    val_expr: block expr
    val_body: block body
  expr.macro 'parameterize { $parameter_expr:
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
