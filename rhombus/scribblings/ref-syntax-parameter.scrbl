#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@title(~tag: "ref-syntax-parameter"){Syntax Parameters}

A @deftech{syntax context parameter} (or just @deftech{syntax
 parameter}) is analogous to a @tech{context parameter}, but for
macro-expansion contexts instead of evaluation contexts. While a context
parameter communicates to a more nested evaluation context, even if that
context is not lexically nested, a syntax parameter communicates to a
more nested expansion position, where nested expression may have been
macro-introduced instead of being lexiecally nested in the original
source. For example, @rhombus(this) is implemented through a syntax
parameter that allows its meaning to be adapted to an enclosing method,
even when a use of @rhombus(this) is macro-introduced in the method
body.

A syntax parameter is not useful on its own, but it can work in concert
with one or more space-specific macros that use it. Bind a new syntax
parameter using @rhombus(syntax_parameter.bridge), and adjust a syntax
parameter's binding in the remainder of a definition context using
@rhombus(syntax_parameter.relet).

@doc(
  defn.macro '«syntax_parameter.bridge $id_name:
                 $body
                 ...»'
){

 Defines a new @tech{syntax parameter} as @rhombus(id_name). The result
 of the expand-time @rhombus(body) sequence is the initial compile-time
 value of the syntax parameter.

@examples(
  ~eval:
    macro_eval
  ~defn:
    syntax_parameter.bridge today_name:
      "Thursday"
    expr.macro 'today':
      '$(syntax_parameter_meta.lookup('today_name'))'
  ~defn:
    fun say_today():
      today
    expr.macro 'say_today_here':
      'today'
  ~repl:
    today
  ~repl:
    block:
      syntax_parameter.relet today_name:
        "Friday"
      [today,
       say_today(),
       say_today_here]
    today
)

}

@doc(
  defn.macro '«syntax_parameter.relet '$id_name':
                 $body
                 ...»'
){

 Binds the @tech{syntax parameter} @rhombus(id_name) (which must be
 defined already) to the compile-time result of the @rhombus(body)
 sequence. The binding applies to all subsequent forms in the enclosing
 definition context, hence it's a ``re-let'' of the binding.

 See @rhombus(syntax_parameter.bridge) for an example.

}

@macro.close_eval(macro_eval)
