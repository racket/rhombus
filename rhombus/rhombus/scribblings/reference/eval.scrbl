#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Eval}

An @deftech{evaluator} is an interactive evaluation context. Each
evaluator has its own set of bindings and, potentially, its own set of
module declarations. A Rhombus read-eval-print loop (REPL) is backed
by an evaluator, for example.

@doc(
  fun eval(seq :: Syntax,
           ~as_interaction: as_interaction :: Any = #false)
){

 Evaluates a term, group, or multi-group sequence @rhombus(seq) in the
 current @tech{evaluator}.

 If @rhombus(as_interaction) is true, then @rhombus(seq) is evaluated as
 if in a read-eval-print loop. Depending on the bindings of the current
 evaluator (as determined by @rhombus(Evaluator.current)), that can be
 different that evaluating non-interaction top-level forms.

 A suitable evaluator must have been created and installed as the value
 of the @rhombus(Evaluator.current) context parameter, since the default
 evaluator is empty.

@examples(
  parameterize { Evaluator.current: Evaluator.make_rhombus() }:
    eval('1 + 2')
)

}

@doc(
  annot.macro 'Evaluator'
){

 Represents an evaluator for interactive evaluation via @rhombus(eval).
 An evaluator is installed as the current one using
 @rhombus(Evaluator.current), usually via @rhombus(parameterize).

}

@doc(
  fun Evaluator.make_rhombus() :: Evaluator
){

 Creates a fresh evaluator with @rhombuslangname(rhombus) imported.

}

@doc(
  fun Evaluator.make_rhombus_empty() :: Evaluator
){

 Creates a fresh evaluator with the @rhombuslangname(rhombus) module
 attached, but not imported.

}

@doc(
  fun Evaluator.import(mod :: ModulePath) :: Void
){

 Imports @rhombus(mod) into the current evaluator, even if the @rhombus(import)
 definition form is not available in the evaluator itself.

}

@doc(
  fun Evaluator.instantiate(mod :: ModulePath,
                            export_name :: maybe(Symbol) = #false) :: Any
){

 In the current evaluator, loads @rhombus(mod) if it is not loaded
 already, and instantiates the run-time component of the loaded module if
 it is not instantiated already.

 If @rhombus(export_name) is @rhombus(#false), then the result is
 @rhombus(#void). Otherwise, the value exported by @rhombus(mod) as
 @rhombus(export_name) is returned, or an exception is thrown if no such
 export is available.

}

@doc(
  fun Evaluator.module_is_declared(mod :: ModulePath,
                                   ~load: load = #false) :: Boolean
){

 Reports whether @rhombus(mod) is declared in the current evaluator. If
 @rhombus(load) is true, then the module may be loaded in the process of
 resolving @rhombus(mod), but no exception is thrown if @rhombus(mod)
 refers to a @rhombus(submodule) that does not exist (either because the
 enclosing module cannot be found or loaded, or because no submodule is
 declared within using the name in @rhombus(mod)).

}

@doc(
  Parameter.def Evaluator.current :: Evaluator
){

 A @tech{context parameter} for the current evaluator.

}
