#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus:
        expose:
          // the "documentation" here for these bindings
          // doesn't have to be related to the actual documentation;
          // they're just convenient names
          fun
          repr
          println
          String
          ReadableString
      rhombus/meta:
        expose:
          expr
          syntax_meta
      rhombus/scribble/tests/string_ext open
      rhombus/draw
      rhombus/cmdline)

@title{Example}

@docmodule(~use_sources: lib("rhombus/private/amalgam.rkt")!core,
           rhombus)

Starting example:

@rhombusblock(
  // should be linked
  repr().length()
  repr().get()
  "Hello".length()
  println().length()
  "Hello".snapshot().length()
  "Hello".copy().length()
  repr().snapshot().length()
  repr().copy().length()
  String.copy("Hello").length()
  String.snapshot("Hello").length()
  rhombus.String.copy("Hello").length()
  String.copy("Hello").snapshot().length()
  "Hello".upcase().length()
  String.more
)

@rhombusblock(
  // also linked
  def s1 = "Hello"
  s1.length()
  def s2 = repr()
  s2.length()
  def s3 = println()
  s2.length()
  s1.copy().length()
  def s4 = println()
  s4.length()
  def s5 = repr().copy()
  s5.length()
  def s6 = String.snapshot("Hello")
  s6.length()
  
  // keep trailing whitespace in above line
  fun (s :: String):
    s.length()
)

@examples(
  println("1\n\n2")
)

@doc(
  defn.macro 'rhombus.fun:
                $apple
                $banana'
  defn.macro 'fun:
                $apple
                $banana'
  fun rhombus.to_string(a,
                        b,
                        c: fun (x):
                             x)
  fun repr() :: String
  method (s :: String).length()
  method String.get()
  fun println() :: ReadableString
  annot.macro 'String'
  annot.macro 'ReadableString':
    ~method_fallback: String
  method String.copy(some_s :: String) :: ReadableString
  method String.snapshot() :: String
  method (some_other_s :: String).upcase() :: String
){

 Description, where @rhombus(some_s.length()) and
 @rhombus(some_other_s.length()) are linked.

 See @rhombus(fun) versus @rhombus(fun, ~defn).

}

@section{Draw}

@docmodule(rhombus/draw)

@doc(
  property (dc :: draw.DC).handle :: Any
  fun draw.DC.from_handle(hand :: Any) :: draw.DC
  enum draw.DC.Fill:
    even_odd
    winding
){

 Draw description.

}

@section{Command Line}

@docmodule(rhombus/cmdline)

@doc(
  annot.macro 'cmdline.String.to_lib_module_path'
){

  Annotation via namespace extension.

}

@section{Extension}

@docmodule(rhombus/scribble/tests/string_ext)

@doc(
  def String.more :: String
){

 Defined by extension.

}

@section{Meta}

@docmodule(rhombus/meta)

@doc(
  defn.macro 'expr.macro'
  fun syntax_meta.DefinitionContext.add_scopes()
){

 The @rhombus(expr.macro) form and
 @rhombus(syntax_meta.DefinitionContext.add_scopes) method.

}
