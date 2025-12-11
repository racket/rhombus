#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus:
        expose:
          // the "documentation" here for these bindings
          // doesn't have to be related to the actual documentation;
          // they're just convenient names
          fun
          method
          property
          repr
          println
          String
          ReadableString
          Port
      rhombus/meta:
        expose:
          expr
          syntax_meta
      rhombus/scribble/tests/string_ext open
      rhombus/draw
      rhombus/cmdline
      rhombus/date
      rhombus/scribble/tests/local open)

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
  class_clause.macro 'method'
  class_clause.macro 'property'
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

 See @rhombus(fun) versus @rhombus(fun, ~defn),
 @rhombus(fun, ~at: rhombus/defn), and @rhombus(fun, ~at rhombus/defn),
 which is the same as @rhombuslink(fun, ~defn){this link},
 @rhombuslink(fun, ~at: rhombus/defn){this link},
 and @rhombuslink(fun, ~at rhombus/defn){this link}.

 Check @rhombuslink(syntax_meta.DefinitionContext.add_scopes){dotted replacement}, too.

 Check @rhombuslink(some_s.length){@bold{unlinked replacement}}, too.

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
  grammar string_more_rhs:
    1
    2
){

 Defined by extension.

}

@doc(
  annot.macro 'String.more'
  grammar string_amore_rhs:
    3
){

 Defined by extension.

}
@section{Meta}

@docmodule(rhombus/meta)

@doc(
  ~nonterminal:
    rhs: String.more string_more_rhs
    rhs2: String.more string_more_rhs ~expr
    rhs3: String.more string_amore_rhs ~at rhombus/annot
    rhs4: String.more string_amore_rhs ~at: rhombus/annot

  defn.macro 'expr.macro'
  fun syntax_meta.DefinitionContext.add_scopes()
){

 The @rhombus(expr.macro) form and
 @rhombus(syntax_meta.DefinitionContext.add_scopes) method.

 Nonterminal links: @rhombus(rhs), @rhombus(rhs2), @rhombus(rhs3),
 @rhombus(rhs4).

}

@section{Nested}

@docmodule(rhombus/date)

@doc(
  class date.Date()
  method (d :: date.Date).to_datetime() :: date.DateTime
  class date.DateTime()
  method (d :: date.DateTime).to_string()
){

 Prefixed and chained @rhombus(date.Date().to_datetime().to_string()).

}


@section{Dotted Paths}

@doc(
  annot.macro 'Port'
  annot.macro 'Port.Output':
    ~method_fallback: Port
  annot.macro 'Port.Output.String':
    ~method_fallback: Port.Output

  method (p :: Port).close()
  method (p :: Port.Output).write_bytes()
  method (p :: Port.Output).open_string() :: Port.Output.String
  method (p :: Port.Output.String).get_string() :: String
){

@rhombusblock(
  def outp = Port.Output.open_string()
  outp.write_bytes(#"x")
  outp.get_string()
  outp.get_string().length()
  outp.close()
)

}

@section{Dotted Paths with Import Prefix}

@doc(
  annot.macro 'rhombus.Port.Input':
    ~method_fallback: Port
  annot.macro 'rhombus.Port.Input.String':
    ~method_fallback: rhombus.Port.Input

  method (p :: rhombus.Port.Input).read_bytes()
  method (p :: rhombus.Port.Input).open_string()
    :: rhombus.Port.Input.String
){

@rhombusblock(
  def inp = rhombus.Port.Input.open_string()
  inp.read_bytes()
  inp.close()
)

}

@section{Using @rhombus(~doc)}

@docmodule(rhombus/scribble/tests/local)

@doc(
  ~include "local.rhm":
    documented_fun
){
}

@doc(
  ~include "local.rhm":
    DocumentedClass
    DocumentedClass.documented_method
    DocumentedClass.documented_property1
    DocumentedClass.documented_property2
){
}
