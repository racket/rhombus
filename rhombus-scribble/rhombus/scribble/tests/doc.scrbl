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
      rhombus/draw)

@docmodule(rhombus)

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
  method String.copy() :: ReadableString
  method String.snapshot() :: String
){

 Description.

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
