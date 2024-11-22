#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus:
        expose:
          fun
          repr
          String
      rhombus/draw)

@docmodule(rhombus)

Starting example:

@rhombusblock(
  // should be linked
  repr().length()
  repr().get()
  "Hello".length()
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
