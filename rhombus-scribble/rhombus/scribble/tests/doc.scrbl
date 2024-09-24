#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus:
        expose:
          fun
      rhombus/draw)

@docmodule(rhombus)

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
