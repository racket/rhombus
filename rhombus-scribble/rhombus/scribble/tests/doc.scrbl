#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus:
        expose:
          fun)

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
