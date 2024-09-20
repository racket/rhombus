#lang rhombus/scribble/manual
@(import:
   rhombus/meta open
   "common.rhm" open:
     except: #%dynamism
   meta_label:
     rhombus/static open:
       only: #%dynamism)

@(
  namespace dynamic:
    import:
      meta_label:
        rhombus:
          expose: #%dynamism
    export:
      dynamism
    def dynamism = @rhombus(#%dynamism)
)

@title(~tag: "static-lib"){Static by Default}

@docmodule(~lang, rhombus/static)

@docmodule(~no_declare, rhombus/static/meta)

@docmodule(~lang, ~no_declare, rhombus/static/and_meta)

The @rhombuslangname(rhombus/static),
@rhombusmodname(rhombus/static/meta), and
@rhombuslangname(rhombus/static/and_meta) modules re-export the
bindings of @rhombuslangname(rhombus), @rhombusmodname(rhombus/meta),
and @rhombuslangname(rhombus/and_meta), respectively, except that
@rhombus(#%dynamism) is exported to indicate static mode.

@doc(
  expr.macro '#%dynamism'
){

 Initially indicates static mode, as opposed to @(dynamic.dynamism) from
 @rhombuslangname(rhombus). See also @rhombus(use_static) and
 @rhombus(use_dynamic).

}
