#lang scribble/rhombus/manual
@(import:
   rhombus/meta open
   "common.rhm" open:
     except: . #%index
   meta_label:
     rhombus/static open:
       only: . #%index)

@(defn.macro 'def_dynamics $dynamic_dot $dynamic_ref':
    'import: meta_label: rhombus open
     def $dynamic_dot: @rhombus(.)
     def $dynamic_ref: @rhombus(#%index)')
@(def_dynamics dynamic_dot dynamic_ref)

@title(~tag: "static-lib"){Rhombus Static by Default}

@docmodule(~lang, rhombus/static)

@docmodule(~no_declare, rhombus/static/meta)

@docmodule(~lang, ~no_declare, rhombus/static/and_meta)

The @rhombuslangname(rhombus/static),
@rhombusmodname(rhombus/static/meta), and
@rhombuslangname(rhombus/static/and_meta) modules re-export the
bindings of @rhombuslangname(rhombus), @rhombusmodname(rhombus/meta),
and @rhombuslangname(rhombus/and_meta), respectively, except that
bindings from @rhombus(use_static) are exported in place of the
dynamic variants.

@doc(
  expr.macro '$target . $identifier'
){

 The static variant of the @(dynamic_dot) operator. See @rhombus(use_static).

}

@doc(
  expr.macro '$expr #%index [$at_expr]',
  expr.macro '$expr #%index [$at_expr] := $rhs_expr',
){

 The static variant of @(dynamic_ref). See @rhombus(use_static).

}
