#lang scribble/rhombus/manual
@(import:
   rhombus/meta open
   "common.rhm" open:
     except: . #%ref
   meta_label:
     rhombus/static open:
       only: . #%ref)

@(defn.macro 'def_dynamics $dynamic_dot $dynamic_ref':
    'import: meta_label: rhombus open
     def $dynamic_dot: @rhombus(.)
     def $dynamic_ref: @rhombus(#%ref)')
@(def_dynamics dynamic_dot dynamic_ref)

@title(~tag: "static-lib"){Rhombus Static by Default}

@docmodule(~lang, rhombus/static)

@docmodule(~no_declare, rhombus/static/meta)

@docmodule(~lang, ~no_declare, rhombus/static/and_meta)

The @rhombusmodname(rhombus/static),
@rhombusmodname(rhombus/static/meta), and
@rhombusmodname(rhombus/static/and_meta) modules re-export the
bindings of @rhombusmodname(rhombus), @rhombusmodname(rhombus/meta),
and @rhombusmodname(rhombus/and_meta), respectively, except that
bindings from @rhombus(use_static) are exported in place of the
dynamic variants.

@doc(
  expr.macro '$target . $identifier'
){

 The static variant of the @(dynamic_dot) operator. See @rhombus(use_static).

}

@doc(
  expr.macro '$expr #%ref [$at_expr]',
  expr.macro '$expr #%ref [$at_expr] := $rhs_expr',
){

 The static variant of @(dynamic_ref). See @rhombus(use_static).

}
