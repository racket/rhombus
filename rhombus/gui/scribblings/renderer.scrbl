#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Renderers}

@doc(
  annot.macro 'Renderer'
){

 An @deftech{renderer} corresponds to @rhombus(#{renderer?}) from
 @rhombusmodname(racket/gui/easy).

}

@doc(
  fun render(view :: WindowView,
             parent :: Maybe(Renderer) = #false) :: Renderer
){

 Renders @rhombus(view). Provide a @rhombus(parent) renderer to show a
 modal dialog for a rendered window.

}
