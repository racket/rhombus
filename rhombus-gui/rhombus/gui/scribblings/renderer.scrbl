#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Renderers}

@doc(
  annot.macro 'Renderer'
){

 An @deftech{renderer} corresponds to @rhombus(#{renderer?}) from
 @racketmodname(racket/gui/easy).

}

@doc(
  fun render(view :: WindowView,
             parent :: maybe(Renderer) = #false)
    :: Renderer
){

 Renders @rhombus(view). Provide a @rhombus(parent) renderer to show a
 modal dialog for a rendered window.

}
