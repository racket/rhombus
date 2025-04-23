#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Renderers}

@(~version_at_least "8.14.0.4")

@doc(
  annot.macro 'gui.Renderer'
){

 An @deftech{renderer} corresponds to @rhombus(#{renderer?}) from
 @racketmodname(racket/gui/easy).

}

@doc(
  fun gui.render(view :: WindowView,
                 parent :: maybe(Renderer) = #false)
    :: Renderer
){

 Renders @rhombus(view). Provide a @rhombus(parent) renderer to show a
 modal dialog for a rendered window.

}
