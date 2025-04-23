#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Views}

@(~version_at_least "8.14.0.4")

@doc(
  interface gui.View
){

 A @deftech{view} corresponds to @rhombus(#{view<%>}) from
 @racketmodname(racket/gui/easy).

 Implementations of @rhombus(View, ~class) include
 @rhombus(Button, ~class) and @rhombus(Canvas, ~class).

}

@doc(
  property (v :: gui.View).handle :: Any
){

 Returns a Racket object that corresponds to the view for use directly
 with @racketmodname(racket/gui/easy).

}


@doc(
  interface gui.WindowView:
    extends View
){

 A @deftech{window view} corresponds to @rhombus(#{window-view<%>}) from
 @racketmodname(racket/gui/easy).

 Create a @rhombus(WindowView, ~class) using @rhombus(Window, ~class).

}
