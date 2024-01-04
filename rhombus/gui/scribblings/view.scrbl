#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Views}

@doc(
  interface View
){

 A @deftech{view} corresponds to @rhombus(#{view<%>}) from
 @racketmodname(racket/gui/easy).

 Implementations of @rhombus(View, ~class) include
 @rhombus(Button, ~class) and @rhombus(Canvas, ~class).

}

@doc(
  property (v :: View).handle :: Any
){

 Returns a Racket object that corresponds to the view for use directly
 with @racketmodname(racket/gui/easy).

}


@doc(
  interface WindowView:
    extends View
){

 A @deftech{window view} corresponds to @rhombus(#{window-view<%>}) from
 @racketmodname(racket/gui/easy).

 Create a @rhombus(WindowView, ~class) using @rhombus(Window, ~class).

}
