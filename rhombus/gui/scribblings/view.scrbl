#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Views}

@doc(
  interface View
){

 A @deftech{view} corresponds to @rhombus(#{view<%>}) from
 @rhombusmodname(racket/gui/easy).

 Implementations of @rhombus(View, ~class) include
 @rhombus(Button, ~class) and @rhombus(Canvas, ~class).

}

@doc(
  property View.handle(v :: View)
){

 Returns an Racket object that corresponds to the view for use directly
 with @rhombusmodname(racket/gui/easy).

}


@doc(
  interface WindowView:
    extends View
){

 A @deftech{window view} corresponds to @rhombus(#{window-view<%>}) from
 @rhombusmodname(racket/gui/easy).

 Create a @rhombus(WindowView, ~class) using @rhombus(Window, ~class).

}
