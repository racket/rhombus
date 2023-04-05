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

@doc(
  expr.macro 'View.if $obs_expr
              | $view_expr
              | $view_expr'
  expr.macro 'View.cond
              | $obs_expr: $view_expr
              | ...
              | ~else: $view_expr'
  expr.macro 'View.cond
              | $obs_expr: $view_expr
              | ...
              | ~else $view_expr'
){

 Produces a @rhombus(View) that changes if @rhombus(obs_expr) is an
 @tech{observable} and when the observable's value changes. Each
 @rhombus(view_expr) must produce a @rhombus(View).

}
