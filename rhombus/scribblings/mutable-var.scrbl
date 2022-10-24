#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "mutable-vars"){Mutable Variables}

Variables are immutable unless they are declared with the
@rhombus(mutable) binding operator. The @rhombus(:=) infix operator
assigns to a mutable variable while also returning the variable’s new
value.

@(demo:
    ~defn:
      def mutable todays_weather: "sunny"
    ~repl:
      todays_weather
      todays_weather := "rainy"
      todays_weather
    ~defn:
      def f(mutable x):
        x := x + 8
        x
    ~repl:
      f(10)
      ~error: f := 5
  )

@aside{The @rhombus(:=) operator should also cooperate with @rhombus(.)
 when a class field is declared @rhombus(mutable), but that’s not yet
 implemented.}
