#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "mutable-vars"){Mutable Variables}

Variables are immutable unless they are declared with the
@rhombus(mutable) binding operator. The @rhombus(:=) infix operator
assigns to a mutable variable.

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

The @rhombus(:=) operator can also change object fields accessed via
@rhombus(.) when a class field is declared @rhombus(mutable, ~bind).

@(demo:
    ~defn:
      class Box(mutable content)
    ~repl:
      def present: Box("socks")
      present.content
      present.content := "toy"
      present.content
  )


