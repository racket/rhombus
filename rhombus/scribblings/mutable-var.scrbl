#lang scribble/rhombus/manual
@(import: "util.rhm": no_prefix)

@title[~tag: "mutable-vars"]{Mutable Variables}

Variables are immutable unless they are declared with the
@rhombus[mutable] binding operator. The @rhombus[=] infix operator
assigns to a mutable variable while also returning the variable’s new
value.

@(rhombusblock:
    def mutable todays_weather: "sunny"

    todays_weather            // prints "sunny"
    todays_weather = "rainy"  // prints "rainy"
    todays_weather            // prints "rainy"

    def f(mutable x):
      x = x + 8
      x

    f(10)  // prints 18

    // f = 5 // would be an error: f is not mutable
  )

@aside{The @rhombus[=] operator should also cooperate with @rhombus[.]
 when a class field is declared @rhombus[mutable], but that’s not yet
 implemented.}
