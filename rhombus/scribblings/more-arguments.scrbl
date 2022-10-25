#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(val args_eval: make_rhombus_eval())

@examples(
  ~eval: args_eval,
  ~hidden: #true,
  class Posn(x, y) 
)

@title(~tag: "more-arguments"){More Function Arguments}

As we saw in @secref("functions_optional"), by using optional arguments
and @litchar{|} clauses, you can define a function that accepts a
varying number of arguments. When using only those features, however,
the allowed number of arguments is still a fixed set of small numbers.
To define or call a function that accepts any number of arguments, use
the @rhombus(...) repetition form or @rhombus(&) list-splicing form after
other arguments.

For example, in the following definition of @rhombus(add), the argument
@rhombus(x) is bound as a repetition, which allows any number of
arguments:

@(demo:
    ~defn:
      fun add(x -: Number, ...):
        for values(total = 0):
          ~each v: [x, ...]
          total+v
    ~repl:
      add(1, 2, 3, 4)
    ~repl:
      val [n, ...]: [20, 30, 40]
      add(10, n, ..., 50)
    ~repl:
      val ns: [20, 30, 40]
      add(10, & ns, 50)
  )

As illustrated in the calls to @rhombus(add), @rhombus(...) and
@rhombus(&) work for function calls the same way that they work for
constructors specifically. Elements of the repetition or list are
spliced into the function call as separate arguments, as opposed to
being passed as a list. While only one of @rhombus(...) or @rhombus(&)
can be used in a function definition, they can appear any number of
times and in any order within a function call.

A function doesn't have to accept an arbitrary number of arguments for
@rhombus(...) or @rhombus(&) to work in a call to the function, as long
at the total number of spliced arguments matches the number that the
function expects.

@(demo:
    expt(& [2, 10])
  )

The @rhombus(add) function could also be written with @rhombus(&) for
its argument instead of @rhombus(...), like this:

@(demo:
    ~eval: args_eval
    ~defn:
      fun add(& xs -: List.of(Number)):
        for values(total = 0):
          ~each v: xs
          total+v
    ~repl:
      add(1, 2, 3, 4)
  )

Note that the annotation on @rhombus(x) as a repetition refers to an
individual argument within the repetition, while the annotation on
@rhombus(xs) refers to the whole list of arguments.

To create a function that works with any number of keyowrd arguments,
use @rhombus(~&) to bind an argument that receives all additional
keyword arguments. The additional arguments are collected into a map
with keywords as keys.

@(demo:
    ~eval: args_eval
    ~defn:
      fun roster(~manager: who, ~& players):
        players
    ~repl:
      roster(~pitcher: "Dave", ~manager: "Phil", ~catcher: "Johnny")
  )

Similarly, use @rhombus(~&) in a function call to pass keyword arguments
that are in map. Using @rhombus(~&) to call a function is most useful
when chaining from one keyword-accepting function to another.

@(demo:
    ~eval: args_eval
    ~defn:
      fun
      | circle_area(~radius): 3.14 * radius * radius
      | circle_area(~diameter): (1/2) * 3.14 * diameter * diameter
    ~defn:
      fun
      | rectangle_area(~width, ~height): width * height
    ~defn:
      fun
      | shape_area(~type: "circle", ~& props): circle_area(~& props)
      | shape_area(~type: "rectangle", ~& props): rectangle_area(~& props)
    ~repl:
      shape_area(~type: "circle", ~radius: 1)
      shape_area(~type: "circle", ~diameter: 8.5)
      shape_area(~type: "rectangle", ~width: 8.5, ~height: 11)
  )

A function call can use @rhombus(~&) only once, but in any order compared
to other arguments. A function definition can use @rhombus(~&) at most
once, and only after all other arguments other than a @rhombus(&)
argument or @rhombus(...) repetition argument. A @rhombus(~&) can appear
in either order with a @rhombus(&) or @rhombus(...) argument.

Functions can use @litchar{|} cases, annotations, and/or pattern
matching to distinguish calls with the same number of arguments.
Different cases use @rhombus(&), @rhombus(...), and @rhombus(~&)
independently.

@(demo:
    ~eval: args_eval
    ~defn:
      fun
      | avg(n :: Number, & ns -: List.of(Number)):
          (n + add(& ns)) / (1 + List.length(ns))
      | avg(p :: Posn, & ps -: List.of(Posn)):
          Posn(avg(p.x, & Function.map(Posn.x, ps)),
               avg(p.y, & Function.map(Posn.y, ps)))
    ~repl:
      avg(1, 2, 6)
      avg(Posn(0, 0), Posn(1, 3), Posn(2, 0))
)

@close_eval(args_eval)
