#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title{More Function Arguments}

In the same way that @rhombus[..., ~bind] can be used in a list
binding pattern to create a repetition, @rhombus[..., ~bind] can be
used after the last argument in a function declaration to bind
repetitions. And just like @rhombus[...] can be used at the end of a
list expression to add the repetition's element to the end of the
list, @rhombus[...] can be used at the end of a function-call
expression to use the repetition's elements as the last arguments to
the function.

@(rhombusblock:
    fun
    | add(): 0
    | add(x): x
    | add(x, y, ...): x + add(y, ...)

    add(1, 2, 3)    // prints 6

    val [n, ...]: [10, 20, 30]
    add(1, n, ...)  // prints 61
  )
