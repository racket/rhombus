#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title{Function Expressions}

The @rhombus(fun) form works in an expression position as λ. Just like
@tt{function} in JavaScript, the expression variant omits a function
name.

@(rhombusblock:
    val curried_add: fun (x):
                       fun (y):
                         x+y

    curried_add(10)(20)  // prints 30
  )

Naturally, keyword and optional arguments (as described in the
@seclink("keyword-arg"){next section}) work with @rhombus(fun)
expressions, too.
