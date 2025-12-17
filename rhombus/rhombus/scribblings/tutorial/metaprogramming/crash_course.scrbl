#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "crash_course"){New to Rhombus}

These warm-up exercises are meant to give you a sense of Rhombus and
where we're trying to go. These exercises are @emph{not} about
metaprogramming in the sense of the rest of the tutorial. We use
interpreters merely as a domain that we expect to be familiar to anyone
following the tutorial.

Skip @seclink("top", ~doc: ModulePath'lib("rhombus/scribblings/guide/rhombus-guide.scrbl")'){the docs},
because Rhombus looks like a lot of things you've seen before.

@section(~tag: "ex-interp1"){Exercise}

Open @local_file("interp1.rhm") and try the exercises listed at the top
of the file.

Solution: @local_file("interp1_soln.rhm").

@section(~tag: "ex-interp2"){Exercise}

Open @local_file("interp2.rhm") and try the exercises listed at the top
of the file.

If you're new to Rhombus, and because you followed the instructions to skip
the docs, you'll need to know about ellipses:

@itemlist(

 @item{When @rhombus(..., ~bind) is used in to match a pattern in a binding
 position, each variable before the @rhombus(..., ~bind) stands
 for a repetition. It binds matching any number of times.

For example, it can be used
for multi-argument functions:

@examples(
  ~defn:
    fun ignore_all_args(x, ...):
      "ok"
  ~repl:
    ignore_all_args(0)
    ignore_all_args()
    ignore_all_args(0, 1, 2)
  )

It can also be used inside lists in a single argument:
@examples(
  ~defn:
    fun ignore_ints_in_list([x :: Int, ...]):
      "ok"
  ~repl:
    def fav_ints = [1, 2, 3]
    ignore_ints_in_list(fav_ints)
    ignore_ints_in_list([1])
    ~error:
      ignore_ints_in_list([1, "apple"])
)}

 @item{Expression forms such as function calls, list constructions, and
 map (dictionary) constructions also recognize @rhombus(...). They
 duplicate the form before the @rhombus(...), which must include a
 reference to a repetition. The referenced repetition determines the
 number times the expression is repeated.

@examples(
  ~defn:
    fun add_to_all(x :: Int, [y :: Int, ...]):
      [x + y, ...]
  ~repl:
    add_to_all(10, [1, 2, 3])
  ~repl:
    def [x, ...] = ["1", "42", "79"]
    String.append(x, ...)
    def m = { x.to_int(): x, ... }
    m
    m[42]
)}

)

Solution: @local_file("interp2_soln.rhm").

@section(~tag: "ex-interp3"){Exercise}

Open @local_file("interp3.rhm") and try the exercises listed at the top
of the file.

This exercise uses @deftech{syntax objects}. Terms surrounded by single
quotes @quotes are @bold{not} strings, but are data structures. They are
typically used to represent program expressions and avoid all the problems
of using strings to represent expressions. They must conform to
@seclink("top", ~doc: shrubbery_doc){shrubbery
 notation} (don't read those docs!) in the same way that quoted
Lisp and Scheme terms must conform to
S-expression notation. Like S-expressions, some notational choices
(such as the presence of leading zeros in numbers)
do not affect the meaning of the syntax object, so
@rhombus('1 #,("") + 0002') (with extra spaces zeros) is the same
as @rhombus('1 + 2'). Unlike S-expressions, syntax objects are a sophisticated
data structure that can even help track binding information.

Using @quotes in a pattern-matching position accepts a
matching syntax object, where @rhombus($, ~bind) escapes back to binding
mode. Similarly, using @rhombus($) escapes in a
@litchar{'}…@litchar{'} expression escapes back to expression mode and
substitutes the expression's result into the syntax object.

@examples(
  ~repl:
    match '1 + 2'
    | '3': "oops"
    | '$left + $right': [left, right]
  ~repl:
    ~error:
      match '1 0 + 2'
      | '$left + $right': "ok"
  ~repl:
    match '1 + 2'
    | '$left + $right':
        'plus($left, $right)'
)

Solution: @local_file("interp3_soln.rhm").
