#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Printables}

Any value is @deftech{printable}. Implementing the @rhombus(Printable, ~class)
interface customizes the way that instances of a class print.

@doc(
  interface Printable
){

@provided_interface_only()

 An interface that a class can implement (publicly or privately) to
 customize the way its objects print. In the simplest case, an
 implementation of the interface's @rhombus(describe, ~datum) method
 returns a string to use as an object's printed form. More generally, a
 @rhombus(describe, ~datum) method implementation returns a description of
 how to print a value, where the description is created using functions
 like @rhombus(PrintDesc.concat), @rhombus(PrintDesc.newline), and
 @rhombus(PrintDesc.or).

 The @rhombus(Printable) interface has one method:

@itemlist(

 @item{@rhombus(#,(@rhombus(describe, ~datum))(#,(@rhombus(mode, ~var)), #,(@rhombus(recur, ~var))))
  --- returns a @rhombus(PrintDesc, ~annot) given a
  @rhombus(mode, ~var), which is either @rhombus(#'text) or
  @rhombus(#'expr), and a @rhombus(recur, ~var) function, which accepts a
  value, an optional @rhombus(~mode) like @rhombus(Printable.describe)
  (unlike @rhombus(Printable.describe), the @rhombus(~mode) defaults to
  @rhombus(#'expr), which is generally desirable when printing subcomponents),
  and an optional @rhombus(~as) argument;
  the @rhombus(recur, ~var) function is specific to a particular overall print
  action so that it can handle cycles and graph references.

  The optional @rhombus(~as) argument for @rhombus(recur, ~var) can be
  @rhombus(#'print) or @rhombus(#'super), and the default is
  @rhombus(#'print). When @rhombus(~as) is @rhombus(#'super), the supplied
  object to print must be the same object whose @rhombus(describe, ~datum)
  method is called; in that case, instead of immediately recurring back to
  @rhombus(describe, ~datum), the default printing implementation is used.
  An exception is thrown for @rhombus(#'super) mode for any other object
  to print.}

)

}


@doc(
  fun Printable.describe(
    v :: Any,
    ~mode: mode :: PrintMode = #'text
  ) :: PrintDesc
){


 Generates a pretty-printing description for @rhombus(v). The
 @rhombus(print) function composes @rhombus(Printable.describe)
 with @rhombus(Printable.render).

}



@doc(
  fun Printable.render(
    pd :: PrintDesc,
    out :: Port.Output = stdout,
    ~column: column :: Nat = 0
  ) :: Void
){

 Pretty-prints the description @rhombus(pd) to @rhombus(out).

 The optional @rhombus(column) argument indicates the current column for
 output, in case @rhombus(pd) contains a @rhombus(PrintDesc.align)
 description that needs to update indentation based on the current
 column.

}


@doc(
  annot.macro 'PrintDesc'
){

 Satisfied by a @tech{string}, @tech{byte string}, or opaque result
 returned by functions like @rhombus(PrintDesc.concat).

 A string or byte string prints as its content. Other
 @rhombus(PrintDesc, ~annot) values describe concatenations,
 line breaks, indentation, and formatting options.

}

@doc(
  fun PrintDesc.concat(pd :: PrintDesc, ...)
    :: PrintDesc
  fun PrintDesc.newline()
    :: PrintDesc
  fun PrintDesc.nest(n :: Nat, pd :: PrintDesc)
    :: PrintDesc
  fun PrintDesc.align(pd :: PrintDesc)
    :: PrintDesc
  fun PrintDesc.or(pd1 :: PrintDesc, pd2 :: PrintDesc)
    :: PrintDesc
  fun PrintDesc.flat(pd :: PrintDesc)
    :: PrintDesc
){

 Core @rhombus(PrintDesc, ~annot) constructors (in addition
 to plain strings and byte strings):

@itemlist(

 @item{@rhombus(PrintDesc.concat) concatenates the @rhombus(pd)s in
  order, with nothing in between.

  @examples(
    Printable.render(
      PrintDesc.concat(
        "a", "b",
      ))
  )}

 @item{@rhombus(PrintDesc.newline) prints a newline plus indentation,
  where indentation is determined by the surrounding description.

  @examples(
    Printable.render(
      PrintDesc.concat(
        "a", PrintDesc.newline(),
        "b",
      ))
  )}

 @item{@rhombus(PrintDesc.nest) increases the current indentation by
  @rhombus(n) while printing @rhombus(pd).

  @examples(
    Printable.render(
      PrintDesc.concat(
        "a",
        PrintDesc.nest(
          2,
          PrintDesc.concat(
            PrintDesc.newline(),
            "b",
          )),
      ))
  )}

 @item{@rhombus(PrintDesc.align) sets the current indentation
  (independent of the current indentation) to the current output column
  while printing @rhombus(pd).

  @examples(
    Printable.render(
      PrintDesc.concat(
        "a",
        PrintDesc.align(
          PrintDesc.concat(
            "b", PrintDesc.newline(),
            "c",
          )),
      ))
  )}

 @item{@rhombus(PrintDesc.or) offers two printing alternatives. Either
  @rhombus(pd1) or @rhombus(pd2) will be printed, depending on choices
  made by a pretty-printer configuration and as constrained by
  @rhombus(PrintDesc.flat) constraints.

  @examples(
    Printable.render(
      PrintDesc.or(
        PrintDesc.concat(
          "a", "; ", "b",
        ),
        PrintDesc.concat(
          "a", PrintDesc.newline(),
          "b",
        )))
  )}

 @item{@rhombus(PrintDesc.flat) prints the same as @rhombus(pd), but
  only if that is possible without any newlines. If all possible ways of
  rendering @rhombus(pd) involve a newline, printing fails. A
  @rhombus(PrintDesc.flat) constraint it particularly useful in one branch
  of a @rhombus(PrintDesc.or) to constrain a description received by
  recursive description.

  @examples(
    def sub = PrintDesc.or(
      PrintDesc.concat(
        "a", "; ", "b",
      ),
      PrintDesc.concat(
        "a", PrintDesc.newline(),
        "b",
      ))
    Printable.render(
      PrintDesc.or(
        PrintDesc.concat(
          "f(", PrintDesc.flat(sub), ")",
        ),
        PrintDesc.concat(
          "f(",
          PrintDesc.nest(
            2,
            PrintDesc.concat(
              PrintDesc.newline(),
              sub,
            )),
          PrintDesc.newline(),
          ")",
        )))
  )}

)

}


@doc(
  fun PrintDesc.list(
    pre_pd :: PrintDesc,
    elements :: Listable.to_list && List.of(PrintDesc),
    post_pd :: PrintDesc
  ) :: PrintDesc
  fun PrintDesc.block(
    head_pd :: PrintDesc,
    body :: PrintDesc
  ) :: PrintDesc
){

 Description-building helpers for list-like and block-like forms where
 the printing options include a single-variant and multi-line variants,
 but the latter only when @rhombus(Printable.current_pretty) is set to
 @rhombus(#true).

 The single-line variant constrains @rhombus(pre_pd), @rhombus(head),
 and any member of @rhombus(elements) other than the last one to be
 printed as a single-line, too. If one of those has no single-line
 option, then the combined single-line variant will not be used (which
 can cause the description to be unprintable).

@examples(
  Printable.render(
    PrintDesc.list(
      "Posn(",
      ["x", "y"],
      ")"
    ))
  Printable.render(
    PrintDesc.block(
      "begin",
      PrintDesc.concat(
        "one", PrintDesc.newline(),
        "two",
      )))
)

}

@doc(
  fun PrintDesc.special(v :: Any,
                        alt_pd :: PrintDesc,
                        ~length: length :: Nat = 1,
                        ~mode: mode :: PrintDesc.SpecialMode
                                 = #'write_special)
    :: PrintDesc

  enum PrintDesc.SpecialMode
  | write_special
  | print
  | write
  | display
){

 The @rhombus(PrintDesc.special) function describes printing of @rhombus(v)
 using Racket printing functions when the output port satisfies
 @rhombus(Port.Output.Special, ~annot), otherwise printing as the given @rhombus(alt_pd). For
 the purposes of pretty printing, @rhombus(v) is counted as using
 @rhombus(length) columns. The @rhombus(mode) argument indicates which
 Racket printing function is used.

}


@doc(
  Parameter.def Printable.current_pretty :: Any.to_boolean
    = #false
){

 A @tech{context parameter} that determines the default printing mode.
 The parameter's value is used by @rhombus(print), @rhombus(println), and
 @rhombus(PrintDesc.list), for example.

}


@doc(
  Parameter.def Printable.current_optimal :: Any.to_boolean
    = #false
){

 A @tech{context parameter} that determines whether pretty printing uses
 a faster but non-optimal strategy or a slower, optimal strategy. The
 parameter's value is not used when @rhombus(Printable.current_pretty) is
 @rhombus(#false).

}


@doc(
  Parameter.def Printable.current_page_width :: Nat
    = 80
){

 A @tech{context parameter} for pretty printing that determines the
 maximum number of columns that printing should use, if possible.

}


@doc(
  Parameter.def Printable.current_graph :: Any.to_boolean
    = #false
){

 A @tech{context parameter} that determines whether printing shows
 sharing of objects in terms of @rhombus(===) identity.

 Sharing is reported by a @litchar{#}@math{n}@litchar{=} prefix on a
 printed value, and then @litchar{#}@math{n}@litchar{#} with the same
 number @italic{n} is used for later occurrences of the value.

 The same notation is used to show cyclic data, which is shown
 independent of the value of the @rhombus(Printable.current_graph)
 parameter.

}
