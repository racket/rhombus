#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Input and Output}

@doc(
  annot.macro 'Port'
  annot.macro 'Port.Input'
  annot.macro 'Port.Output'
){

 The @rhombus(Port, ~annot) annotation is satisified by a
 @deftech{port}, which is an input or output stream for files, network
 connects, terminals, etc. The @rhombus(Port.Input, ~annot) annotation
 recognizes input ports specifically, while @rhombus(Port.Output, ~annot)
 ports, and it is possible for a port to be both.

}

@doc(
  fun print(v,
            out = Port.current_output() :: Port.Output,
            ~mode: mode :: (#'text || #'expr) = #'expr,
            ~pretty: pretty = Printable.current_pretty())
    :: Void
){

 Prints @rhombus(v) to @rhombus(out).

 In @rhombus(#'text) mode, strings, symbols, identifiers, and keywords
 print as their character content, a byte string prints as its raw byte
 content, and a @tech{syntax object} prints as unquoted. Any other
 predefined kind of value prints the same in @rhombus(#'text) and
 @rhombus(#'expr) mode, but a class can implement
 @rhombus(Printable, ~class) so that its instances print differently in
 different modes.

 When @rhombus(pretty) is @rhombus(#false), then printing tends to
 use a single line, but also prints faster. The value of the
 @rhombus(Printable.current_pretty) @tech{context parameter} is
 to match @rhombus(pretty) while printing, which affects functions
 like @rhombus(PrintDesc.list).

}

@doc(
  fun println(v,
              out = Port.output_port() :: Port.Output,
              ~mode: mode :: (#'text || #'expr) = #'expr,
              ~pretty: pretty = Printable.current_pretty())
    :: Void
){

 Prints like @rhombus(print), then prints a newline.

}


@doc(
  def Port.current_input :: Parameter
  fun Port.current_input() :: Port.Input
  fun Port.current_input(in :: Port.Input)
){

 A @tech{context parameter} for the default port to use when reading.

}

@doc(
  def Port.current_output :: Parameter
  fun Port.current_output() :: Port.Output
  fun Port.current_output(out :: Port.Output)
){

 A @tech{context parameter} for the default port to use when printing.

}


@doc(
  def Port.current_error :: Parameter
  fun Port.current_error() :: Port.Output
  fun Port.current_error(out :: Port.Output)
){

 A @tech{context parameter} for the default port to use when printing
 errors.

}

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
  value and an optional @rhombus(~mode) like @rhombus(Printable.describe);
  the @rhombus(recur) function is specific to a particular overall print
  action so that it can handle cycles and graph references}

)

}


@doc(
  fun Printable.describe(v,
                         ~mode: mode :: (#'text || #'expr) = #'expr)
    :: PrintDesc
){


 Generates a pretty-printing description for @rhombus(v). The
 @rhombus(print) function composes @rhombus(Printable.describe)
 with @rhombus(Printable.render).

}



@doc(
  fun Printable.render(pd :: PrintDesc,
                       out = Port.output_port() :: Port.Output,
                       ~column: column :: NonnegInt = 0)
    :: Void
){

 Pretty-prints the description @rhombus(pd) to @rhombus(out).

 The optional @rhombus(indent) argument indicates the initial
 indentation value, in case @rhombus(pd) contains a
 @rhombus(PrintDesc.newline) description that needs to write indentation.

 The optional @rhombus(column) argument indicates the current column for
 output, in case @rhombus(pd) contains a @rhombus(PrintDesc.align)
 description that needs to update indentation based on the current
 column. 

}


@doc(
  annot.macro 'PrintDesc'
){

 Satisified by a @tech{string}, @tech{byte string}, or opaque result
 returned by functions like @rhombus(PrintDesc.concat).

 A string or byte string prints as its content. Other
 @rhombus(PrintDesc, ~annot) values describe concatenations,
 line breaks, indentation, and formatting options.

}

@doc(
  fun PrintDesc.concat(pd :: PrintDesc, ...) :: PrintDesc
  fun PrintDesc.newline() :: PrintDesc
  fun PrintDesc.nest(n :: NonnegInt, pd :: PrintDesc) :: PrintDesc
  fun PrintDesc.align(pd :: PrintDesc) :: PrintDesc
  fun PrintDesc.or(pd1 :: PrintDesc, pd2 :: PrintDesc) :: PrintDesc
  fun PrintDesc.flat(pd :: PrintDesc) :: PrintDesc
){

 Core @rhombus(PrintDesc, ~annot) constructors (in addition
 to plain strings and byte strings):

@itemlist(

 @item{@rhombus(PrintDesc.concat) concatenates the @rhombus(pd)s in
  order, with nothing in between.

  @examples(
    Printable.render(
      PrintDesc.concat("a", "b")
    )
  )}

 @item{@rhombus(PrintDesc.newline) prints a newline plus indentation,
  where indentation is determined by the surrounding description.

  @examples(
    Printable.render(
      PrintDesc.concat("a", PrintDesc.newline(),
                       "b")
    )
  )}

 @item{@rhombus(PrintDesc.nest) increases the current indentation by
  @rhombus(n) while printing @rhombus(pd).

  @examples(
    Printable.render(
      PrintDesc.concat(
        "a",
        PrintDesc.nest(2, PrintDesc.concat(PrintDesc.newline(),
                                           "b"))
      )
    )
  )}

 @item{@rhombus(PrintDesc.align) sets the current indentation
  (independent of the current indentation) to the current output column
  while printing @rhombus(pd).

  @examples(
    Printable.render(
      PrintDesc.concat("a",
                       PrintDesc.align(
                         PrintDesc.concat("b", PrintDesc.newline(),
                                          "c"))
                       )
    )
  )}

 @item{@rhombus(PrintDesc.or) offers two printign alternatives. Either
  @rhombus(pd1) or @rhombus(pd2) will be printed, depending on choices
  made by a pretty-printer configuration and as constrainted by
  @rhombus(PrintDesc.flat) constraints.

  @examples(
    Printable.render(
      PrintDesc.or(PrintDesc.concat("a", "; ", "b"),
                   PrintDesc.concat("a", PrintDesc.newline(),
                                    "b"))
    )
  )}

 @item{@rhombus(PrintDesc.flat) prints the same as @rhombus(pd), but
  only if that is possible without any newlines. If all possible ways of
  rendering @rhombus(pd) involve a newline, printing fails. A
  @rhombus(PrintDesc.flat) constraint it particularly useful in one branch
  of a @rhombus(PrintDesc.or) to constrain a description received by
  recursive description.

  @examples(
    def sub = PrintDesc.or(PrintDesc.concat("a", "; ", "b"),
                           PrintDesc.concat("a", PrintDesc.newline(), "b"))
    Printable.render(
      PrintDesc.or(
        PrintDesc.concat("f(", PrintDesc.flat(sub), ")"),
        PrintDesc.concat("f(",
                         PrintDesc.nest(
                           2,
                           PrintDesc.concat(PrintDesc.newline(), sub)
                         ),
                         PrintDesc.newline(), ")")
      )
    )
  )}

)

}


@doc(
  fun PrintDesc.list(pre_pd :: PrintDesc,
                     elements :: List.of(PrintDesc),
                     post_pd :: PrintDesc)
    :: PrintDesc
  fun PrintDesc.block(head_pd :: PrintDesc,
                      body :: PrintDesc)
    :: PrintDesc
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
  Printable.render(PrintDesc.list("Posn(", ["x", "y"], ")"))
  Printable.render(PrintDesc.block("begin",
                                   PrintDesc.concat("one",
                                                    PrintDesc.newline(),
                                                    "two")))
)

}


@doc(
  def Printable.current_pretty :: Parameter
  fun Printable.current_pretty() :: Boolean
  fun Printable.current_pretty(on)
){

 A @tech{context parameter} that determines the default printing mode.
 The parameter's value is used by @rhombus(print), @rhombus(println), and
 @rhombus(PrintDesc.list), for example.

}


@doc(
  def Printable.current_optimal :: Parameter
  fun Printable.current_optimal() :: Boolean
  fun Printable.current_optimal(on)
){

 A @tech{context parameter} that determines whether pretty printing uses
 a faster but non-optimal strategy or a slower, optimal strategy. The
 parameter's value is not used when @rhombus(Printable.current_pretty) is
 @rhombus(#false).

}


@doc(
  def Printable.current_page_width :: Parameter
  fun Printable.current_page_width() :: Boolean
  fun Printable.current_page_width(on)
){

 A @tech{context parameter} for pretty printing that determines the
 maximum number of columns that printing should use, if possible.

}


@doc(
  def Printable.current_show_graph :: Parameter
  fun Printable.current_show_graph() :: Boolean
  fun Printable.current_show_graph(on)
){

 A @tech{context parameter} that determines whether printing shows
 sharing of objects in terms of @rhombus(===) identity.

 Sharing is reported by a @litchar{#}@italic{n}@litchar{=} prefix on a
 printed value, and then @litchar{#}@italic{n}@litchar{#} with the same
 number @italic{n} is used for later occurrences of the value.

 The same notation is used to show cyclic data, which is shown
 independent of the value of the @rhombus(Printable.current_show_graph)
 parameter.

}
