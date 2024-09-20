#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Printing}

@doc(
  fun print(v :: Any, ...,
            ~out: out :: Port.Output = Port.Output.current(),
            ~mode: mode :: PrintMode = #'text,
            ~pretty: pretty = Printable.current_pretty())
    :: Void
){

 Prints each @rhombus(v) to @rhombus(out). In the case that more than
 one @rhombus(v) is provided, a space is printed between the output for
 each @rhombus(v)---unless @rhombus(pretty) is @rhombus(#true), in which
 case a newline is printed between the output of each @rhombus(v).

 In @rhombus(#'text) mode, strings, symbols, identifiers, and keywords
 print as their character content, a byte string prints as its raw byte
 content, and a @tech{syntax object} prints as unquoted. Any other
 predefined kind of value prints the same in @rhombus(#'text) and
 @rhombus(#'expr) mode, but a class can implement
 @rhombus(Printable, ~class) so that its instances print differently in
 different modes.

 When @rhombus(pretty) is @rhombus(#true), then compound values like
 lists may print with line breaks to split the output across lines.
 When @rhombus(pretty) is @rhombus(#false), then printing tends to
 use a single line, but also prints faster. The value of the
 @rhombus(Printable.current_pretty) @tech{context parameter} is
 to match @rhombus(pretty) while printing, which affects functions
 like @rhombus(PrintDesc.list).

@examples(
  print("apple")
  print("apple", ~mode: #'expr)
  print("apple", "banana", "coconut")
  print("apple", "banana", "coconut", ~pretty: #true)
)

}

@doc(
  fun println(v :: Any, ...,
              ~out: out :: Port.Output = Port.Output.current(),
              ~mode: mode :: PrintMode = #'text,
              ~pretty: pretty = Printable.current_pretty())
    :: Void
){

 Prints like @rhombus(print), then prints a newline.

}

@doc(
  fun show(v :: Any, ...,
           ~out: out :: Port.Output = Port.Output.current(),
           ~pretty: pretty = Printable.current_pretty())
    :: Void
  fun showln(v :: Any, ...,
             ~out: out :: Port.Output = Port.Output.current(),
             ~pretty: pretty = Printable.current_pretty())
    :: Void
){

 Like @rhombus(print) and @rhombus(println) with @rhombus(~mode: #'expr).

}

@doc(
  enum PrintMode:
    text
    expr
){

 A printing mode for use with functions like @rhombus(print).

}
