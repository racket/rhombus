#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Input and Output}

A @deftech{port} is an input or output stream for a file, network
connection, terminal, etc. An @deftech{input port} is specifically for
input, while an @deftech{output port} is specifically for output.

@dispatch_table(
  "input port"
  Port.Input
  in.peek_byte(arg, ...)
  in.peek_bytes(arg, ...)
  in.peek_char(arg, ...)
  in.peek_string(arg, ...)
  in.read_byte()
  in.read_bytes(arg, ...)
  in.read_char()
  in.read_line(arg, ...)
  in.read_string(arg)
)

@dispatch_table(
  "output port"
  Port.Output
  out.get_bytes()
  out.get_string()
  out.flush()
  out.print(arg, ...)
  out.println(arg, ...)
  out.show(arg, ...)
  out.showln(arg, ...)
)

@doc(
  annot.macro 'Port'
  annot.macro 'Port.Input'
  annot.macro 'Port.Output'
  annot.macro 'Port.EOF'
){

 The @rhombus(Port, ~annot) annotation is satisified by a @tech{port}.
 The @rhombus(Port.Input, ~annot) annotation
 recognizes input ports specifically, while @rhombus(Port.Output, ~annot)
 recognizes output ports, and it is possible for a port to be both.

 The @rhombus(Port.EOF, ~annot) annotation is satisfied by the
 @rhombus(Port.eof) value.

}

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

@doc(
  Parameter.def Port.Input.current :: Port.Input
){

 A @tech{context parameter} for the default port to use when reading.

}

@doc(
  Parameter.def Port.Output.current :: Port.Output
){

 A @tech{context parameter} for the default port to use when printing.

}


@doc(
 def Port.eof :: Port.EOF
){

 A value (distinct from all other values) that represents an end-of-file.

}

@doc(
  Parameter.def Port.Output.current_error :: Port.Output
){

 A @tech{context parameter} for the default port to use when printing
 errors.

}

@doc(
  fun Port.Input.open_bytes(bstr :: Bytes,
                            name :: Symbol = #'string)
    :: Port.Input
){

 Creates an @tech{input port} that reads bytes from @rhombus(bstr), a
 @tech{byte string}. The optional @rhombus(name) is used as the name for the
 returned port.

}

@doc(
  fun Port.Input.open_string(str :: ReadableString,
                             name :: Symbol = #'string)
    :: Port.Input
){

 Creates an @tech{input port} that reads @tech{characters} from @rhombus(str),
 a @tech{string}. The optional @rhombus(name) is used as the name for the
 returned port.

}

@doc(
  fun Port.Input.peek_byte(in :: Port.Input,
                           ~skip_bytes: skip :: NonnegInt = 0)
    :: Byte || Port.EOF
){

 Like @rhombus(Port.Input.read_byte), but peeks instead of reading, and skips
 @rhombus(skip) bytes at the start of the port.

}

@doc(
  fun Port.Input.peek_bytes(in :: Port.Input,
                            amount :: NonnegInt,
                            ~skip_bytes: skip :: NonnegInt = 0)
    :: Bytes || Port.EOF
){

 Like @rhombus(Port.Input.read_bytes), but peeks instead of reading, and skips
 @rhombus(skip) bytes at the start of the port.

}

@doc(
  fun Port.Input.peek_char(in :: Port.Input,
                           ~skip_bytes: skip :: NonnegInt = 0)
    :: Char || Port.EOF
){

 Like @rhombus(Port.Input.read_char), but peeks instead of reading, and skips
 @rhombus(skip) bytes (not characters) at the start of the port.

}

@doc(
  fun Port.Input.peek_string(in :: Port.Input,
                             amount :: NonnegInt,
                             ~skip_bytes: skip :: NonnegInt = 0)
    :: String || Port.EOF
){

 Like @rhombus(Port.Input.read_string), but peeks instead of reading, and skips
 @rhombus(skip) bytes at the start of the port.

}

@doc(
  fun Port.Input.read_byte(in :: Port.Input)
    :: Byte || Port.EOF
){

 Reads a single byte from @rhombus(in). If no bytes are available before and
 end-of-file, then @rhombus(Port.eof) is returned.

}

@doc(
  fun Port.Input.read_bytes(in :: Port.Input,
                            amount :: NonnegInt)
    :: Bytes || Port.EOF
){

 Reads a @tech{byte string} containing the next @rhombus(amount) bytes from
 @rhombus(in). If @rhombus(amount) is 0, then an empty byte string is returned.
 Otherwise if fewer than @rhombus(amount) bytes are available before an
 end-of-file is encountered, then the returned byte string will contain only
 those bytes before the end-of-file; that is, the returned byte string's length
 will be less than @rhombus(amount). If no bytes are available before an
 end-of-file, then @rhombus(Port.eof) is returned.

}

@doc(
  fun Port.Input.read_char(in :: Port.Input)
    :: Char || Port.EOF
){

 Reads a single character from @rhombus(in) --- which may involve reading
 several bytes to UTF-8-decode them into a character; a minimal number of
 bytes are read/peeked to perform the decoding. If no bytes are available
 before an end-of-file, then @rhombus(Port.eof) is returned.

}

@doc(
  fun Port.Input.read_line(in :: Port.Input,
                           mode :: Port.ReadLineMode = #'any)
    :: String || Port.EOF
){

 Returns a string containing the next line of characters from @rhombus(in).

 Characters are read from @rhombus(in) until a line separator or an
 end-of-file is read. The line separator is not included in the result
 string (but it is removed from the port's stream). If no characters
 are read before an end-of-file is encountered, @rhombus(Port.eof) is
 returned.

 The @rhombus(mode) argument determines the line separator(s). It must be one
 of the following symbols:

 @itemlist(
   @item{
     @rhombus(#'linefeed) breaks lines on linefeed characters.
   }

   @item{
     @rhombus(#'return) breaks lines on return characters.
   }

   @item{
     @rhombus(#'return_linefeed) breaks lines on
     return-linefeed combinations. If a return character is not followed by a
     linefeed character, it is included in the result string; similarly, a
     linefeed that is not preceded by a return is included in the result string.
   }

   @item{
     @rhombus(#'any) breaks lines on any of a return character,
     linefeed character, or return-linefeed combination. If a return character
     is followed by a linefeed character, the two are treated as a combination.
   }

   @item{
     @rhombus(#'any_one) breaks lines on either a return or
     linefeed character, without recognizing return-linefeed combinations.
   }
 )

}

@doc(
  fun Port.Input.read_string(in :: Port.Input,
                             amount :: NonnegInt)
    :: String || Port.EOF
){

 Returns a string containing the next @rhombus(amount) characters from
 @rhombus(in).

 If @rhombus(amount) is @rhombus(0), then the empty string is
 returned. Otherwise, if fewer than @rhombus(amount) characters are
 available before an end-of-file is encountered, then the returned
 string will contain only those characters before the end-of-file; that
 is, the returned string's length will be less than @rhombus(amount). (A
 temporary string of size @rhombus(amount) is allocated while reading the
 input, even if the size of the result is less than @rhombus(amount)
 characters.) If no characters are available before an end-of-file,
 then @rhombus(Port.eof) is returned.

}

@doc(
  enum Port.ReadLineMode:
    linefeed
    return
    return_linefeed
    any
    any_one
){

 Line reading modes for @rhombus(Port.Input.read_line).

}

@doc(
  fun Port.Output.print(out :: Port.Output,
                        v :: Any, ...,
                        ~mode: mode :: PrintMode = #'text)
    :: Void
  fun Port.Output.println(out :: Port.Output,
                          v :: Any, ...,
                          ~mode: mode :: PrintMode = #'text)
    :: Void
  fun Port.Output.show(out :: Port.Output,
                       v :: Any, ...,
                       ~mode: mode :: PrintMode = #'text)
    :: Void
  fun Port.Output.showln(out :: Port.Output,
                         v :: Any, ...,
                         ~mode: mode :: PrintMode = #'text)
    :: Void
){

 The same as @rhombus(print), @rhombus(println), @rhombus(show), and
 @rhombus(showln), but with an output provided as a required intiial
 argument instead of an optional @rhombus(~out) keyword argument.

}

@doc(
  fun Port.Output.open_bytes(name :: Symbol) :: Port.Output
  fun Port.Output.open_string(name :: Symbol) :: Port.Output
){

 Creates an @tech{output port} that accumulates the output into a
 @tech{byte string}. The optional @rhombus(name) argument is used as
 the name for the returned port.

 @rhombus(Port.Output.open_string) does the same as
 @rhombus(Port.Output.open_bytes), but can be used to clarify the
 intention together with @rhombus(Port.Output.get_string).

}

@doc(
  fun Port.Output.get_bytes(out :: Port.Output) :: Bytes
  fun Port.Output.get_string(out :: Port.Output) :: String
){

 @rhombus(Port.Output.get_bytes) returns the bytes accumulated in the
 @tech{output port} @rhombus(out) so far in a freshly allocated
 @tech{byte string} (including any bytes written after the port's
 current position, if any).

 @rhombus(Port.Output.get_string) is like
 @rhombus(Port.Output.get_bytes), but returns a string converted from
 the byte string instead.

}

@doc(
  fun Port.Output.flush(out :: Port.Output = Port.Output.current())
    :: Void
){

 Flushes the content of @rhombus(out)'s buffer.

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
  value and an optional @rhombus(~mode) like @rhombus(Printable.describe)
  (unlike @rhombus(Printable.describe), the @rhombus(~mode) defaults to
  @rhombus(#'expr), which is generally desirable when printing subcomponents);
  the @rhombus(recur, ~var) function is specific to a particular overall print
  action so that it can handle cycles and graph references.}

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
    out :: Port.Output = Port.Output.current(),
    ~column: column :: NonnegInt = 0
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

 Satisified by a @tech{string}, @tech{byte string}, or opaque result
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
  fun PrintDesc.nest(n :: NonnegInt, pd :: PrintDesc)
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

 @item{@rhombus(PrintDesc.or) offers two printign alternatives. Either
  @rhombus(pd1) or @rhombus(pd2) will be printed, depending on choices
  made by a pretty-printer configuration and as constrainted by
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
                        ~length: length :: NonnegInt = 1,
                        ~mode: mode :: Any.of(#'#{write-special},
                                              #'print,
                                              #'write,
                                              #'display)
                                 = #'#{write-special})
    :: PrintDesc
){


 Prints @rhombus(v) using Racket printing when the output port supports
 ``special'' output, otherwise prints as the given @rhombus(alt_pd). For
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
  Parameter.def Printable.current_page_width :: NonnegInt
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
