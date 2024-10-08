#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Ports}

A @deftech{port} is an input or output stream for a file, network
connection, terminal, etc. An @deftech{input port} is specifically for
input, while an @deftech{output port} is specifically for
output. Moreover, an @deftech{input string port} or an
@deftech{output string port} reads from or writes to a
@tech{byte string}.

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
  out.flush()
  out.print(arg, ...)
  out.println(arg, ...)
  out.show(arg, ...)
  out.showln(arg, ...)
)

@dispatch_table(
  "output string port"
  Port.Output
  out.get_bytes()
  out.get_string()
)

@doc(
  annot.macro 'Port'
  annot.macro 'Port.Input'
  annot.macro 'Port.Input.String'
  annot.macro 'Port.Output'
  annot.macro 'Port.Output.String'
  annot.macro 'Port.EOF'
){

 The @rhombus(Port, ~annot) annotation is satisfied by a @tech{port}.
 The @rhombus(Port.Input, ~annot) annotation
 recognizes @tech{input ports} specifically, while @rhombus(Port.Output, ~annot)
 recognizes @tech{output ports}, and it is possible for a port to be both.

 Moreover, the @rhombus(Port.Input.String, ~annot) and
 @rhombus(Port.Output.String, ~annot) annotations recognize
 @tech{input string ports} and @tech{output string ports},
 respectively.

 The @rhombus(Port.EOF, ~annot) annotation is satisfied by the
 @rhombus(Port.eof) value.

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
    :: Port.Input.String
){

 Creates an @tech{input string port} that reads bytes from the
 @tech{byte string} @rhombus(bstr). The optional @rhombus(name) is
 used as the name for the returned port.

}

@doc(
  fun Port.Input.open_file(file :: PathString) :: Port.Input
){

 Creates an @tech{input port} that reads from the @tech{path} @rhombus(file).

}

@doc(
  fun Port.Input.open_string(str :: ReadableString,
                             name :: Symbol = #'string)
    :: Port.Input.String
){

 Creates an @tech{input string port} that reads @tech{characters} from
 the @tech{string} @rhombus(str). The optional @rhombus(name) is used
 as the name for the returned port.

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
                           ~mode: mode :: Port.ReadLineMode = #'any)
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
 @rhombus(showln), but with an output provided as a required initial
 argument instead of an optional @rhombus(~out) keyword argument.

}

@doc(
  fun Port.Output.open_file(path :: PathString,
                            ~exists: exists_flag
                                       :: Port.Output.ExistsFlag = #'error)
    :: Port.Output
){

 Creates an @tech{output port} that writes to the @tech{path} @rhombus(file).
 The @rhombus(exists_flag) argument specifies how to handle/require
 files that already exist:

 @itemlist(

  @item{@rhombus(#'error) --- throws @rhombus(Exn.Fail.Filesystem.Exists)
        if the file exists.}

  @item{@rhombus(#'replace) --- remove the old file, if it
        exists, and write a new one.}

  @item{@rhombus(#'truncate) --- remove all old data, if the file
        exists.}

  @item{@rhombus(#'must_truncate) --- remove all old data in an
        existing file; if the file does not exist, the
        @rhombus(Exn.Fail.Filesystem) exception is thrown.}

  @item{@rhombus(#'truncate_replace) --- try @rhombus(#'truncate);
        if it fails (perhaps due to file permissions), try
        @rhombus(#'replace).}

  @item{@rhombus(#'update) --- open an existing file without
        truncating it; if the file does not exist, the
        @rhombus(Exn.Fail.Filesystem) exception is thrown.}

  @item{@rhombus(#'can_update) --- open an existing file without
        truncating it, or create the file if it does not exist.}

  @item{@rhombus(#'append) --- append to the end of the file,
        whether it already exists or not; on Windows,
        @rhombus(#'append) is equivalent to @rhombus(#'update), except that
        the file is not required to exist, and the file position is
        immediately set to the end of the file after opening it.}

)}

@doc(
  fun Port.Output.open_bytes(name :: Symbol = #'string)
    :: Port.Output.String
  fun Port.Output.open_string(name :: Symbol = #'string)
    :: Port.Output.String
){

 Creates an @tech{output string port} that accumulates the output into a
 @tech{byte string}. The optional @rhombus(name) argument is used as
 the name for the returned port.

 @rhombus(Port.Output.open_string) does the same as
 @rhombus(Port.Output.open_bytes), but can be used to clarify the
 intention together with @rhombus(Port.Output.get_string).

}

@doc(
  fun Port.Output.get_bytes(out :: Port.Output.String)
    :: Bytes
  fun Port.Output.get_string(out :: Port.Output.String)
    :: String
){

 @rhombus(Port.Output.get_bytes) returns the bytes accumulated in the
 @tech{output string port} @rhombus(out) so far in a freshly allocated
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
  enum Port.Output.ExistsFlag:
    error
    append
    update
    can_update
    replace
    truncate
    must_truncate
    truncate_replace
){

  Flags for handling existing files when opening @tech{output ports}.

}
