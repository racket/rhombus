#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Input Ports}

An @deftech{input port} is a @tech{port} specifically for input.
Moreover, an @deftech{input string port} reads from a @tech{byte
 string}.

@doc(
  annot.macro 'Port.Input'
  annot.macro 'Port.Input.String'
){

 The @rhombus(Port.Input, ~annot) annotation
 recognizes @tech{input ports}.
 The @rhombus(Port.Input.String, ~annot) annotation recognizes
 @tech{input string ports}.
}

@doc(
  Parameter.def Port.Input.current :: Port.Input
){

 A @tech{context parameter} for the default port to use when reading.

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
  fun Port.Input.open_file(file :: PathString,
                           ~mode: mode :: Port.Mode = #'binary)
    :: Port.Input
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
  method (in :: Port.Input).peek_byte(
    ~skip_bytes: skip :: NonnegInt = 0
  ):: Byte || Port.EOF
){

 Like @rhombus(Port.Input.read_byte), but peeks instead of reading, and skips
 @rhombus(skip) bytes at the start of the port.

}

@doc(
  method (in :: Port.Input).peek_bytes(
    amount :: NonnegInt,
    ~skip_bytes: skip :: NonnegInt = 0
  ) :: Bytes || Port.EOF
){

 Like @rhombus(Port.Input.read_bytes), but peeks instead of reading, and skips
 @rhombus(skip) bytes at the start of the port.

}

@doc(
  method (in :: Port.Input).peek_char(
    ~skip_bytes: skip :: NonnegInt = 0
  ) :: Char || Port.EOF
){

 Like @rhombus(Port.Input.read_char), but peeks instead of reading, and skips
 @rhombus(skip) bytes (not characters) at the start of the port.

}

@doc(
  method (in :: Port.Input).peek_string(
    amount :: NonnegInt,
    ~skip_bytes: skip :: NonnegInt = 0
  ) :: String || Port.EOF
){

 Like @rhombus(Port.Input.read_string), but peeks instead of reading, and skips
 @rhombus(skip) bytes at the start of the port.

}

@doc(
  method (in :: Port.Input).read_byte()
    :: Byte || Port.EOF
){

 Reads a single byte from @rhombus(in). If no bytes are available before and
 end-of-file, then @rhombus(Port.eof) is returned.

}

@doc(
  method (in :: Port.Input).read_bytes(amount :: NonnegInt)
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
  method (in :: Port.Input).read_char()
    :: Char || Port.EOF
){

 Reads a single character from @rhombus(in) --- which may involve reading
 several bytes to UTF-8-decode them into a character; a minimal number of
 bytes are read/peeked to perform the decoding. If no bytes are available
 before an end-of-file, then @rhombus(Port.eof) is returned.

}

@doc(
  method (in :: Port.Input).read_line(
    ~mode: mode :: Port.ReadLineMode = #'any
  ) :: String || Port.EOF
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
  method (in :: Port.Input).read_string(amount :: NonnegInt)
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
