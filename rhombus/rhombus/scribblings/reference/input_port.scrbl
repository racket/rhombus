#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/thread open)

@title{Input Ports}

An @deftech{input port} is a @tech{port} specifically for input.
Moreover, an @deftech{input string port} reads from a @tech{byte
 string}.

@doc(
  annot.macro 'Port.Input'
  annot.macro 'Port.Input.String'
  annot.macro 'Port.Input.Progress'
){

 The @rhombus(Port.Input, ~annot) annotation
 recognizes @tech{input ports}.
 The @rhombus(Port.Input.String, ~annot) annotation recognizes
 @tech{input string ports}.
 The @rhombus(Port.Input.Progress, ~annot) annotation recognizes
 input ports that can provide a progress event via @rhombus(Port.Input.Progress.evt).

}


@doc(
  ~nonterminal:
    port_expr: block expr
    path_expr: block expr

  Parameter.def Port.Input.current :: Port.Input

  expr.macro 'stdin'

  expr.macro 'Port.Input.using $port_expr:
                $body
                ...'
  expr.macro 'Port.Input.using ~file $path_expr:
                $body
                ...'
){

 The @rhombus(Port.Input.current) @tech{context parameter} determines a
 default port to use when reading. The @rhombus(stdin) form is a
 shorthand for @rhombus(Port.Input.current()) to get that default port.

 The @rhombus(Port.Input.using) form sets @rhombus(Port.Input.current)
 while evaluating the @rhombus(body) result. If @rhombus(~file) is not
 supplied, then @rhombus(port_expr) must produce a
 @rhombus(Port.Input, ~annot), otherwise @rhombus(path_expr) must produce
 a @rhombus(PathString). Breaks are disabled during the evaluation of
 @rhombus(port_expr) or opening the file indicated by
 @rhombus(path_expr), the same as for @rhombus(Closeable.let), and the
 port is similarly closed on return or escape from the @rhombus(body)
 sequence.

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
    :: Port.Input && Port.FileStream
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
  fun Port.Input.open_nowhere(name :: Symbol = #'nowhere)
    :: Port.Input
){

 Creates an @tech{input port} that is empty. The optional @rhombus(name)
 is used as the name for the returned port.

}


@doc(
  method (in :: Port.Input).close() :: Void
){

 Closes an @tech{input port}.

}


@doc(
  method (in :: Port.Input).read_byte(
    ~special_wrap: special_wrap :: maybe(Any -> Any) = #false,
    ~source_name: source_name :: Any = #false
  ) :: Byte || Port.EOF || Any
){

 Normally reads a single byte from @rhombus(in). If no bytes are
 available before an end-of-file, then @rhombus(Port.eof) is returned.

 If @rhombus(special_wrap) is not @rhombus(#false) and if the port can
 supply ``special'' non-byte results, then the result can be
 @rhombus(special_wrap) applied to the special result. Supply
 @rhombus(values) as @rhombus(special_wrap) to enable special results
 without extra treatment. The @rhombus(source_name) argument can be
 anything, and it is delivered to the port's implementation to
 potentially specialize its behavior.

}


@doc(
  method (in :: Port.Input).read_bytes(amount :: NonnegInt)
    :: Bytes || Port.EOF
){

 Reads a @tech{byte string} containing the next @rhombus(amount) bytes from
 @rhombus(in). If @rhombus(amount) is @rhombus(0), then an empty byte string is returned.
 Otherwise if fewer than @rhombus(amount) bytes are available before an
 end-of-file is encountered, then the returned byte string will contain only
 those bytes before the end-of-file; that is, the returned byte string's length
 will be less than @rhombus(amount). If no bytes are available before an
 end-of-file, then @rhombus(Port.eof) is returned.

}


@doc(
  method (in :: Port.Input).read_bytes_to(
    bytes :: MutableBytes,
    ~start: start :: NonnegInt = 0,
    ~end: end :: NonnegInt = bytes.length(),
    ~wait: wait :: Port.WaitMode = #'all
  ) :: NonnegInt || Port.EOF
){

 Like @rhombus(Port.Input.read_bytes), but delivering the read bytes to
 a mutable byte string, @rhombus(bytes). The read bytes are written to
 the @rhombus(start) through @rhombus(end) substring of @rhombus(bytes).
 The result is the number of bytes written to @rhombus(bytes); the result
 is @rhombus(0) only when @rhombus(start == end), otherwise
 @rhombus(Port.eof) is returned if no bytes are read due to an
 end-of-file.

 If @rhombus(wait) is @rhombus(#'all), then @rhombus(end-start) bytes
 are written to @rhombus(bytes) unless an end-of-file is found in the
 input. If @rhombus(wait) is @rhombus(#'some), then the number of read
 bytes may be fewer, but reading will wait until at least one byte is
 read or an end-of-file is found. If @rhombus(wait) is @rhombus(#'none),
 then reading will always return without waiting, even if no bytes are
 immediately available. If @rhombus(wait) is @rhombus(#'enable_break),
 then waiting is like @rhombus(#'some), but asynchronous break exceptions
 are enabled during the wait; in that case, if breaks are disabled before
 the call to @rhombus(Port.Input.read_bytes_to) either some bytes will be
 read or a break exception will be thrown, but not both.

}


@doc(
  method (in :: Port.Input).read_char(
    ~special_wrap: special_wrap :: maybe(Any -> Any) = #false,
    ~source_name: source_name :: Any = #false
  ) :: Char || Port.EOF || Any
){

 Reads a single character from @rhombus(in) --- which may involve reading
 several bytes to UTF-8-decode them into a character; a minimal number of
 bytes are read/peeked to perform the decoding. If no bytes are available
 before an end-of-file, then @rhombus(Port.eof) is returned.

 The result can be a non-@rhombus(Char), non-@rhombus(Port.eof) value if
 @rhombus(special_wrap) is not @rhombus(#false). The
 @rhombus(special_wrap) and @rhombus(source_name) argument as used as by
 @rhombus(Port.Input.peek_byte).

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
  method (in :: Port.Input).read_string_to(
    str :: String,
    ~start: start :: NonnegInt = 0,
    ~end: end :: NonnegInt = str.length()
  ) :: NonnegInt || Port.EOF
){


 Like @rhombus(Port.Input.read_bytes_to), but delivering decoded
 characters to a mutable string, @rhombus(str) in its @rhombus(start)
 through @rhombus(end) substring.

}


@doc(
  method (in :: Port.Input).read_bytes_line(
    ~mode: mode :: Port.Input.ReadLineMode = #'any
  ) :: Bytes || Port.EOF
  method (in :: Port.Input).read_line(
    ~mode: mode :: Port.Input.ReadLineMode = #'any
  ) :: String || Port.EOF
){

 Returns a byte string or string containing the next line of bytes or
 characters from @rhombus(in).

 Bytes are read from @rhombus(in) until a line separator or an
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
  method (in :: Port.Input).bytes_lines(
    ~mode: mode :: Port.Input.ReadLineMode = #'any
  ) :: Sequence.expect_of(Bytes) && Listable.expect_of(Bytes)
  method (in :: Port.Input).lines(
    ~mode: mode :: Port.Input.ReadLineMode = #'any
  ) :: Sequence.expect_of(String) && Listable.expect_of(String)
){

 Returns a @tech{listable} @tech{sequence} that reads lines from
 @rhombus(in) on demand. The @rhombus(mode) argument determines line
 parsing the same as for @rhombus(Port.Input.read_line).

@examples(
  def p = Port.Input.open_string("a\nb\nc")
  for (ln in p.lines()):
    showln(ln)
)

}


@doc(
  method (in :: Port.Input).peek_byte(
    ~skip_bytes: skip :: NonnegInt = 0,
    ~special_wrap: special_wrap :: maybe(Any -> Any) = #false,
    ~source_name: source_name :: Any = #false
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
  method (in :: Port.Input).peek_bytes_to(
    bytes :: MutableBytes,
    ~start: start :: NonnegInt = 0,
    ~end: end :: NonnegInt = bytes.length(),
    ~skip_bytes: skip :: NonnegInt = 0,
    ~wait: wait :: Port.WaitMode = #'all,
    ~progress: progress :: maybe(ProgressEvt) = #false
  ) :: NonnegInt || Port.EOF
){

 Like @rhombus(Port.Input.peek_bytes), but delivering the read bytes to
 a mutable byte string, @rhombus(bytes). The read bytes are written to
 the @rhombus(start) through @rhombus(end) substring of @rhombus(bytes).
 The result is the number of bytes written to @rhombus(bytes).

 If @rhombus(wait) is not @rhombus(#'all), then @rhombus(progress) can
 be an event produced by @rhombus(Port.Input.Progress.evt(port)). Bytes
 are peeked only when @rhombus(progress) is not ready for
 synchronization, otherwise the result is @rhombus(0).

}


@doc(
  method (in :: Port.Input).peek_char(
    ~skip_bytes: skip :: NonnegInt = 0,
    ~special_wrap: special_wrap :: maybe(Any -> Any) = #false,
    ~source_name: source_name :: Any = #false
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
  method (in :: Port.Input).peek_string_to(
    str :: MutableString,
    ~start: start :: NonnegInt = 0,
    ~end: end :: NonnegInt = str.length(),
    ~skip_bytes: skip :: NonnegInt = 0
  ) :: NonnegInt || Port.EOF
){

 Like @rhombus(Port.Input.peek_string), but delivering decoded
 characters to a mutable string, @rhombus(str) in its @rhombus(start)
 through @rhombus(end) substring.

}


@doc(
  method (in :: Port.Input).copy_to(out :: Port.Output, ...) :: Void
){

 Read from @rhombus(in) until an end-of-file and writes all read content
 to @rhombus(out). Bytes are read from @rhombus(in) as available and
 written to each @rhombus(out) before continuing (as opposed to waiting
 until all data can be read from @rhombus(in)).

}

@doc(
  method (port :: Port.Input.Progress).evt() :: ProgressEvt
  method (port :: Port.Input.Progress).is_evt(e :: ProgressEvt) :: Boolean
){

 The @rhombus(Port.Input.Progress.evt) method returns a a @tech{synchronizable
 event} that becomes ready for synchronization when data is read from
 @rhombus(port). The @rhombus(Port.Input.Progress.evt) method check whether
 @rhombus(e) belongs to @rhombus(port).

}


@doc(
  method (port :: Port.Input.Progress).commit(
    amt :: NonnegInt,
    progress :: ProgressEvt,
    evt :: CommitEvt
  ) :: Boolean
){

 Attempts to commit as read the first @rhombus(amt) previously peeked
 bytes, non-byte specials, and @rhombus(Port.eof)s from @rhombus(port),
 or the first @rhombus(Port.eof) or special value peeked from
 @rhombus(post). Mid-stream @rhombus(Port.eof)s can be committed, but a
 @rhombus(Port.eof) when the port is exhausted does not necessarily
 commit, since it does not correspond to data in the stream.

 The read commits only if @rhombus(progress) does not become ready first
 (i.e., if no other process reads from @rhombus(port) first), and only if
 @rhombus(evt) is chosen by a sync within port-commit-peeked (in which
 case the event result is ignored); the @rhombus(evt) must be either a
 channel-put event, channel, semaphore, semaphore-peek event, ``always''
 event, or ``never'' event. Suspending the thread that calls
 @rhombus(Port.Input.Progress.commit) may or may not prevent the commit
 from proceeding.

 The result is @rhombus(#true) if data has been committed, and
 @rhombus(#false) otherwise.

 If no data has been peeked from @rhombus(port) and @rhombus(progress)
 is not ready, then an @rhombus(Exn.Fail.Annot) exception is thrown. If
 fewer than @rhombus(amt) items have been peeked at the current start of
 @rhombus(port)'s stream, then only the peeked items are committed as
 read. If @rhombus(port)'s stream currently starts at a
 @rhombus(Port.eof) or a non-byte special value, then only the
 @rhombus(Port.eof) or special value is committed as read.

 If @rhombus(progress) is not a result of
 @rhombus(Port.Input.Progress.evt(port)), then an
 @rhombus(Exn.Fail.Annot) exception is thrown.

}


@doc(
  enum Port.Input.ReadLineMode:
    linefeed
    return
    return_linefeed
    any
    any_one
){

 Line reading modes for @rhombus(Port.Input.read_line).

}
