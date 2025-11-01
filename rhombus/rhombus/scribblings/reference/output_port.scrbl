#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Output Ports}

An @deftech{output port} is a @tech{port} specifically for output, and
an @deftech{output string port} writes to a @tech{byte string}.

@doc(
  annot.macro 'Port.Output'
  annot.macro 'Port.Output.String'
  annot.macro 'Port.Output.Special'
){

 The @rhombus(Port.Output, ~annot) annotation recognizes @tech{output
  ports}, the @rhombus(Port.Output.String, ~annot) annotation recognizes
 @tech{output string ports}, and the
 @rhombus(Port.Output.Special, ~annot) annotation recognizes ports that
 support @rhombus(Port.Output.Special.write).

}


@doc(
  ~nonterminal:
    port_expr: block expr
    path_expr: block expr
    exists_mode_expr: block expr
    exists_mode_body: block body

  Parameter.def Port.Output.current :: Port.Output
  Parameter.def Port.Output.current_error :: Port.Output

  expr.macro 'stdout'
  repet.macro 'stdout'
  expr.macro 'stderr'
  repet.macro 'stderr'

  expr.macro 'Port.Output.using $port_expr:
                $body
                ...'
  expr.macro 'Port.Output.using ~file $path_expr:
                $option; ...
                $body
                ...'

  grammar option:
    ~exists $exists_mode_expr
    ~exists: $exists_mode_body; ...
){

 The @rhombus(Port.Output.current) @tech{context parameter} determines a
 default port to use when printing, and @rhombus(Port.Output.current)
 determines the default port to use when printing errors.

 The @rhombus(stdout) form is a shorthand for
 @rhombus(Port.Output.current()), and the @rhombus(stderr) form is a
 shorthand for @rhombus(Port.Output.current_error()).

 The @rhombus(Port.Output.using) form is analogous to
 @rhombus(Port.Input.using), but for setting the current output port
 while evaluating the @rhombus(body) sequence. When the @rhombus(~file)
 variant is used, an @rhombus(~exists) option before the @rhombus(body)
 sequence indicates how to handle the case that a file with the indicated
 path exists, the same as the @rhombus(~exists) argument to
 @rhombus(Port.Output.open_file).

}


@doc(
  fun Port.Output.open_file(
    path :: PathString,
    ~exists: exists_flag :: Port.Output.ExistsMode = #'error,
    ~mode: mode :: Port.Mode = #'binary,
    ~permissions: permissions :: Int.in(0, 65535) = 0o666,
    ~replace_permissions: replace_permissions = #false
  ) :: Port.Output && Port.FileStream
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
 intention together with @rhombus(Port.Output.String.get_string).

}


@doc(
  method (out :: Port.Output.String).get_bytes() :: Bytes
  method (out :: Port.Output.String).get_string() :: String
){

 @rhombus(Port.Output.String.get_bytes) returns the bytes accumulated in the
 @tech{output string port} @rhombus(out) so far in a freshly allocated
 @tech{byte string} (including any bytes written after the port's
 current position, if any).

 @rhombus(Port.Output.String.get_string) is like
 @rhombus(Port.Output.String.get_bytes), but returns a string converted from
 the byte string instead.

}


@doc(
  fun Port.Output.open_nowhere(name :: Symbol = #'nowhere)
    :: Port.Output
){

 Creates an @tech{output port} that discards all output written to the
 port. The optional @rhombus(name) is used as the name for the returned
 port.

}


@doc(
  method (out :: Port.Output).close() :: Void
){

 Closes an @tech{output port}.

}


@doc(
  method (out :: Port.Output).write_byte(b :: Byte) :: Void
  method (out :: Port.Output).write_char(ch :: Char) :: Void
){

 Writes a single byte or a single (UTF-8 encoded) @tech{character} to
 @rhombus(out).

}

@doc(
  method (out :: Port.Output).write_bytes(
    bytes :: Bytes,
    ~start: start :: Nat = 0,
    ~end: end :: Nat = bytes.length(),
    ~wait: wait :: Port.WaitMode = #'all
  ) :: Nat
){

 Writes bytes from the @rhombus(start) to @rhombus(end) substring of
 @rhombus(bytes) and returns the number of bytes that are written.

 Writing to @rhombus(out) may block, depending on the backing device.
 If @rhombus(wait) is @rhombus(#'all), then all bytes are written, but
 not necessarily flushed (see @rhombus(Port.Output.flush)), and the
 result is @rhombus(end-start). If @rhombus(wait) is
 @rhombus(#'one), then writing will block until at least one byte is
 written and immediately flushed, and the result is the number of bytes
 written. If @rhombus(wait) is @rhombus(#'none), then writing never
 blocks, and the result is the number of bytes written and immediately
 flushed. If @rhombus(wait) is @rhombus(#'enable_break), then writing
 blocks in the same way as for @rhombus(#'one), but asynchronous break
 exceptions are enabled during the wait; in that case, if breaks are
 disabled before the call to @rhombus(Port.Output.write_bytes) either some
 bytes will be written or a break exception will be thrown, but not both.

}


@doc(
  method (out :: Port.Output).write_string(
    str :: ReadableString,
    ~start: start :: Nat = 0,
    ~end: end :: Nat = bytes.length()
  ) :: Nat
){

 Like @rhombus(Port.Output.write_bytes) with @rhombus(#'all) waiting,
 but writing a (UTF-8 encoded) substring of @rhombus(str) from
 @rhombus(start) to @rhombus(end). The result is the number of characters
 written.

}


@doc(
  method (out :: Port.Output).print(v :: Any, ...,
                                    ~mode: mode :: PrintMode = #'text)
    :: Void
  method (out :: Port.Output).println(v :: Any, ...,
                                      ~mode: mode :: PrintMode = #'text)
    :: Void
  method (out :: Port.Output).show(v :: Any, ...,
                                   ~mode: mode :: PrintMode = #'text)
    :: Void
  method (out :: Port.Output).showln(v :: Any, ...,
                                     ~mode: mode :: PrintMode = #'text)
    :: Void
){

 The same as @rhombus(print), @rhombus(println), @rhombus(show), and
 @rhombus(showln), but with an output provided as a required initial
 argument instead of an optional @rhombus(~out) keyword argument.

}


@doc(
  method Port.Output.flush(out :: Port.Output = stdout) :: Void
){

 Flushes the content of @rhombus(out)'s buffer.

}


@doc(
  method (port :: Port.Output.Special).write(v) :: Void
){

 Writes an arbitrary ``special'' value to a port that supports content
 other than just bytes.

}


@doc(
  enum Port.Output.ExistsMode:
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
