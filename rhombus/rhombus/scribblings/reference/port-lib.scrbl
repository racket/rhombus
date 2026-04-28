#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/port
      rhombus/bytes)

@(def port_eval = make_rhombus_eval())

@examples(
  ~eval: port_eval
  ~hidden:
    import:
      rhombus/port
)

@title(~tag: "port-lib"){Port Conversions}

@docmodule(rhombus/port)

The @rhombusmodname(rhombus/port) library provides functions for
adjusting and combining ports.

@doc(
  fun port.input.append(
    in :: Port.Input,
    ...,
    ~close: close :: Any.to_boolean = #false,
    ~name: name = PairList[in.name(), ...]
  ) :: Port.Input
){

 Takes any number of input ports and returns a new input port. Reading
 from the new input port draws bytes (and special non-byte values) from
 the given input ports in order. If @rhombus(close) is true, then each
 port is closed when an end-of-file is encountered from the port, or when
 the result input port is closed. Otherwise, data not read from the
 returned input port remains available for reading in its original input
 port.

 The @rhombus(name) argument determines the name as reported by
 @rhombus(Port.name) for the returned input port.

 See also @rhombus(port.input.interleave), which interleaves data from
 multiple input ports as it becomes available.

}
  
@doc(
  fun port.input.interleave(
    in :: Port.Input,
    ...,
    ~buffer_limit: buffer_limit :: maybe(Nat) = 4096
  ) :: Port.Input
){

 Takes any number of input ports and returns a new input port. The new
 port merges the data from all original ports, so data can be read from
 the new port whenever it is available from any of the two original
 ports. The data from the original ports are interleaved. When an
 end-of-file has been read from an original port, it no longer
 contributes characters to the new port. After an end-of-file has been
 read from all original ports, the new port returns end-of-file. Closing
 the merged port does not close the original ports.

 The optional @rhombus(buffer_limit) argument limits the number of bytes
 to be buffered from any one port, so that the merge process does not
 advance arbitrarily beyond the rate of consumption of the merged data. A
 @rhombus(#false) value disables the limit.

 See also @rhombus(port.input.append), which concatenates input streams
 instead of interleaving them.

}
  
@doc(
  fun port.output.tee(
    out :: Port.Output,
    ...
  ) :: Port.Output
){

 Accepts any number of output ports and returns a new output port. When
 bytes are written to the new port, it first writes as many bytes as
 possible to the first original port, and then tries to write the same
 number of bytes to the next port, and so on. If writing doesn't succeed,
 what is left over is buffered and no further writes can go through until
 the original ports can receive the bytes written so far.

 The new port is ready as a @tech{synchronizable event} when all
 original ports reports being ready. However, the first port may stop
 being ready while waiting for a later port to be ready, so it cannot be
 guaranteed that all ports are ready at once.

 When the new port is closed, the close operation blocks until all bytes
 are written to all original ports.

}

@doc(
  fun port.copy(
    in :: Port.Input,
    out :: Port.Output,
    ...,
    ~in_encoding: in_encoding :: String = "UTF-8",
    ~out_encoding: out_encoding :: String = "UTF-8"
  ) :: Void
){

 Reads data from @rhombus(in) and writes it back out to each
 @rhombus(out), returning when reading from @rhombus(in) produces
 @rhombus(Port.eof). The copy is efficient, and it is without significant
 buffer delays (i.e., a byte that becomes available on @rhombus(in) is
 immediately transferred to the first @rhombus(out), even if future reads
 on @rhombus(in) must block). If @rhombus(in) produces a special non-byte
 value, it is transferred to @rhombus(out) using
 @rhombus(Port.Output.Special.write).

 This function is often called from a ``background'' thread to
 continuously pump data from one stream to another.

 If multiple @rhombus(out)s are provided, data from @rhombus(in) is
 written to every @rhombus(out). The different @rhombus(out)s block
 output to each other, because each block of data read from @rhombus(in)
 is written completely to one @rhombus(out) before moving to the next
 @rhombus(out). The @rhombus(out)s are written in the provided order, so
 non-blocking ports (e.g., file output ports) should be placed first in
 the argument list.

 If @rhombus(in_encoding) and @rhombus(out_encoding) are not both
 @rhombus("UTF-8"), then data from @rhombus(in), is converted using using
 @rhombus(bytes.Converter(~from: in_encoding, ~to: out_encoding)) before
 being written to each @rhombus(out). If opening the converter fails, an
 @rhombus(Exn.Fail) exception is thrown. Similarly, if a conversion error
 occurs at any point while reading from @rhombus(in), an
 @rhombus(Exn.Fail) exception is thrown.

}

@doc(
  fun port.input.dup(
    in :: Port.Input,
    ~close: close :: Any.to_boolean = #false
  ) :: Port.Input

  fun port.output.dup(
    out :: Port.Output,
    ~close: close :: Any.to_boolean = #false
  ) :: Port.Output
){

 Returns an input port that draws directly from @rhombus(in) or an
 output port that writes directory to @rhombus(out). Closing the
 resulting port closes the original @rhombus(in) or @rhombus(out) only
 @rhombus(close) is true.

}

@doc(
  fun port.input.peeking(
    in :: Port.Input,
    ~name: name :: Any = in.name(),
    ~skip: skip :: Nat = 0,
    ~init_position: init_position :: PosInt = 1
  ) :: Port.Input
){

 Returns an input port whose content is determined by peeking into
 @rhombus(in). In other words, the resulting port contains an internal
 skip count, and each read of the port peeks into @rhombus(in) with the
 internal skip count, and then increments the skip count according to the
 amount of data successfully peeked.

The optional @rhombus(name) argument is the name of the resulting
 port. The @rhombus(skip) argument is the port initial skip count.

 The resulting port's initial position (as reported by
 @rhombus(Port.position)) is @rhombus(init_position-1), no matter the
 position of @rhombus(in).

 The resulting port supports buffering (see
 @secref(~doc: model_doc, "port-buffer")), and a @rhombus(#'block) buffer
 mode allows the port to peek further into @rhombus(in) than requested.
 The resulting port's initial buffer mode is @rhombus(#'block), unless
 @rhombus(in) supports buffering and its mode is initially
 @rhombus(#'none) (i.e., the initial buffer mode is taken from
 @rhombus(in) when it supports buffering). If @rhombus(in) supports
 buffering, adjusting the resulting port's buffer mode via
 @rhombus(Port.buffer) adjusts @rhombus(in)'s buffer mode.

 For example, reading from a peeking port produces the same results as
 reading from the original port:

@examples(
  ~eval: port_eval
  ~repl:
    def orig_in = Port.Input.open_string("123456789")
    def peek_in = port.input.peeking(orig_in)
    peek_in.buffer(#'none)
    peek_in.read_string(3)
    peek_in.read_string(2)
    orig_in.read_string(3)
)

 Beware that the reading from the original port is invisible to the
 peeking port, which keeps its own separate internal counter, and thus
 interleaving reads on the two ports can produce confusing results.
 Continuing the above example, if we read three more characters from the
 peeking port, we end up skipping over the @litchar{678} in the
 port----but only because buffering as disabled for the peeking port:

@examples(
  ~eval: port_eval
  ~repl:
    peek_in.read_string(2)
)

 If @rhombus(peek_in, ~var) had remained buffered, then
 @rhombus(peek_in.read_string(2)) likely would have produced
 @rhombus("67") out of previously buffered bytes.

}

@doc(
  fun port.input.limit(
    in :: Port.Input,
    limit :: Nat,
    ~close: close :: Any.to_boolean = #false
  ) :: Port.Input:
    rkt.#{make-limited-input-port}(in, limit, close)
){

 Returns a port whose content is drawn from @rhombus(in), but where an
 end-of-file is reported after @rhombus(limit) bytes (and non-byte
 special values) have been read. If @rhombus(close) is true, then the
 original port is closed if the returned port is closed.

 Bytes are consumed from @rhombus(in) only when they are consumed from
 the returned port. In particular, peeking into the returned port peeks
 into the original port.

 If @rhombus(in) is used directly while the resulting port is also used,
 then the @rhombus(limit) bytes provided by the port need not be
 contiguous parts of the original port's stream.

}

@doc(
  fun port.input.reencode(
    in :: Port.Input,
    ~from: from :: String,
    ~name: name :: Any = in.name(),
    ~error_bytes: error_bytes :: maybe(Bytes) = #false,
    ~convert_newlines: convert_newlines :: Any.to_boolean = #false,
    ~close: close :: Any.to_boolean = #false,
    ~encode_fail: encode_fail :: (String, Port.Output) -> Never
                    = fun (msg, in): error(....)
  ) :: Port.Input
  
  fun port.output.reencode(
    out :: Port.Output,
    ~from: from :: String,
    ~name: name = out.name(),
    ~error_bytes: error_bytes :: maybe(Bytes) = #false,
    ~convert_newlines = #false,
    ~close: close :: Any.to_boolean = #false,
    ~encode_fail: encode_fail :: (String, Port.Output) -> Never
                    = fun (msg, in): error(....)
  ) :: Port.Output
){

 Produces a new input port that draws bytes from @rhombus(in) or a new
 output port that propagates bytes to @rhombus(out), but converts the
 byte stream using @rhombus(bytes.Converter(~from: from, ~to: "UTF-8")).
 In addition, if @rhombus(convert_newlines) is true, then decoded
 sequences that correspond to UTF-8 encodings of @rhombus("\r\n"),
 @rhombus("\r\x85"), @rhombus("\r"), @rhombus("\x85"), and
 @rhombus("\u2028") are all converted to the UTF-8 encoding of
 @rhombus("\n").
 
 If @rhombus(error_bytes) is provided and not @rhombus(#false), then the
 given byte sequence is used in place of bytes from @rhombus(in) or to
 the new output port that trigger conversion errors. Otherwise, if a
 conversion is encountered, @rhombus(encode_fail) is called, which must
 throw an exception or otherwise escape.

 If @rhombus(close) is true, then closing the new input or output port
 also closes @rhombus(in) or @rhombus(out). The @rhombus(name) argument
 is used as the name of the new port.

 In the case of an input port, in non-buffered mode (see
 @secref(~doc: model_doc, "port-buffer")), the new input port attempts
 to draw bytes from @rhombus(in) only as needed to satisfy requests.
 Toward that end, the input port assumes that at least @math{n} bytes
 must be read to satisfy a request for @math{n} bytes. This is true even
 if the port has already drawn some bytes, as long as those bytes form an
 incomplete encoding sequence.

 In the case of an output port, the new output port supports buffering,
 and the initial buffer mode is @rhombus(Port.buffer(out) || #'block). In
 @rhombus(#'block) mode, the port's buffer is flushed only when it is
 full or a flush is requested explicitly. In @rhombus(#'line) mode, the
 buffer is flushed whenever a newline or carriage-return byte is written
 to the port. In @rhombus(#'none) mode, the port's buffer is flushed
 after every write. Implicit flushes for @rhombus(#'line) or
 @rhombus(#'none) leave bytes in the buffer when they are part of an
 incomplete encoding sequence.

 A new output port does not support atomic writes. An explicit
 @rhombus(Port.flush) call or @rhombus(Port.Output.Special.write) to the
 output port can hang if the most recently written bytes form an
 incomplete encoding sequence.

 When a new output port is buffered, a @tech{flush callback} is
 registered with the @tech{current plumber} to flush the buffer.

}

@doc(
  fun port.input.open_nowhere(
    ~name: name = #'nowhere
  ) :: Port.Input
  
  fun port.output.open_nowhere(
    ~name: name = #'nowhere,
    ~allow_special: allow_special :: Any.to_boolean = #false
  ) :: Port.Output
){

 Creates an input port that is empty or an output port that discards all
 output sent to it (without blocking).

 The @rhombus(name) argument is used as the port's
 name.

 For an output port, if @rhombus(allow_special) is true, then the
 resulting port satisfies @rhombus(Port.Output.Special, ~annot),
 otherwise it does not.

}

@doc(
  fun port.input.relocate(
    in :: Port.Input,
    line :: maybe(PosInt),
    column :: maybe(Nat),
    position :: maybe(PosInt),
    ~name: name :: Any = in.name(),
    ~close: close :: Any.to_boolean = #false
  ) :: Port.Input

  fun port.output.relocate(
    out :: Port.Output,
    line :: maybe(PosInt),
    column :: maybe(Nat),
    position :: maybe(PosInt),
    ~name: name = out.name(),
    ~close: close :: Any.to_boolean = #false
  ) :: Port.Output
){

 Produces a new input or output port that is equivalent to @rhombus(in)
 or 2rhombus(out) except in how it reports location information (and
 possibly its name). A new input port's content starts with the remaining
 content of @rhombus(in), and a new output writes to @rhombus(out), but
 the new port starts at the given line, column, and position as reported
 by @rhombus(Port.locations). A @rhombus(#false) for the line or column
 means that the line and column will always be reported as
 @rhombus(#false).

 The @rhombus(line) and @rhombus(column) values are used only if line
 counting is enabled for @rhombus(in) or @rhombus(out) and also for the
 new port, typically through @rhombus(Port.locations_enabled). The
 @rhombus(column) value determines the column for the first line (i.e.,
 the one numbered @rhombus(line)), and later lines start at column
 @rhombus(0). The given @rhombus(position) is used even if line counting
 is not enabled.

 When line counting is on for a new input port, reading from
 @rhombus(in) instead of the new port increments location reports for the
 new port. Otherwise, the resulting port's position does not increment
 when data is read from @rhombus(in). Writing to @rhombus(out) similarly
 affects the location for a new output port.

 If @rhombus(close) is true, then closing the new port also closes
 @rhombus(in) or @rhombus(out). If @rhombus(close) is @rhombus(#false),
 then closing the resulting port does not close @rhombus(in).

 The @rhombus(name) argument is used as the name for the new port, where
 the default value keeps the same name as @rhombus(in) or @rhombus(out).

}

@doc(
  fun port.pipe.make_special(
    limit :: Nat,
    ~in_name: in_name = #'pipe,
    ~out_name: out_name = #'pipe
  ) :: values(Port.Input, Port.Output.Special)

){

 Like @rhombus(Port.Pipe.make), but the result ports support non-byte
 values written via @rhombus(Port.Output.Special.write) and received via
 @rhombus(Port.Input.read_byte) with @rhombus(~special_wrap).

 The @rhombus(limit) argument determines the maximum capacity of the
 pipe in bytes, but this limit is disabled if special values are written
 to the pipe before @rhombus(limit) is reached. The limit is re-enabled
 after the special value is read from the pipe.

 The optional @rhombus(in_name) and @rhombus(out_name) arguments
 determine the names of the result ports.

}

@close_eval(port_eval)
