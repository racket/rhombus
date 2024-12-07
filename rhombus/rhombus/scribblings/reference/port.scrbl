#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Ports}

A @deftech{port} is an input or output stream for a file, network
connection, terminal, etc. An @tech{input port} is specifically for
input, while an @tech{output port} is specifically for
output; it is possible for an object to be both an input and output port.

@doc(
  annot.macro 'Port'
){

 The @rhombus(Port, ~annot) annotation is satisfied by a @tech{port}.
 See also @rhombus(Port.Input, ~annot) and @rhombus(Port.Output, ~annot).

}

@doc(
 def Port.eof :: Port.EOF
 bind.macro 'Port.eof'
 annot.macro 'Port.EOF'
){

 The @rhombus(Port.eof) value represents an end-of-file (distinct from
 all other values), and the @rhombus(Port.eof) binding match matches that
 value.

 The @rhombus(Port.EOF, ~annot) annotation is satisfied by the
 @rhombus(Port.eof) value.

}

@doc(
  method (port :: Port).close() :: Void
  method (port :: Port).is_closed() :: Boolean
){

 Closes a port, equivalent to @rhombus(Port.Input.close) or
 @rhombus(Port.Output.close), or checks whether a port has been closed.
 Closing an already-closed port has no effect.

}

@doc(
  method (port :: Port).buffer() :: maybe(Port.BufferMode)
  method (port :: Port).buffer(mode :: Port.BufferMode) :: Void
){

 Gets or sets the buffer mode for @rhombus(port) as @rhombus(#'none),
 @rhombus(#'line) (output only), or @rhombus(#'block). An exception is
 thrown if a port does not support a provided buffer mode. When no
 @rhombus(mode) argument is provided, @rhombus(#false) is returned if the
 buffer mode cannot be determined.

 A port's default buffer mode depends on the communication channel that
 it represents. A file port is nomrally @rhombus(#'block) buffered. The
 initial @rhombus(stdin) port is @rhombus(#'block) buffered. The initial
 @rhombus(stdout) port is @rhombus(#'line) buffered if it writers to a
 terminal, @rhombus(#'block) buffered otherwise. The initial
 @rhombus(stderr) port's buffer mode is @rhombus(#'none).

}


@doc(
  method (port :: Port).position() :: NonnegInt
  method (port :: Port).position(pos :: NonnegInt || Port.EOF) :: Void
){

 Gets or sets (if supported) a port's position, which represents an
 offset in bytes.

 Calling @rhombus(Port.position) without @rhombus(pos) on a port other
 than a @rhombus(Port.FileStream, ~annot),
 @rhombus(Port.Input.String, ~annot), or
 @rhombus(Port.Output.String, ~annot) port returns the number of bytes
 that have been read from that port if the position is known.

 For @rhombus(Port.FileStream, ~annot),
 @rhombus(Port.Input.String, ~annot), or
 @rhombus(Port.Output.String, ~annot) ports, providing @rhombus(pos) sets
 the read/write position relative to the beginning of the file or (byte)
 string if @rhombus(pos) is a number, or to the current end of the file or
 (byte) string if @rhombus(pos) is @rhombus(Port.eof). For other kinds of
 ports, an exception is thrown when @rhombus(pos) is supplied.
 Furthermore, not all @rhombus(Port.FileStream, ~annot) ports support
 setting the position; if @rhombus(pos) is supplied for such a port, the
 @rhombus(Exn.Fail.Filesystem) exception is thrown.

 When file-position sets the position beyond the current size of an
 output file or (byte) string, the file/string is enlarged to size
 @rhombus(pos), and the new region is filled with @rhombus(0) bytes in
 the case of a file. In the case of a file output port, the file might
 not be enlarged until more data is written to the file; in that case,
 beware that writing to a file opened in @rhombus(#'append) mode on Unix
 and Mac OS will reset the file pointer to the end of a file before each
 write, which defeats file enlargement via @rhombus(Port.position). If
 @rhombus(pos) is beyond the end of an input file or (byte) string, then
 reading thereafter returns eof without changing the port’s position.

 When changing the file position for an output port, the port is first
 flushed if its buffer is not empty. Similarly, setting the position for
 an input port clears the port’s buffer (even if the new position is the
 same as the old position). However, although input and output ports
 produced by open-input-output-file share the file position, setting the
 position via one port does not flush the other port’s buffer.

}


@doc(
  method (port :: Port).locations_enabled() :: Boolean
  method (port :: Port).locations_enabled(on) :: Void
  Parameter.def Port.current_enable_locations
    :: Any.to_boolean
  method (port :: Port).next_location()
    :: values(maybe(PostInt), maybe(NonnegInt), maybe(PosInt))
  method (port :: Port).next_location(
    line :: maybe(PostInt),
    column :: maybe(NonnegInt),
    offset :: maybe(PosInt)
  ) :: Void
){

 The @rhombus(Port.locations_enabled) method checks or turns on whether
 line, column, and decoded-character offsets are tracked as bytes are
 read from a port or written to a port. Calling
 @rhombus(Port.locations_enabled) with one argument attempts to enable or
 disable his location tracking, but the port's state may not change,
 either because it does not support tracking or because it does not
 support disabling tracking after it's enabled. The
 @rhombus(Port.current_enable_locations) parameter determines whether
 tracking is enabled by default for a newly opened port, and its initial
 value is @rhombus(#false).

 The @rhombus(Port.next_location) method with zero arguments reports a
 line, column, and offset for the next character to be read from the
 port. If tracking is not enabled, then the first two results will be
 @rhombus(#false), but @rhombus(port.position()+1) may be returned as an
 approximation for the last result (i.e., a position measured in bytes
 used as an approximation of the number of characters read). Calling
 @rhombus(Port.next_location) with arguments attempts to set the next
 location, but the attempt is ignored if location tracking has not been
 enabled or if the port does not support external adjustments.

}

@doc(
  fun Port.Output.open_input_output_file(
    path :: PathString,
    ~exists: exists_flag :: Port.Output.ExistsMode = #'error,
    ~mode: mode :: Port.Mode = #'binary,
    ~permissions: permissions :: Int.in(0, 65535) = 0o666,
    ~replace_permissions: replace_permissions = #false
  ) :: values(Port.Input, Port.Output)
){

 Like @rhombus(Port.Output.open_file), but returns both input and output
 ports.The two ports are connected in that they share the underlying file
 descriptor.

 This procedure is intended for use with special devices that can be
 opened by only one process, such as @filepath{COM1} in Windows. For
 regular files, sharing the file descriptor can be confusing. For
 example, using one port does not automatically flush the other port’s
 buffer, and reading or writing in one port moves the file position (if
 any) for the other port. For regular files, use separate
 @rhombus(Port.Input.open_file) and @rhombus(Port.Output.open_file) calls
 to avoid confusion.

}

@doc(
 enum Port.Mode:
   binary
   text
){

 Modes for reading and writing files that determine how newlines are
 read and written.

}

@doc(
 enum Port.BufferMode:
   none
   line
   block
){

 Buffer modes for input and output ports; see @rhombus(Port.buffer). The
 @rhombus(#'line) buffer mode is supported only for output ports.

}

@doc(
 enum Port.WaitMode:
   all
   some
   none
   enable_break
){

 Modes used for methods like @rhombus(Port.Input.read_bytes_to) and
 @rhombus(Port.Output.write_bytes_to) to determine how they block to wait
 for input or output.

}
