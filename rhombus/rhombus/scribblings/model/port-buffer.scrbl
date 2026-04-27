#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread open)

@title(~tag: "port-buffer"){Ports Buffers and Positions}

Some ports---especially those that read from and write to files---are
internally buffered:

@itemlist(

 @item{An @tech{input port} is typically block-buffered by default,
 which means that on any read, the buffer is filled with
 immediately-available bytes to speed up future reads. Thus, if a file is
 modified between a pair of reads to the file, the second read can
 produce stale data. Calling @rhombus(Port.position) to set an input
 port's file position flushes its buffer.}

 @item{An @tech{output port} is typically block-buffered by default,
 though a terminal output port is line-buffered, and the initial error
 output port is unbuffered. An output buffer is filled with a sequence of
 written bytes to be committed as a group, either when the buffer is full
 (in block mode), when a newline is written (in line mode), when the port
 is closed via @rhombus(Port.close), or when a flush is explicitly
 requested via a procedure like @rhombus(Port.Output.flush).}

)

If a port supports buffering, its buffer mode can be changed via
@rhombus(Port.buffer).

For an input port, peeking always places peeked bytes into the port's
buffer, even when the port's buffer mode is @rhombus(#'none).
Furthermore, on some platforms, testing the port for input (via
@rhombus(Evt.sync) may be implemented with a peek. If an input port's
buffer mode is @rhombus(#'none), then at most one byte is read for
@rhombus(Port.Input.read_bytes) in @rhombus(#'some) mode; if any bytes
are buffered in the port (e.g., to satisfy a previous peek), the
procedures may access multiple buffered bytes, but no further bytes are
read.

In addition, the initial current output and error ports are
automatically flushed when they are terminal ports (see
@rhombus(Port.FileStream.Terminal, ~annot) and when
@rhombus(Port.Input.read_bytes), @rhombus(Port.Input.read_string),
@rhombus(Port.Input.read_line), etc., are performed on the initial input
port.
