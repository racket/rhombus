#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/custodian open)

@title(~tag: "port"){Ports}

A @deftech{port} (satisfying @rhombus(Port, ~annot)) produces and/or
consumes bytes. An @deftech{input port} (satisfying
@rhombus(Port.Input, ~annot)) produces bytes, while an @deftech{output
 port} (satisfying @rhombus(Port.Output, ~annot)) consumes bytes.
Although a typical port is either an input port or an output port, a
port can be both.

When an input port is provided to a character-based operation, the bytes
are decoded to a character, and character-based output operations
similarly encode the character to bytes. See @secref("encoding") for
more information. In addition to bytes and characters encoded as bytes,
some ports can produce and/or consume arbitrary values as ``special''
results via @rhombus(Port.Output.Special.write) and
@rhombus(Port.Input.read_byte) with @rhombus(~special_wrap).

When a port corresponds to a file, network connection, or some other
system resource, it must be explicitly closed via @rhombus(Port.close)
(or indirectly via @rhombus(Custodian.shutdown_all) to
release low-level resources associated with the port. For any kind of
port, after it is closed, attempting to read from or write to the port
raises @rhombus(Exn.Fail, ~annot).

Data produced by a input port can be @defterm{read} or @defterm{peeked}.
When data is read, it is considered consumed and removed from the port’s
stream. When data is peeked, it remains in the port’s stream to be
returned again by the next read or peek. Previously peeked data can be
@defterm{committed}, which causes the data to be removed from the port
as for a read in a way that can be synchronized with other attempts to
peek or read through a @tech(~doc: ref_doc){synchronizable event}. Both
read and peek operations are normally blocking, in the sense that the
read or peek operation does not complete until data is available from
the port; non-blocking variants of read and peek operations are also
available via @rhombus(Port.Input.read_bytes_to) or
@rhombus(Port.Input.write_bytes) with mode @rhombus(Port.WaitMode.none).

The value @rhombus(Port.eof) is used to represent an end-of-file, and it
is the only value that satisfies the annotation
@rhombus(Port.EOF, ~annot). Reading from a port produces an end-of-file
result when the port has no more data, but some ports may also return
end-of-file mid-stream. For example, a port connected to a Unix terminal
returns an end-of-file when the user types control-D; if the user
provides more input, the port returns additional bytes after the
end-of-file.

Every port has a name, as reported by @rhombus(Port.name). The name can
be any value, and it is used mostly for error-reporting purposes. The
read-syntax procedure uses the name of an input port as the default
source location for the syntax objects that it produces.

A port can be used as a @tech(~doc: ref_doc){synchronizable event}. An
input port is ready for synchronization when
@rhombus(Port.Input.read_byte) would not block, and an output port is
ready for synchronization when @rhombus(Port.Output.write_byte) would
not block or when the port contains buffered characters and
@rhombus(Port.Output.write_bytes) in @rhombus(Port.WaitMode.some) mode
can flush part of the buffer (although @rhombus(Port.Output.write_bytes)
might block in that mode). A value that can act as both an input port
and an output port acts as an input port when used as a synchronizable
event. The synchronization result of a port is the port itself.
