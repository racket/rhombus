#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "pipe"){Pipes}

A Rhombus @deftech{pipe} is internal to Rhombus (and Racket), and not
related to OS-level pipes for communicating between different processes.

@doc(
  annot.macro 'Port.Pipe'
){

 Regonizes pipes created with @rhombus(Port.Pipe.make).

}


@doc(
  fun Port.Pipe.make(
    ~limit: maybe(PosInt),
    ~input_name: input_name :: Any = #'pipe,
    ~input_name: output_name :: Any = #'pipe
  ) :: values(Port.Input && Port.Pipe,
              Port.Output && Port.Pipe)
){

 Returns two port values: the first port is an input port and the second
 is an output port. Data written to the output port is read from the
 input port, with no intermediate buffering. Unlike some other kinds of
 ports, pipe ports do not need to be explicitly closed to be reclaimed by
 garbage collection.

 If limit is @rhombus(#false), the new pipe holds an unlimited number of
 unread bytes (i.e., limited only by the available memory). If
 @rhombus(limit) is a positive number, then the pipe will hold at most
 @rhombus(limit) unread/unpeeked bytes; writing to the pipe's output port
 thereafter will block until a read or peek from the input port makes
 more space available. (Peeks effectively extend the port's capacity
 until the peeked bytes are read.)

 The optional @rhombus(input_name) and @rhombus(output)name) are used as
 the names for the returned input and output ports, respectively.

}

@doc(
  method (port :: Port.Pipe).content_length()
    :: NonnegInt
){

 Returns the number of bytes contained in a pipe, where @rhombus(port) is
 either of the pipe's ports produced by @rhombus(Port.Pipe.make). The pipe's content
 length counts all bytes that have been written to the pipe and not yet
 read (though possibly peeked).

}
