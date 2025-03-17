#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "port"){Input and Output Ports}

A @deftech{port} represents a source or destination for reading and
writing data. The names @rhombus(stdin), @rhombus(stdout), and
@rhombus(stderr) access the current input source, output destination,
and error-output destination, and they are sometimes used implicitly.
For example, @rhombus(print) takes an output port as an optional
@rhombus(~out) argument, and it uses @rhombus(stdout) when
@rhombus(~out) is not supplied.

@margin_note_block{Examples show output written to @rhombus(stderr) in
 red italics.}

@examples(
  print("hello", ~out: stdout)
  print("hello", ~out: stderr)
)

Ports are objects that satisfy @rhombus(Port, ~annot) and either
@rhombus(Port.Input, ~annot) or @rhombus(Port.Output, ~annot), and they
support associated methods. The @rhombus(Port.Output.print) method can
be used on @rhombus(stdout) and @rhombus(stderr), for example, instead
of calling the @rhombus(print) function.

@examples(
  stdout.print("hello")
  stderr.print("hello")
)

All ports ultimately read or write bytes, and so the most primitive
methods work in terms of @deftech{byte strings}. A byte string is
written like a string, but with a @litchar{#} prefix. As a convenience,
ports also provide methods for reading and writing strings using an
automatic UTF-8 conversion.

The @rhombus(Port.Input.open_file) and @rhombus(Port.Output.open_file)
functions create port objects to read and write to a file.

@examples(
  ~repl:
    def tmp = filesystem.make_temporary()
  ~repl:
    def outp = Port.Output.open_file(tmp.path, ~exists: #'truncate)
    outp.write_bytes(#"data")
    outp.close()
  ~repl:
    def inp = Port.Input.open_file(tmp.path)
    inp.read_bytes(4)
    inp.close()
  ~repl:
    :
      tmp.close() // deletes temporary file
)

In a non-interactive context, using @rhombus(Closeable.let) is normally
a better choice than directly calling @rhombus(Port.Output.close) and
similar methods. We return to that form in @secref("closeable").

A port can be used for @rhombus(stdout) or @rhombus(stdin) by using
@rhombus(parameterize) with @rhombus(Port.Output.current) or
@rhombus(Port.Input.current).

@examples(
  ~repl:
    def tmp = filesystem.make_temporary()
  ~repl:
    def outp = Port.Output.open_file(tmp.path, ~exists: #'truncate)
    parameterize { Port.Output.current: outp }:
      println("data")
    outp.close()
  ~repl:
    def inp = Port.Input.open_file(tmp.path)
    parameterize { Port.Input.current: inp }:
      stdin.read_line()
    inp.close()
  ~repl:
    tmp.close()
)

Finally, since the combination of opening, setting @rhombus(stdin) or
@rhombus(stdout), and then close is so common, it is supported directly
by @rhombus(Port.Output.using) and @rhombus(Port.Intput.using).

@examples(
  ~repl:
    def tmp = filesystem.make_temporary()
  ~repl:
    Port.Output.using ~file tmp.path:
      ~exists: #'truncate
      println("data")
  ~repl:
    Port.Input.using ~file tmp.path:
      stdin.read_line()
  ~repl:
    tmp.close()
)

The @rhombus(Port.Output.open_string) function creates an output port
that accumulates a string, so it can be used to build up large strings
where appending would be inconvenient or inefficient. The
@rhombus(Port.Output.String.get_string) method extracts an accumulated
string from the port. The @rhombus(Port.Output.open_string) function
similar enables reading from a string. String ports can be closed, but
since they do not allocate constrained system resources, leaving them
completely to garbage collection is fine.

@examples(
  ~repl:
    def outp = Port.Output.open_string()
    outp.print("hello")
    outp.print(" ")
    outp.print("world")
    outp.get_string()
  ~repl:
    def inp = Port.Input.open_string("λ") // UTF-8 is encoding is #"\316\273"
    inp.read_byte()
    inp.read_byte()
)
