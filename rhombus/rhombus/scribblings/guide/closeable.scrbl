#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "closeable"){Closeable Objects}

The @rhombus(Closeable, ~class) interface has a single method,
@rhombus(Closeable.close). A @rhombus(Closeable, ~class) object can be
used with the @rhombus(Closeable.let) form, which automatically calls
@rhombus(Closeable.close) at the point where the binding goes out of
scope. @rhombus(Port, ~annot) objects implement
@rhombus(Closeable, ~class) as does the @rhombus(TemporaryFile, ~annot)
result of @rhombus(filesystem.make_temporary).

The first file example of @secref("port") can be written with
@rhombus(Closeable.let) like this, where the ports are automatically
closed and the temporary file is automatically deleted:

@examples(
    block:
      Closeable.let tmp = filesystem.make_temporary()
      block:
        Closeable.let outp = Port.Output.open_file(tmp.path, ~exists: #'truncate)
        outp.write_bytes(#"data")
      block:
        Closeable.let inp = Port.Input.open_file(tmp.path)
        inp.read_bytes(4)
)

The @rhombus(Closeable.let) form is unusual in that it is a
@deftech{definition sequence} macro, which receives all forms that
follow the definition in a block context. The @rhombus(Closeable.let)
form moves those forms into a @rhombus(try) close and adds a call to
@rhombus(Closeable.let) in a @rhombus(~finally) clause, so the close
operation happens even if an error occurs the body after
@rhombus(Closeable.let).

Beware that the right-hand side of a @rhombus(Closeable.let) definition
is evaluated with asynchronous breaks disabled. Disabling breaks
reliably handles the case that, say, a user hits Ctl-C in a terminal
running a Rhombus program. In that case, either the
@rhombus(Closeable, ~annot) is has not opened, or it is opened and will
be reliably closed when the exception triggered by Ctl-C is thrown.
