#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Output Ports}

An @deftech{output port} is a @tech{port} specifically for output, and
an @deftech{output string port} writes to a @tech{byte string}.

@dispatch_table(
  "output port"
  Port.Output
  out.flush()
  out.print(arg, ...)
  out.println(arg, ...)
  out.show(arg, ...)
  out.showln(arg, ...)
)

@dispatch_table(
  "output string port"
  Port.Output
  out.get_bytes()
  out.get_string()
)

@doc(
  annot.macro 'Port.Output'
  annot.macro 'Port.Output.String'
){

 The @rhombus(Port.Output, ~annot) annotation recognizes @tech{output
  ports}, and the @rhombus(Port.Output.String, ~annot) annotation
 recognizes @tech{output string ports}.

}

@doc(
  Parameter.def Port.Output.current :: Port.Output
){

 A @tech{context parameter} for the default port to use when printing.

}


@doc(
  Parameter.def Port.Output.current_error :: Port.Output
){

 A @tech{context parameter} for the default port to use when printing
 errors.

}

@doc(
  fun Port.Output.print(out :: Port.Output,
                        v :: Any, ...,
                        ~mode: mode :: PrintMode = #'text)
    :: Void
  fun Port.Output.println(out :: Port.Output,
                          v :: Any, ...,
                          ~mode: mode :: PrintMode = #'text)
    :: Void
  fun Port.Output.show(out :: Port.Output,
                       v :: Any, ...,
                       ~mode: mode :: PrintMode = #'text)
    :: Void
  fun Port.Output.showln(out :: Port.Output,
                         v :: Any, ...,
                         ~mode: mode :: PrintMode = #'text)
    :: Void
){

 The same as @rhombus(print), @rhombus(println), @rhombus(show), and
 @rhombus(showln), but with an output provided as a required initial
 argument instead of an optional @rhombus(~out) keyword argument.

}

@doc(
  fun Port.Output.open_file(path :: PathString,
                            ~exists: exists_flag
                                       :: Port.Output.ExistsFlag = #'error)
    :: Port.Output
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
 intention together with @rhombus(Port.Output.get_string).

}

@doc(
  fun Port.Output.get_bytes(out :: Port.Output.String)
    :: Bytes
  fun Port.Output.get_string(out :: Port.Output.String)
    :: String
){

 @rhombus(Port.Output.get_bytes) returns the bytes accumulated in the
 @tech{output string port} @rhombus(out) so far in a freshly allocated
 @tech{byte string} (including any bytes written after the port's
 current position, if any).

 @rhombus(Port.Output.get_string) is like
 @rhombus(Port.Output.get_bytes), but returns a string converted from
 the byte string instead.

}

@doc(
  fun Port.Output.flush(out :: Port.Output = Port.Output.current())
    :: Void
){

 Flushes the content of @rhombus(out)'s buffer.

}

@doc(
  enum Port.Output.ExistsFlag:
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
