#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/subprocess open)

@title(~tag: "subprocess"){Subprocesses}

@docmodule(rhombus/subprocess)

@doc(
  class Subprocess():
    implements Closeable
){

 Represents a subprocess created with functions like @rhombus(run)
 and @rhombus(run_shell).

}

@doc(
  fun run(
    exe :: PathString,
    ~in: in :: Port.Input || Subprocess.Pipe = Port.Input.current(),
    ~out: out :: Port.Output || Subprocess.Pipe = Port.Output.current(),
    ~err: err :: Port.Output || Subprocess.ErrorPipe = Port.Output.current(),
    ~group: group :: Subprocess.Group || Subprocess.NewGroup
              = (if current_subprocess_group_new() | #'new | #'same),
    arg :: PathString || ReadableString,
    ...
  ) :: Subprocess
){

 Runs another program as a subprocess in the host operating system. The
 executable to run in the process is named by @rhombus(exe), and it
 receives the @rhombus(arg)s.

 The new process's input, output, or error output can be captured in a
 pipe using @rhombus(#'pipe) for @rhombus(in), @rhombus(out), or
 @rhombus(err), respectively; the other end of the pipe is then
 accessible using @rhombus(Subprocess.in),
 @rhombus(Subprocess.out), or @rhombus(Subprocess.err),
 respectively. Otherwise, the new process's input and output use the
 given ports, which default to the current ports. In the case of
 @rhombus(err), @rhombus(#'out) is allowed to indicate that error output
 should use the same pipe or port as non-error output.

 If @rhombus(group) is @rhombus(#'same), the process group of the
 current process is used for the new subprocess. If @rhombus(group) is
 @rhombus(#'new), the new subprocess is created in a fresh process group;
 the resulting @rhombus(Subprocess, ~annot) object then satisfies
 @rhombus(Subprocess.NewGroup, ~annot). If @rhombus(group) is a
 @rhombus(Subprocess.NewGroup, ~annot), then the new subprocess is in the
 same group as that previously created subprocess.

 When pipes are created for a subprocess, the local end of the pipe must
 be closed explicitly, perhaps using @rhombus(Subprocess.close). See also
 @rhombus(Closeable.let, ~defn).

}


@doc(
  fun run_shell(
    command :: String,
    ~in: in :: Port.Input || Subprocess.Pipe = Port.Input.current(),
    ~out: out :: Port.Output || Subprocess.Pipe = Port.Output.current(),
    ~err: err :: Port.Output || Subprocess.ErrorPipe = Port.Output.current(),
    ~group: group :: Subprocess.Group || Subprocess.NewGroup
              = (if current_subprocess_group_new() | #'new | #'same)
  ) :: Subprocess
  fun shell(command :: String) :: Boolean
){

 The @rhombus(run_shell) function is like @rhombus(run), but it runs a
 single shell @rhombus(command) provided as a string, instead of running
 an executable with a list of individual arguments.

 On Unix and Mac OS variants, @rhombus("/bin/sh") is run as the shell.
 On Windows, @rhombus("cmd.com") or @rhombus("command.exe") is used.

 The @rhombus(shell) function is a shorthand to combine @rhombus(run_shell)
 and @rhombus(Subprocess.wait_ok) as @rhombus(run_shell(command).wait_ok()).

}

@doc(
  property (subp :: Subprocess).in :: Port.Output
  property (subp :: Subprocess).out :: Port.Input
  property (subp :: Subprocess).err :: Port.Input
  property (subp :: Subprocess).maybe_in :: maybe(Port.Output)
  property (subp :: Subprocess).maybe_out :: maybe(Port.Input)
  property (subp :: Subprocess).maybe_err :: maybe(Port.Input)
){

 Accesses pipe ends created for subprocess.

 Accessing the @rhombus(Subprocess.in), @rhombus(Subprocess.out), or
 @rhombus(Subprocess.err), property raises an exception if the subprocess
 does not have a pipe for the corresponding subprocess stream. Accessing
 the @rhombus(Subprocess.maybe_in), @rhombus(Subprocess.maybe_out), or
 @rhombus(Subprocess.maybe_err) property either produces the same result
 as @rhombus(Subprocess.in), @rhombus(Subprocess.out), or
 @rhombus(Subprocess.err), or it returns @rhombus(#false).

}


@doc(
  property (subp :: Subprocess).close() :: Void
){

 Closes any pipes created for the subprocess that are still open.

}


@doc(
  property (subp :: Subprocess).pid :: NonnegInt
){

 Returns the operating system's process ID for a subprocess.

}


@doc(
  method (subp :: Subprocess).wait() :: Int
  method (subp :: Subprocess).wait_ok() :: Boolean
  method (subp :: Subprocess).poll() :: maybe(Int)
){

 Waits for a subprocess to complete or checks whether it has completed.

 The @rhombus(Subprocess.wait) method waits for the subprocess and it
 returns its exit code. An exit code of @rhombus(0) typically indicates
 success, and @rhombus(Subprocess.wait_ok(subp)) is equivalent to
 @rhombus(Subprocess.wait(subp) == 0).

 The @rhombus(Subprocess.poll) method immediately returns
 @rhombus(#false) if the subprocess has not completed, or it returns the
 same result as @rhombus(Subprocess.wait).

}


@doc(
  method (subp :: Subprocess).interrupt() :: Void
  method (subp :: Subprocess).kill() :: Void
){

 Send a process an interrupt signal or a kill signal, respectively. The
 latter cannot be ignored by a process.

}

@doc(
  Parameter.def current_subprocess_group_new
    :: Any.to_boolean
){

 A @tech{context parameter} that determines the default group for a
 subprocess. See @rhombus(Subprocess), @rhombus(run), and other
 subprocess-creation functions.

}


@doc(
  Parameter.def current_subprocess_custodian_mode
    :: False || matching(#'kill) || matching(#'interrupt)
){

 A @tech{context parameter} that determines whether and how subprocesses
 are managed by a custodian.

}

@doc(
  enum Subprocess.Pipe:
    pipe
  enum Subprocess.ErrorPipe:
    ~is_a Subprocess.Pipe
    out
  enum Subprocess.Group:
    same
    new
  annot.macro 'Subprocess.NewGroup'
){

 Port and group modes for use with @rhombus(Subprocess, ~class), @rhombus(run),
 @rhombus(shell), and @rhombus(run_shell).

}
