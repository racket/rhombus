#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{File/Stream Ports}

@doc(
  annot.macro 'Port.FileStream':
    ~method_fallback: Port
  annot.macro 'Port.FileStream.Terminal':
    ~method_fallback: Port
){

 The @rhombus(Port.FileStream, ~annot) annotation recognizes
 @tech{ports} that correspond to operating-system resources such as files
 and pipes for interprocess communication. The
 @rhombus(Port.FileStream.Terminal, ~annot) annotation recognizes ports that
 represent to interactive terminals.

}


@doc(
   method (port :: Port.FileStream).truncate(
     size :: Nat
   ) :: Void
){

 When @rhombus(port) represents a file,
 @rhombus(Port.FileStream.truncate) sets the file's size. The
 @rhombus(Port.position) function can be used to make a file larger,
 but @rhombus(Port.FileStream.truncate) can make a file smaller.

}


@doc(
   method (port :: Port.FileStream).try_lock(
     mode :: Port.FileStream.LockMode
   ) :: Boolean
   method (port :: Port.FileStream).unlock() :: Void
){

 The @rhombus(Port.FileStream.try_lock) function attempts to acquire a
 filesystem lock on the file that @rhombus(port) represents using the
 operating systems's facilities for file locking. The result is
 @rhombus(#true) if the lock acquisition succeeds, @rhombus(#false)
 otherwise.

 The @rhombus(mode) argument can be @rhombus(#'shared) or
 @rhombus(#'exclusive). Multiple processes can acquire a
 @rhombus(#'shared) lock on a file, but at most one process can hold an
 @rhombus(#'exclusive) lock, and @rhombus(#'shared) and
 @rhombus(#'exclusive) locks are mutually exclusive. When mode is
 @rhombus(#'shared), then port must be an input port; when mode is
 @rhombus(#'exclusive), then port must be an output port.

 The result is @rhombus(#true) if the requested lock is acquired,
 @rhombus(#false) otherwise. When a lock is acquired, it is held until
 either it is released with @rhombus(Port.FileStream.unlock) or the port
 is closed (perhaps because the process terminates).

 Depending on the platform, locks may be merely advisory (i.e., locks
 affect only the ability of processes to acquire locks) or they may
 correspond to mandatory locks that prevent reads and writes to the
 locked file. Specifically, locks are mandatory on Windows and advisory
 on other platforms. Multiple tries for a @rhombus(#'shared) lock on a
 single port can succeed; on Unix and Mac OS, a single
 @rhombus(Port.FileStream.unlock) releases the lock, while on other
 Windows, a @rhombus(Port.FileStream.unlock) is needed for each
 successful @rhombus(Port.FileStream.try_lock). On Unix and Mac OS,
 multiple tries for a @rhombus(#'exclusive) lock can succeed and a single
 @rhombus(Port.FileStream.unlock) releases the lock, while on Windows, a
 try for an @rhombus(#'exclusive) lock fails for a given port if the port
 already holds the lock.

 A lock acquired for an input port from
 @rhombus(Port.open_input_output_file) can be released through
 @rhombus(Port.FileStream.unlock) on the corresponding output port, and
 vice versa. If the output port from
 @rhombus(Port.open_input_output_file) holds an @rhombus(#'exclusive)
 lock, the corresponding input port can still acquire a
 @rhombus(#'shared) lock, even multiple times; on Windows, a
 @rhombus(Port.open_input_output_file) is needed for each successful lock
 try, while a single @rhombus(Port.open_input_output_file) balances the
 lock tries on Unix and Mac OS. A @rhombus(#'shared) lock on an input
 port can be upgraded to an @rhombus(#'exclusive) lock through the
 corresponding output port on Unix and Mac OS, in which case a single
 @rhombus(Port.open_input_output_file) (on either port) releases the
 lock, while such upgrades are not allowed on Windows.

 Locking is normally supported only for file ports, and attempting to
 acquire a lock with other kinds of file-stream ports throws an
 @rhombus(Exn.Fail.Filesystem) exception.

}

@doc(
   method (port :: Port.FileStream).identity() :: PosInt
   method (port :: Port.FileStream).stat() :: Map
){

 The @rhombus(Port.FileStream.identity) method returns an integer that
 represents a file's identity on the filesystem. When multiple ports read
 and write to the same file, they have he same identity. A port that
 refers to the same as a path will have the same identity as
 @rhombus(filesystem.identity) produces for the path.

 The @rhombus(Port.FileStream.stat) function similarly corresponds to
 @rhombus(filesystem.stat).

}


@doc(
   method (port :: Port.FileStream).is_waiting_on_peer()
     :: Boolean
){

 Returns @rhombus(#true) if @rhombus(port) is not ready for reading or
 writing because it is waiting for a peer process to complete a stream
 construction, @rhombus(#false) otherwise.

 On Unix and Mac OS, opening a fifo for output creates a peer-waiting
 port if no reader for the same fifo is already opened. In that case, the
 output port is not ready for writing until a reader is opened; that is,
 write operations will block. Use @rhombus(sync) if necessary to wait
 until writing will not block---that is, until the read end of the fifo
 is opened.

}


@doc(
  enum Port.FileStream.LockMode
  | shared
  | exclusive
){

 Lock modes for @rhombus(Port.FileStream.try_lock).

}
