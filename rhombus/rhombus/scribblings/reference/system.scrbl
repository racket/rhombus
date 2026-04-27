#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/measure
      rhombus/bytes)

@title(~tag: "system"){System Information and Control}

@doc(
  fun system.version() :: String
){

 Reports the running Rhombus version.

 See @rhombusmodname(rhombus/version) for version-number conversion and
 comparison utilities.

@examples(
  system.version()
)

}

@doc(
  fun system.racket_version() :: String
){

 Reports the host Racket version.

 See @rhombusmodname(rhombus/version) for version-number conversion and
 comparison utilities.

@examples(
  system.racket_version()
)

}

@doc(
  fun system.type() :: Symbol
){

 Report the host platform's general type: @rhombus(#'unix),
 @rhombus(#'windows), or @rhombus(#'macosx).

}

@doc(
  fun system.os() :: Symbol
){

 Report the host platform's operating system more specifically than
 @rhombus(system.type). For example, the result may be @rhombus(#'linux)
 or @rhombus(#'freebsd) instead of @rhombus(#'unix).

}

@doc(
  fun system.arch() :: Symbol
){

 Report the host platform's architecture. Possible results include
 @rhombus(#'x86_64), @rhombus(#'i386), @rhombus(#'aarch64),
 @rhombus(#'arm) (32-bit), and @rhombus(#'ppc) (32-bit).

}

@doc(
  fun system.word() :: Int
){

 Report the word size in bits for the host platform's architecture.
 Possible results currently are @rhombus(32) and @rhombus(64).

}

@doc(
  fun system.host() :: String
){

 Reports details of the host operating system in a platform-specific
 format.

}

@doc(
  fun system.target_machine() :: system.TargetMachineSymbol
  annot.macro 'system.TargetMachineSymbol'
){

 Reports the native compilation target machine for the host platform.
 Possible results include @rhombus(#'ta6le), @rhombus(#'tarm64osx), and
 @rhombus(#'ti3nt).

 The @rhombus(system.TargetMachineSymbol, ~annot) annotation is
 satisfied by a machine type symbol that are supported as a compilation
 target by the host Racket system, which may include a cross-compilation
 target machine.

}

@doc(
  fun system.path(which :: Symbol) :: Path
){

 Returns a system-specific path categorized by @rhombus(which):

@itemlist(

 @item{@rhombus(#'temp_dir): a path to a directory for storing temporary
  files.}

)

}

@doc(
  fun system.so_suffix() :: Bytes
){

 Reports a file suffix conventionally used by the host operating system
 for @defterm{shared objects}, also known as @defterm{dynamically linked
  libraries}. Possible results include @rhombus(#".so"), @rhombus(#".dll"),
 and @rhombus(#".dylib").

}

@doc(
  fun system.locale_string_encoding() :: String
){

 A string for the current locale's encoding. This string is suitable for
 use as an encoding name with @rhombus(bytes.Converter).

}

@doc(
  fun system.language_and_country() :: String
){

 A string to identify the current user’s language and country.

 On Unix and Mac OS, the string is five characters: two lowercase ASCII
 letters for the language, an underscore, and two uppercase ASCII letters
 for the country. On Windows, the string can be arbitrarily long, but the
 language and country are in English (all ASCII letters or spaces)
 separated by an underscore.

 On Unix, the result is determined by checking the @tt{LC_ALL},
 @tt{LC_TYPE}, and @tt{LANG} environment variables, in that order (and
 the result is used if the environment variable’s value starts with two
 lowercase ASCII letters, an underscore, and two uppercase ASCII letters,
 followed by either nothing or a period). On Windows and Mac OS, the
 result is determined by system calls.

}

@doc(
  fun system.seconds() :: Int
  fun system.milliseconds() :: Flonum
){

 The @rhombus(system.seconds) reports the current time in seconds since
 @deftech{the epoch}, January 1, 1970 UTC, which is consistent with the
 @rhombus(filesystem.modify_seconds) function's result, for example. The
 @rhombus(system.milliseconds) function reports the same time with more
 precision, expressed in milliseconds (including fractional milliseconds)
 since the epoch.

 See also @rhombus(measure.cpu_milliseconds) and
 @rhombus(measure.real_milliseconds).

}


@doc(
  fun system.exit(v :: Any = #true) :: Void
){

 Calls the system exit handler, which normally exits the current
 process. If @rhombus(v) is a byte, then the byte is used as the
 process's exit code, otherwise @rhombus(0) is used as the exit code.

}
