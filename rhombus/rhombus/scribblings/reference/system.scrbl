#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "system"){System Information}

@doc(
  fun system.version() :: String
){

 Reports the host Racket version.

@examples(
  system.version()
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