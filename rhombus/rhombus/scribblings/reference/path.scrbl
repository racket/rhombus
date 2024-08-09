#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Paths}

A @deftech{path} value represents a filesystem path.

@dispatch_table(
  "path"
  Path
  path.bytes()
  path.string()
)

Paths are @tech{comparable}, which means that generic operations like
@rhombus(<) and @rhombus(>) work on paths.

@doc(
  annot.macro 'Path'
  annot.macro 'PathString'
  annot.macro 'PathString.to_path'
){

 Matches a path value.  The @rhombus(PathString, ~annot) annotation allows
 @rhombus(ReadableString, ~annot) as well as @rhombus(Path, ~annot) values.
 The @rhombus(PathString.to_path, ~annot)
 @tech(~doc: guide_doc){converter annotation} allows
 @rhombus(PathString, ~annot) values, but converts
 @rhombus(ReadableString, ~annot) values to @rhombus(Path) values.
}

@doc(
  fun Path(path :: Bytes || ReadableString || Path) :: Path
){

 Constructs a path given a byte string, string, or existing path. When a
 path is provided as @rhombus(path), then the result is @rhombus(path).

@examples(
  def p = Path("/home/rhombus/shape.txt")
  p
  Path(p)
  p.string()
)

}

@doc(
  bind.macro 'Path($bind)'
){

 Matches a path where the byte-string form of the path matches
 @rhombus(bind).

@examples(
  def Path(p) = Path("/home/rhombus/shape.txt")
  p
)

}

@doc(
  fun Path.bytes(path :: Path) :: Bytes
){

 Converts a path to a byte-string form, which does not lose any
 information about the path.

@examples(
  def p = Path("/home/rhombus/shape.txt")
  Path.bytes(p)
  p.bytes()
)

}

@doc(
  fun Path.string(path :: Path) :: String
){

 Converts a path to a human-readable form, but the conversion may lose
 information if the path cannot be expressed using a string (e.g., due to
 a byte-string form that is not a UTF-8 encoding).

@examples(
  def p = Path(#"/home/rhombus/shape.txt")
  Path.string(p)
  p.string()
)

}

@// ------------------------------------------------------------

@include_section("runtime-path.scrbl")
