#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Paths}

@dispatch_table(
  "path"
  @rhombus(Path)
  [path.bytes(), Path.bytes(path)]
  [path.string(), Path.string(path)]
)

@doc(
  annot.macro 'Path'
){

 Matches a path value.
}

@doc(
  fun Path(p :: Bytes || String || Path) :: Path
){

 Constructs a path given a byte string, string, or existing path. When a
 path is provided as @rhombus(p), then the result is @rhombus(p).

@examples(
  def p = Path("/home/rhombus/shape.txt")
  p
  Path(p)
  p.string()
)

}

@doc(
  bind.macro 'Path($binding)'
){

 Matches a path where the byte-string form of the path matches
 @rhombus(binding).

@examples(
  def Path(p): Path("/home/rhombus/shape.txt")
  p
)

}

@doc(
  fun Path.bytes(p :: Path) :: Bytes
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
  fun Path.string(p :: Path) :: String
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
