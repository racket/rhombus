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
  path.extend(part, ...)
  path.is_absolute()
  path.parts()
  path.read_with(proc)
  path.string()
  path.write_with(proc, ...)
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
  fun Path.extend(path :: Path,
                  part :: PathString | #'up | #'same, ...) :: Path
){

  Creates a path given a base path and any number of sub-path
  extensions. See also @rhombus(++).  If @rhombus(path) is an absolute path,
  the result is an absolute path, otherwise the result is a relative path.

  The @rhombus(path) and each @rhombus(part) must be either a relative
  path, the symbol @rhombus(#'up) (indicating the relative parent
  directory), or the symbol @rhombus(#'same) (indicating the
  relative current directory).  For Windows paths, if @rhombus(path) is a
  drive specification (with or without a trailing slash) the first
  @rhombus(part) can be an absolute (driveless) path. For all platforms,
  the last @rhombus(part) can be a filename.

  The @rhombus(path) and @rhombus(part) arguments can be paths for
  any platform. The platform for the resulting path is inferred from the
  @rhombus(path) and @rhombus(part) arguments, where string arguments imply
  a path for the current platform. If different arguments are for
  different platforms, the @rhombus(Exn.Fail.Contract, ~class) exception
  is thrown.  If no argument implies a platform (i.e., all are @rhombus(#'up)
  or @rhombus(#'same)), the generated path is for the current platform.

  Each @rhombus(part) and @rhombus(path) can optionally end in a directory
  separator. If the last @rhombus(part) ends in a separator, it is
  included in the resulting path.

  The @rhombus(build-path) procedure builds a path @italic{without}
  checking the validity of the path or accessing the filesystem.

@examples(
  def p = Path("/home/rhombus")
  Path.extend(p, "shape.txt")
  p.extend("shape.txt")
  p ++ "shape.txt"
)

}

@doc(fun Path.is_absolute(path :: Path)){

 Returns @rhombus(#true) if @rhombus(path) is an absolute path, @rhombus(#false)
 otherwise.  This procedure does not access the filesystem.
}

@doc(
  fun Path.parts(path :: Path) :: List.of(Path || #'up || #'same)
){

  Returns a list of path elements that constitute @rhombus(path).

  The @rhombus(Path.parts) function computes its result in time
  proportional to the length of @rhombus(path).

@examples(
  def p = Path("/home/rhombus/shape.txt")
  Path.parts(p)
  p.parts()
)

}

@doc(
  fun Path.read_with(path :: Path, read_proc :: Function.of_arity(1))
){

 Opens @rhombus(path) for reading and calls @rhombus(read_proc) with the
 @tech{input port}.  The result of @rhombus(read_proc) is the result of
 @rhombus(Path.read_with).

}

@doc(
  fun Path.write_with(path :: Path,
                      proc :: Function.of_arity(1),
                      ~exists: exists_flag
                                 :: Port.Output.ExistsFlag = #'error)
){

 Opens @rhombus(path) for writing and calls @rhombus(write_proc) with the
 @tech{output port}.  The result of @rhombus(write_proc) is the result of
 @rhombus(Path.write_with).

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
