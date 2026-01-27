#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "cross-path"){Cross-Platform Paths}

A @deftech{cross-platform path} value represents a filesystem path
combined with @rhombus(#'unix) or @rhombus(#'windows) to indicate the
filesystem's path convention. A cross-platform path whose convention
matches the current host system is also a @tech{path}. The @rhombus(.)
operator can be used on a cross-platform path expression in the same way
as path expressions.

Cross-platform paths are @tech{comparable} the same as paths, which
means that generic operations like @rhombus(<) and @rhombus(>) work on
paths, cross-platform paths, and combinations. Paths with the
@rhombus(#'unix) convention are ordered before paths with the
@rhombus(#'windows) convention.

@doc(
  enum CrossPath.Convention
  | unix
  | windows
  annot.macro 'CrossPath':
    ~method_fallback: Path
  annot.macro 'CrossPath.Absolute':
    ~method_fallback: CrossPath
  annot.macro 'CrossPath.Relative':
    ~method_fallback: CrossPath
  annot.macro 'CrossPath.DriveRelative':
    ~method_fallback: CrossPath
  annot.macro 'CrossPath.Directory':
    ~method_fallback: CrossPath
  annot.macro 'CrossPath.Element':
    ~method_fallback: CrossPath
  annot.macro 'CrossPath.Unix':
    ~method_fallback: CrossPath
  annot.macro 'CrossPath.Windows':
    ~method_fallback: CrossPath
){

 The @rhombus(CrossPath.Convention, ~annot) enumeration represents the
 two supported path conventions. Other annotations are analogous to
 @rhombus(Path, ~annot), @rhombus(Path.Absolute, ~annot),
 @rhombus(Path.Relative, ~annot), @rhombus(Path.DriveRelative, ~annot),
 @rhombus(Path.Directory, ~annot), @rhombus(Path.Element, ~annot). The
 @rhombus(CrossPath.Unix, ~annot) and @rhombus(CrossPath.Windows, ~annot)
 annotations are satisfied by cross-platform paths with those respective
 conventions.

 Every @rhombus(Path, ~annot) is also a @rhombus(CrossPath, ~annot), and
 @rhombus(Path.convention) for a @rhombus(Path, ~annot) will produce the
 same symbol as @rhombus(CrossPath.Convention.current()). Operations on
 @rhombus(CrossPath, ~annot)s also work on @rhombus(Path, ~annot), but
 they typically do not accept strings, instead requiring
 @rhombus(Path, ~annot) or @rhombus(CrossPath, ~annot) values that have
 an associated convention.

}


@doc(
  fun CrossPath(
    cross_path :: Bytes || Path.Dot || CrossPath,
    convention :: CrossPath.Convention = CrossPath.Convention.current()
  ) :: CrossPath
  fun CrossPath.Unix(cross_path :: Bytes || Path.Dot) :: CrossPath
  fun CrossPath.Windows(cross_path :: Bytes || Path.Dot) :: CrossPath
){

 The @rhombus(CrossPath) function constructs a @tech{cross-platform
  path} given a byte string and path convention. When a cross-platform
 path is provided as @rhombus(cross_path), then the result is
 @rhombus(cross_path).

 The @rhombus(CrossPath.Unix) and @rhombus(CrossPath.Windows) functions
 are shorthands for @rhombus(CrossPath) with @rhombus(#'unix) and
 @rhombus(#'windows), respectively.

@examples(
  def p = CrossPath(#"/home/rhombus/shape.txt", #'unix)
  p.string()
  p.convention()
)

}

@doc(
  ~nonterminal:
    bytes_bind: def bind ~defn
    convention_bind: def bind ~defn
  bind.macro 'CrossPath($bytes_bind, $convention_bind)'
){

 Matches a cross-platform path where the byte-string form of the path
 matches @rhombus(bytes_bind) and the convention matches
 @rhombus(convention_bind).

@examples(
  def CrossPath(bstr, _) = Path("/home/rhombus/shape.txt")
  bstr
)

}


@doc(
  fun CrossPath.Convention.current() :: CrossPath.Convention
){

 Reports the convention of the current platform, either @rhombus(#'unix)
 or @rhombus(#'windows).

}
