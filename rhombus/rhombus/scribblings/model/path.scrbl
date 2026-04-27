#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "path", ~style: #'toc){Filesystem Paths}

When a Rhombus operation takes a filesystem path as an argument, the
path can be provided either as a string or as an instance of the
@deftech{path} datatype. That choice is reflected by the
@rhombus(PathString, ~annot) annotation. If a string is provided, it is
converted to a path using the @rhombus(Path) constructor. Beware that
some paths may not be representable as strings; see
@secref("unix-path") and @secref("windows-path") for more
information. A Rhombus operation that generates a filesystem path always
generates a @tech{path} value.

By default, paths are created and manipulated for the current platform,
but procedures that merely manipulate paths (without using the
filesystem) can manipulate @tech(~doc: ref_doc){cross-platform paths}
using conventions for other supported
platforms. The @rhombus(CrossPath) constructor accepts a convention
argument that indicates the platform for the path, either
@rhombus(CrossPath.Convention.unix) or
@rhombus(CrossPath.Convention.windows). For other operations, such as
@rhombus(+/), the behavior is sensitive to the kind of path that is
supplied. Unless otherwise specified, an operation that requires a path
accepts only paths for the current platform.

Two @tech{path} values are @rhombus(==) when they use the same
convention type and when their byte-string representations are
@rhombus(==). A path string (or byte string) cannot be empty, and it
cannot contain a null character or byte. An empty string or a string
containing null does not satisfy @rhombus(PathString, ~annot).

Most Rhombus operations that accept paths first @deftech{cleanse} the
path before using it. Procedures that build paths or merely check the
form of a path do not cleanse paths, with the exceptions of
@rhombus(Path.cleanse), @rhombus(Path.simplify),
@rhombus(filesystem.expand_user_path), and
@rhombus(filesystem.simplify_path). For more information about path
cleansing and other platform-specific details, see @secref("unix-path")
and @secref("windows-path").

@local_table_of_contents()

@include_section("unix-path.scrbl")
@include_section("windows-path.scrbl")
