#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "filesystem"){Filesystem}

@doc(
  fun filesystem.file_exists(path :: PathString) :: Boolean
  fun filesystem.directory_exists(path :: PathString) :: Boolean
  fun filesystem.link_exists(path :: PathString) :: Boolean
){

 Reports whether @rhombus(path) refers to an existing file, directory,
 or soft link, respectively. The @rhombus(filesystem.link_exists) result
 is not mutually exclusive with the other two: when @rhombus(path) refers
 to a soft link, @rhombus(filesystem.file_exists) and
 @rhombus(filesystem.directory_exists) report whether the link reaches a
 file or directory.

}


@doc(
  fun filesystem.simplify_path(path :: PathString) :: Path
  fun filesystem.normalize_path(path :: PathString) :: Path
){

 The @rhombus(filesystem.simplify_path) is similar to
 @rhombus(Path.simplify), but @rhombus(filesystem.simplify_path)
 simplifies based on the filesystem, while @rhombus(Path.simplify) is a
 purely syntactic simplification. In particular, when a prefix of
 @rhombus(path) refers to a link, the link is resolved before a
 subsequent directory-navigation element is simplified, which can produce
 a different reference than a syntactic simplication.

 The @rhombus(filesystem.normalize_path) function is equivalent to
 @rhombus(filesystem.simplify_path(Path.to_absolute_path(path))). Note
 that normalization @emph{does not} use @rhombus(Path.normal_case).

}


@doc(
  fun filesystem.resolve_path(path :: PathString) :: Path
){

 Uses @rhombus(Path.cleanse) on @rhombus(path) and then returns a path
 that references the same file or directory as path. If @rhombus(path)
 refers to a soft link to another path, then the referenced path is
 returned (which may be a relative path with respect to the directory
 parent of @rhombus(path)), otherwise the cleansed @rhombus(path) is
 returned.

 On Windows, the path for a link should be simplified syntactically, so
 that an up-directory indicator removes a preceding path element
 independent of whether the preceding element itself refers to a link.
 Beware that relative-paths links require further care.

}


@doc(
  fun filesystem.expand_user_path(path :: PathString) :: Path
){

 Uses @rhombus(Path.cleanse) on @rhombus(path) and then, on platforms
 that use the @rhombus(#'unix) path convention, checks for a leading
 @litchar{~} and replaces the path element starting from @litchar{~} with
 the path to the specified user's home directory. A
 @rhombus(Exn.Fail.Filesystem, ~annot) exception is thrown if the home
 directory cannot be found.

}

@doc(
  fun filesystem.list_directory(path :: PathString
                                  = Path.current_directory(),
                                ~extend_path: extend = #false) :: List.of(Path)
){

  Returns a list of all files and directories in the directory specified
  by @rhombus(path).  If @rhombus(extend) is @rhombus(#true) the resulting
  paths are extended from @rhombus(path).

}