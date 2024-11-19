#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "filesystem"){Filesystem}

@doc(
  fun filesystem.type(path :: PathString,
                      ~must_exist: must_exist = #false)
    :: matching(#'file || #'directory || #'link || #'directory_link || #false)
){

 Reports whether @rhombus(path) refers to an existing file, directory,
 or soft link, returning a corresponding symbol or @rhombus(#false) if
 @rhombus(path) does not exist. If @rhombus(must_exist) is true, then
 @rhombus(#false) is never returned, and an exception is thrown, instead.

 The result can be @rhombus(#'directory_link) only on Windows, which
 distinguishes file and directory links.

}

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
    fun filesystem.files(
      path :: PathString = Path.current_directory(),
      ~add_path: add_path = #false,
      ~recur: recur = #false,
      ~follow_links: follow_links = #false,
      ~keep: keep :: Path -> Any = fun (x): #true,
      ~skip: skip :: Path -> Any = fun (x): #false
    ) :: List.of(Path)
){

 Returns a list of paths within the directory referenced by
 @rhombus(path). Each path in the result list is relative to
 @rhombus(path), unless @rhombus(add_path) is true, in which case
 @rhombus(Path.add) is used with @rhombus(path) and each result path in
 the list.

 If @rhombus(recur) is @rhombus(#false), then the result includes only
 files, directories, and links immediately within @rhombus(path). If
 @rhombus(recur) is true, then each directory within @rhombus(path) is
 followed by its own recursive content in the result list. If
 @rhombus(follow_links) true, the a recursive traversal continues into a
 link that refers to a directory, otherwise the link is not traversed for
 the result list; link cycles can cause @rhombus(filesystem.files) to
 never return.

 The @rhombus(keep) and @rhombus(skip) functions are applied to every
 path encountered during a traversal, and a path is omitted from the list
 when @rhombus(keep) returns @rhombus(#false) or @rhombus(skip) returns a
 true value for the path. When @rhombus(recur) is true, skipping a
 directory (or link that refers to a directory) means that it is not
 traversed, so the directory's content is also skipped. If @rhombus(keep)
 returns @rhombus(#false) for a path, then @rhombus(skip) is not applied
 to the path.

}

@doc(
  fun filesystem.roots() :: List.of(Path.Absolute)
){

 Returns a list of all roots on the current filesystem. For a filesystem
 that uses Unix path conventions, the result is always
 @rhombus([Path("/")]), but the result list can contain multiple drive
 paths on Windows.

}

@doc(
  fun filesystem.rename(path :: PathString,
                        to_path :: PathString,
                        ~exists_ok: exists_ok = #false)
    :: Void
){

 Renames the file or directory @rhombus(path) to @rhombus(to_path). If
 @rhombus(exists_ok) is true, then renaming can replace an existing file.

 When @rhombus(exists_ok) is @rhombus(#false), a check is needed before
 the move operation internally, and that combination is not atomic on
 Unix and Mac OS. If @rhombus(to_path) exists and is replaced (when
 @rhombus(exists_ok) is true), then the replacement is atomic on Unix and
 Mac OS, but not necessarily atomic on Windows.

}


@doc(
  fun filesystem.delete(
    path :: PathString,
    ~as: mode :: matching(#'any || #'file || #'directory) = #'any,
    ~recur: recur = #false,
    ~must_exist: must_exist = #true
  ) :: Void
){

 Deletes the file, directory, or link referenced by @rhombus(path). If
 @rhombus(mode) is @rhombus(#'file), then @rhombus(path) must refer to a
 file or link, and if @rhombus(mode) is @rhombus(#'directory), then
 @rhombus(path) must refer to a directory.

 If @rhombus(recur) is true and if @rhombus(path) refers to a directory,
 then each file and directory within @rhombus(path) is recursively
 deleted before attempting to delete @rhombus(path) itself. When a link
 that refers to a directory is deleted, then the directory content is not
 deleted through the link, even when @rhombus(recur) is true.

 If @rhombus(must_exist) is @rhombus(#true) and @rhombus(path) does not
 refer to a file, directory, or link, then an exception is thrown.

 When @rhombus(mode) is @rhombus(#'any) or @rhombus(must_exist) is
 @rhombus(#false), the check for @rhombus(path)'s type/existence is not
 atomic combined with the deletion of @rhombus(path), so an exception can
 be thrown if a file, directory or link disappears or changes between the
 check and deletion.

 On Windows, a file or link is deleted by first renaming it to a file in
 the temporary directory reported by @rhombus(system.path(#'temp_dir)),
 and then deleting the file there. That two-step process can succeed in
 situations when a file might otherwise be locked by an asynchronous
 background process. If moving the file does not succeed, it is instead
 deleted in-place. See also
 @rhombus(filesystem.current_force_delete_permissions).

}


@doc(
  fun filesystem.make_directory(
    path :: PathString,
    ~parents: parents = #false,
    ~permissions: permissions :: Int.in(0, 65535) = 0o777
  ) :: Void
){

 Creates a directory @rhombus(path). If @rhombus(parents) is true, then
 any non-existent parent of @rhombus(path) is created, first. If
 @rhombus(parents) is @rhombus(#false) and @rhombus(path) exists already,
 then an exception is thrown.

 The @rhombus(permissions) argument specifies permissions used for a new
 created directory on Unix and Mac OS. The given @rhombus(permissions)
 are adjusted based on the current process's umask. The
 @rhombus(permissions) argument is ignored on Windows.

}


@doc(
  fun filesystem.make_link(to_path :: PathString,
                           path :: PathString)
    :: Void
){

 Creates @rhombus(path) as a soft link whose content is the byte-string
 form of @rhombus(path). An exception is thrown if @rhombus(path) exists
 already.

}


@doc(
  fun filesystem.make_temporary(
    ~in: dir :: PathString = system.path(#'temp_dir),
    ~as: mode :: (PathString || matching(#'file) || matching(#'directory))
           = #'file,
    ~make_name: make_name :: String -> Path.Relative
                  = fun (s): Path("tmp" +& s),
    ~permissions: permissions :: maybe(Int.in(0, 65535))
                    = #false,
    ~replace_permissions: replace_permissions = #false
  ) :: filesystem.Temporary
  class filesystem.Temporary(path :: Path,
                             is_directory :: Any.to_boolean):
    implements Closeable
){

 Creates a temporary file or directory within @rhombus(dir) and returns
 a @rhombus(filesystem.Temporary, ~class), which contains a path to the
 created file. If @rhombus(mode) is a path or
 @rhombus(#'file), the result contains a file path, otherwise it contains a directory
 path. If @rhombus(mode) is a path, the temporary file is created as a copy
 of @rhombus(mode).

 A @rhombus(filesystem.Temporary, ~class) object is
 @rhombus(Closeable, ~class), which means that it can be used with
 @rhombus(Closeable.let). The @rhombus(Closeable.close) implementation
 for @rhombus(filesystem.Temporary, ~class) deletes the temporary file or
 directory, recursively deleting content in the case of a temporary
 directory.

 The name of the temporary file is based on the current time and the
 @rhombus(make_name) function. The argument to @rhombus(make_name) is a
 string containing only ASCII digits. The path returns by
 @rhombus(make_name) is added to @rhombus(dir), and if a file, directory,
 or link with that name exists already,
 @rhombus(filesystem.make_temporary) starts again to try a new file name.

 The @rhombus(permissions) and @rhombus(replace_permissions) arguments
 apply to the newly created file. When @rhombus(mode) as a path, the
 permissions argument as used as in @rhombus(filesystem.copy). Otherwise,
 the permissions arguments are used as in @rhombus(Port.Output.open_file)
 or @rhombus(filesystem.make_directory).

@examples(
  ~defn:
    fun process_big_data(src :: Path):
      Closeable.let tmp = filesystem.make_temporary()
      setup(src, tmp.path)
      work(tmp.path)
      finish(tmp.path) // produces result
      // `tmp.path` is deleted on return or on exception
)

}


@doc(
  fun filesystem.copy(
    src_path :: PathString,
    dest_path :: PathString,
    ~recur: recur = #false,
    ~exists_ok: exists_ok = #false,
    ~follow_links: follow_links = #true,
    ~permissions: permissions :: maybe(Int.in(0, 65535)) = #false,
    ~replace_permissions: replace_permissions = #true,
    ~keep_modify_seconds: keep_modify_seconds = #false
  ) :: Void
){

 Copies the file or link @rhombus(src_path) to @rhombus(dest_path), or
 recursively copies a directory when @rhombus(recur) is true.

 If @rhombus(exists_ok) is false, an exception is thrown if
 @rhombus(dest_path) exists already. When @rhombus(exists_ok) is true,
 @rhombus(recur) is true, and a directory is copied, then the content of
 @rhombus(src_path) is effectively spliced with any existing directory
 tree @rhombus(dest_path).

 When @rhombus(follow_links) is @rhombus(#false), then a link is copied
 to a new link. Otherwise, the target of the link is copied to the new
 path.

 The @rhombus(permissions) argument is used for newly created files and
 directories. When it is @rhombus(#false), then a new file or directory
 uses the permissions of the copied files or directory, otherwise
 @rhombus(permissions) is used for both copied files and new directories.
 On Windows, if @rhombus(permissions) is supplied as
 non-@rhombus(#false), then after copying, @rhombus(dest_path) is set to
 read-only or not depending on whether the @rhombus(0o2) bit is present
 in @rhombus(permissions).

 The @rhombus(replace_permissions) argument is used only on Unix and Mac
 OS. When a @rhombus(dest_path) is created, it is created with
 @rhombus(permissions) or the permissions of @rhombus(src_path); however,
 the process's umask may unset bits in the requested permissions. When
 @rhombus(dest_path) already exists (and @rhombus(exists_ok) is true),
 then the permissions of @rhombus(dest_path) are initially left as-is.
 Finally, when @rhombus(replace_permissions) is a true value, then the
 permissions of @rhombus(dest_path) are set after the file content is
 copied (using again @rhombus(permissions) or the permissions of
 @rhombus(src_path)) without modification by the process's umask.

 On Windows, the modification time of @rhombus(src_path) is transferred
 to @rhombus(dest_path). When @rhombus(keep_modify_seconds) is true, then
 on Unix and Mac OS, too, the modification time of @rhombus(src_path) is
 transferred to @rhombus(dest_path) after copying.

}

@doc(
  fun filesystem.size(path :: PathString) :: NonnegInt
){

 Returns the size of the file referenced by @rhombus(path). An exception
 is thrown if @rhombus(path) does not refer to an existing file.

}


@doc(
  fun filesystem.modify_seconds(
    path :: PathString,
    ~set_to: set_to :: maybe(Int) = #false,
    ~must_exist: must_exist = #true
  ) :: maybe(Int) || Void
){

 Returns or changes (when @rhombus(set_to) is not @rhombus(#false))
 the modification timestamp of @rhombus(path).

 If @rhombus(path) cannot be accessed or the modification timestamp
 cannot be changed, an exception is thrown only if @rhombus(must_exist)
 is @rhombus(#false). Otherwise, @rhombus(#false) is reported for an
 inaccessible timestamp and failure is silently ignored (and
 @rhombus(#void) is still returned) when setting a timestamp fails.

}

@doc(
  fun filesystem.permissions(
    path :: PathString,
    ~bits: bits = #false,
    ~set_to: set_to :: maybe(Int.in(0, 65535)) = #false
  ) :: Int || Set.of(matching(#'read || #'write || #'execute)) || Void
){

 Returns or changes (when @rhombus(set_to) is not @rhombus(#false))
 the permissions of @rhombus(path).

 When @rhombus(set_to) is provided, the result is @rhombus(#void).
 Otherwise, the result is an integer when @rhombus(bits) is true, or it
 is a list of distinct symbols when @rhombus(bits) is false. A
 symbol-list result summarizes permissions for the current process's user
 and group.

}

@doc(
  fun filesystem.identity(
    path :: PathString,
    ~follow_links: follow_links = #true
  ) :: PosInt
){

 Returns an integer representing the identity of the file or directory
 referenced by @rhombus(path). This function can be used to check whether
 two paths correspond to the same filesystem object.

 When @rhombus(path) refers to a link, the result is the identity of the
 link when @rhombus(follow_links) is @rhombus(#false), otherwise it is
 the identity of the link target.

}


@doc(
  fun filesystem.stat(path :: PathString,
                      ~follow_links: follow_links = #true)
    :: Map
){

 Returns a symbol-keyed map containing information about @rhombus(path).
 The map includes information available from @rhombus(filesystem.type),
 @rhombus(filesystem.modify_seconds), @rhombus(filesystem.permissions),
 @rhombus(filesystem.identity), and other functions, but potentially in a
 more detailed form. This information is relatively low-level, and the
 result map uses Racket-style symbols as keys.

 If @rhombus(path) refers to a link, then the result map describes the
 target of the link unless @rhombus(follow_links) is @rhombus(false).

}


@doc(
  fun filesystem.read_bytes(
    path :: PathString,
    ~mode: mode :: Port.Mode = #'binary
  ) :: Bytes
  fun filesystem.read_string(
    path :: PathString,
    ~mode: mode :: Port.Mode = #'binary
  ) :: String
){

 Returns the content of the file referenced by @rhombus(path), either in
 byte form or string form. The latter corresponds to a UTF-8 decoding of
 the file's bytes.

}

@doc(
  fun filesystem.read_bytes_lines(
    path :: PathString,
    ~mode: mode :: Port.ReadLineMode = #'any
  ) :: List.of(String)
  fun filesystem.read_lines(
    path :: PathString,
    ~mode: mode :: Port.ReadLineMode = #'any
  ) :: List.of(String)
){

 Returns the content of the file referenced by @rhombus(path) as a list
 of byte-string or string lines. The latter corresponds to a UTF-8
 decoding of the file's bytes. The @rhombus(mode)
 argument is the same as for @rhombus(Port.Input.open_file).

}


@doc(
  fun filesystem.write_bytes(
    path :: PathString,
    bstr :: Bytes,
    ~exists: exists :: Port.Output.ExistsMode = #'error,
    ~mode: mode :: Port.Mode = #'binary
  ) :: Void
  fun filesystem.write_string(
    path :: PathString,
    str :: String,
    ~exists: exists :: Port.Output.ExistsMode = #'error,
    ~mode: mode :: Port.Mode = #'binary
  ) :: Void
){

 Replaces the content of @rhombus(path) with @rhombus(bstr) or the UTF-8
 encoding of @rhombus(str). The @rhombus(mode) and @rhombus(exists)
 arguments are the same as for @rhombus(Port.Output.open_file).

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
 a different reference than a syntactic simplification.

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
  Parameter.def \
  filesystem.current_force_delete_permissions
    :: Any.to_boolean
){

 When @rhombus(filesystem.current_force_delete_permissions) has a true
 value (the default), then when attempting to delete a file on Windows,
 the files is made writable before attempting to delete it. This mode
 more closely imitates the behavior of file deletion on Unix.

}
