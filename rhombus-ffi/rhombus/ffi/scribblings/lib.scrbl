#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/custodian open
      lib("setup/dirs.rkt").#{get-lib-search-dirs})

@title(~tag: "ffi-lib"){Foreign Libraries}

@doc(
  class Lib()
){

 Represents a foreign library. Create a @rhombus(Lib) instance with
 @rhombus(Lib.load), and access exports of a loaded library using
 @rhombus(Lib.find).

}

@doc(
  fun Lib.load(
    path :: PathString || False,
    version :: String || List.of(String || False) || False = #false,
    ~get_lib_dirs: get_lib_dirs :: () -> (Listable.to_list && List.of(Path))
                     = #{get-lib-search-dirs},
    ~fail: fail :: maybe(() -> maybe(Lib)) = #false,
    ~as_global: as_global :: Any.to_boolean = #false,
    ~custodian: cust :: maybe(Custodian || Any.of(#'place)) = #false
  ) :: maybe(Lib)
){

 If @rhombus(path) is @rhombus(#false), then the resulting
 foreign-library value represents all libraries loaded in the current
 process, including libraries previously opened with @rhombus(Lib.load).
 The @rhombus(version) argument is ignored when @rhombus(path) is
 @rhombus(#false).

 Otherwise, @rhombus(path) and @rhombus(version) will be combined as
 follows:

@itemlist(

 @item{@rhombus(path): A path string that typically does not have a
  version or suffix (i.e., without @filepath{.dll}, @filepath{.so}, or
  @filepath{.dylib}).}

 @item{@rhombus(version): A list, usually, of versions to try in order
  with @rhombus(#false) (i.e., no version) as the last element of the
  list; for example, @rhombus(["2", #false]) indicates version 2 with a
  fallback to a versionless library. A string or @rhombus(#false)
  @rhombus(version) is equivalent to a list containing just the string or
  @rhombus(#false), and an empty string (by itself or in a list) is
  equivalent to @rhombus(#false).

  When the library suffix as reported by @rhombus(system.so_suffix) is
  @filepath{.dylib}, then a version is added to @rhombus(path) after a
  @filepath{.} and before the @filepath{.dylib} suffix. When the library
  suffix is @filepath{.dll}, then a version is added to @rhombus(path)
  after a @filepath{-} and before the @filepath{.dll} suffix. For any
  other suffix, the version number is added after the suffix plus
  @filepath{.}.

  Beware of relying on versionless library names. On some platforms,
  versionless library names are provided only by development packages. At
  the same time, other platforms may require a versionless fallback. A
  list of version strings followed by @rhombus(#false) is typically best
  for @rhombus(version).}

)

 Assuming that @rhombus(path) is not @rhombus(#false), the result from
 @rhombus(Lib.load) represents the library found by the following search
 process:

@itemlist(

 @item{If @rhombus(path) is not an absolute path, look in each directory
  reported by @rhombus(get_lib_dirs); the default list is the result of
  @rhombus(#{get-lib-search-dirs}()). In each directory, try
  @rhombus(path) with the first version in @rhombus(version), adding a
  suitable suffix if @rhombus(path) does not already end in the suffix,
  then try the second version in @rhombus(version), etc. (If
  @rhombus(version) is an empty list, no paths are tried in this step.)}

 @item{Try the same filenames again, but without converting the path to
  an absolute path, which allows the operating system to use its own
  search paths. (If @rhombus(version) is an empty list, no paths are tried
  in this step.)}

 @item{Try @rhombus(path) without adding any version or suffix, and
  without converting to an absolute path.}

 @item{Try the version-adjusted filenames again, but relative to the
  current directory. (If @rhombus(version) is an empty list, no paths are
  tried in this step.)}

 @item{Try @rhombus(path) without adding any version or suffix, but
  converted to an absolute path relative to the current directory.}

)

 If none of the paths succeed and @rhombus(fail) is a function, then
 @rhombus(fail) is called to get a result for @rhombus(Lib.load). If
 @rhombus(fail) is @rhombus(#false), an exception is thrown as if trying
 just the first path from the second bullet above or (if
 @rhombus(version) is an empty list) from the third bullet above. A
 library file may exist but fail to load for some reason; the eventual
 error message will unfortunately name the fallback from the second or
 third bullet, since some operating systems offer no way to determine why
 a given library path failed.

 If @rhombus(path) is not @rhombus(#false), @rhombus(as_global) is true,
 and the operating system supports opening a library in ``global'' mode
 so that the library's symbols are used for resolving references from
 libraries that are loaded later, then global mode is used to open the
 library. Otherwise, the library is opened in ``local'' mode, where the
 library's symbols are not made available for future resolution. This
 local-versus-global choice does not affect whether the library's symbols
 are available via @rhombus(Lib.load(#false)).

 If @rhombus(custodian) is a @rhombus(Custodian, ~annot), the library is
 unloaded when @rhombus(custodian) is shut down. When a library is
 unloaded, all references to the library become invalid. Supplying
 @rhombus(Custodian.current()) for @rhombus(custodian) tends to unload
 the library for eagerly, but requires even more care to ensure that
 library references are not accessed after the library is unloaded. If
 @rhombus(custodian) is @rhombus(#'place), it is equivalent to the main
 custodian of the current place, which is consistent with finalization
 via @rhombusmodname(ffi/finalize). If @rhombus(custodian) is
 @rhombus(#false), the loaded library is associated with Rhombus (or
 DrRacket) for the duration of the process; in that case, loading again
 with @rhombus(Lib.load), will not force a re-load of the corresponding
 library.

 When @rhombus(Lib.load) returns a reference to a library that was
 previously loaded within the current place, it increments a reference
 count on the loaded library rather than loading the library fresh.
 Unloading a library reference decrements the reference count and
 requests unloading at the operating-system level only if the reference
 count goes to zero.

 The @rhombus(Lib.load) function logs on the topic
 @rhombus(#'{ffi-lib}). In particular, on failure it logs the paths
 attempted according to the rules above, but it cannot report the paths
 tried due to the operating system's library search path.

}

@doc(
  method (lib :: Lib).find(
    name :: String || Bytes || Symbol,
    ~fail: fail :: maybe(() -> maybe(ptr_t)) = #false
  ) :: maybe(ptr_t)
){

 Looks for the byte-string form of @rhombus(name) as an export from
 @rhombus(lib). If @rhombus(name) is found in @rhombus(lib), its
 address is returned as a @tech{pointer} object.

 If @rhombus(name) is not found and @rhombus(fail) is a function, it is
 called to get the result for @rhombus(Lib.find). For example, a failure
 thunk can be provided to report a specific error if a name is not found:

@rhombusblock(
  def foo:
    Lib.find(foolib,
             "foo",
             fun ():
               error("installed foolib does not export \"foo\""))
)

 If @rhombus(name) is not found and @rhombus(fail) is @rhombus(#false),
 an exception is thrown.

}


@doc(
  property (lib :: Lib).handle :: Any
  fun Lib.from_handle(lib :: Any) :: Lib
){

 Converts to and from a representation of a foreign library that is
 compatible with Racket's libraries.

}
