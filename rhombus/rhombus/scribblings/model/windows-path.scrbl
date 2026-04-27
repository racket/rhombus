#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/bnf
    rhombus/bytes)

@(def MzAdd  = @italic{Rhombus-specific:})

@(def nonterm  = bnf.nt)

@(def indexed_file = filepath)

@(fun litchar_([s :~ String]): litchar(s.replace("~", " ")))

@title(~tag: "windows-path"){Windows Paths}

A path on Windows is natively a sequence of UTF-16 code units, where
the sequence can include unpaired surrogates. This sequence is encoded
as a byte string through an extension of UTF-8, where unpaired
surrogates in the UTF-16 code-unit sequence are converted as if they
were non-surrogate values. The extended encodings are implemented on
Windows as the @rhombus("platform-UTF-16") and
@rhombus("platform-UTF-8") encodings for @rhombus(bytes.Converter).

Rhombus's internal representation of a Windows path is a byte string, so
that @rhombus(Path.bytes) and @rhombus(Path) on byte string are always
inverses. When converting a path to a native UTF-16 code-unit sequence,
@rhombus(Char"\t") is used in place of platform-UTF-8 decoding errors
(on the grounds that tab is normally disallowed as a character in a
Windows path, unlike @rhombus(Char"\uFFFD")).

A Windows path is converted to a string by treating the platform-UTF-8
encoding as a UTF-8 encoding with @rhombus(Char"\uFFFD") in place of
decoding errors. Similarly, a string is converted to a path by UTF-8
encoding (in which case no errors are possible).

In general, a Windows pathname consists of an optional drive specifier
and a drive-specific path. A Windows path can be
@defterm{drive-relative}; such paths start with a @litchar{/} or
@litchar{\} separator and are not UNC paths or paths that start with
@litchar{\\?\}.

A path that starts with a drive specification is @defterm{absolute}.
Roughly, a drive specification is either a Latin letter followed by a
colon, a UNC path of the form
@litchar{\\}@nonterm{machine}@litchar{\}@nonterm{volume}, or a
@litchar{\\?\} form followed by something other than
@litchar{REL\}@nonterm{element}, or
@litchar{RED\}@nonterm{element}. (Variants of @litchar{\\?\}
paths are described further below.)

Rhombus fails to implement the usual Windows path syntax in one way.
Outside of Rhombus, a pathname @filepath{C:rant.txt} can be a
drive-specific relative path. That is, it names a file
@filepath{rant.txt} on drive @filepath{C:}, but the complete path to the
file is determined by the current working directory for drive
@filepath{C:}. Rhombus does not support drive-specific working
directories (only a working directory across all drives, as reflected by
the @rhombus(Path.current_directory) parameter). Consequently,
Rhombus implicitly converts a path like @filepath{C:rant.txt} into
@filepath{C:\rant.txt}.

@itemlist(

 @item{@(MzAdd) Whenever a path starts with a drive specifier
 @nonterm{letter}@litchar{:} that is not followed by a @litchar{/} or
 @litchar{\}, a @litchar{\} is inserted as the path is @tech{cleanse}d.}

)

Otherwise, Rhombus follows standard Windows path conventions, but also
adds @litchar{\\?\REL} and @litchar{\\?\RED} conventions to deal with
paths inexpressible in the standard convention, plus conventions to deal
with excessive @litchar{\}s in @litchar{\\?\} paths.

In the following, @nonterm{letter} stands for a Latin letter (case
does not matter), @nonterm{machine} stands for any sequence of
characters that does not include @litchar{\} or @litchar{/} and is
not @litchar{?}, @nonterm{volume} stands for any sequence of
characters that does not include @litchar{\} or @litchar{/} , and
@nonterm{element} stands for any sequence of characters that does not
include @litchar{\}.

@itemlist(

 @item{Trailing spaces and @litchar{.} in a path element are ignored
 when the element is the last one in the path, unless the path starts
 with @litchar{\\?\} or the element consists of only spaces and
 @litchar{.}s.}

 @item{The following special ``files'', which access devices, exist in
 all directories, case-insensitively, and with all possible endings after
 a period or colon, except in pathnames that start with @litchar{\\?\}:
 @indexed_file{NUL}, @indexed_file{CON}, @indexed_file{PRN},
 @indexed_file{AUX}, @indexed_file{COM1}, @indexed_file{COM2},
 @indexed_file{COM3}, @indexed_file{COM4}, @indexed_file{COM5},
 @indexed_file{COM6}, @indexed_file{COM7}, @indexed_file{COM8},
 @indexed_file{COM9}, @indexed_file{LPT1}, @indexed_file{LPT2},
 @indexed_file{LPT3}, @indexed_file{LPT4}, @indexed_file{LPT5},
 @indexed_file{LPT6}, @indexed_file{LPT7}, @indexed_file{LPT8},
 @indexed_file{LPT9}.}

 @item{Except for @litchar{\\?\} paths, @litchar{/}s are equivalent to
 @litchar{\}s. Except for @litchar{\\?\} paths and the start of UNC
 paths, multiple adjacent @litchar{/}s and @litchar{\}s count as a single
 @litchar{\}. In a path that starts @litchar{\\?\} paths, elements can be
 separated by either a single or double @litchar{\}.}

 @item{A directory can be accessed with or without a trailing separator.
 In the case of a non-@litchar{\\?\} path, the trailing separator can be
 any number of @litchar{/}s and @litchar{\}s; in the case of a
 @litchar{\\?\} path, a trailing separator must be a single @litchar{\},
 except that two @litchar{\}s can follow
 @litchar{\\?\}@nonterm{letter}@litchar{:}.}

 @item{Except for @litchar{\\?\} paths, a single @litchar{.} as a path
 element means ``the current directory,'' and a @litchar{..} as a path
 element means ``the parent directory.'' Up-directory path elements
 (i.e., @litchar{..}) immediately after a drive are ignored.}

 @item{A pathname that starts
 @litchar{\\}@nonterm{machine}@litchar{\}@nonterm{volume} (where a
 @litchar{/} can replace any @litchar{\}) is a UNC path, and the starting
 @litchar{\\}@nonterm{machine}@litchar{\}@nonterm{volume} counts as the
 drive specifier.}

 @item{Normally, a path element cannot contain a character in the range
 @rhombus(Char"\x00") to @rhombus(Char"\x1F") nor any of the following
 characters:

  @centered{@litchar{<} @litchar{>} @litchar{:} @litchar{"}
            @litchar{/} @litchar{\} @litchar{|} @litchar{?} @litchar{*}}

 Except for @litchar{\}, path elements containing these characters can
 be accessed using a @litchar{\\?\} path (assuming that the underlying
 filesystem allows the characters).}

 @item{In a pathname that starts
 @litchar{\\?\}@nonterm{letter}@litchar{:\}, the
 @litchar{\\?\}@nonterm{letter}@litchar{:\} prefix counts as the path's
 drive, as long as the path does not both contain non-drive elements and
 end with two consecutive @litchar{\}s, and as long as the path contains
 no sequence of three or more @litchar{\}s. Two @litchar{\}s can appear
 in place of the @litchar{\} before @nonterm{letter}. A @litchar{/}
 cannot be used in place of a @litchar{\} (but @litchar{/}s can be used
 in element names, though the result typically does not name an actual
 directory or file).}

 @item{In a pathname that starts
 @litchar{\\?\UNC\}@nonterm{machine}@litchar{\}@nonterm{volume}, the
 @litchar{\\?\UNC\}@nonterm{machine}@litchar{\}@nonterm{volume} prefix
 counts as the path's drive, as long as the path does not end with two
 consecutive @litchar{\}s, and as long as the path contains no sequence
 of three or more @litchar{\}s. Two @litchar{\}s can appear in place of
 the @litchar{\} before @litchar{UNC}, the @litchar{\}s after
 @litchar{UNC}, and/or the @litchar{\}s after@nonterm{machine}. The
 letters in the @litchar{UNC} part can be uppercase or lowercase, and
 @litchar{/} cannot be used in place of @litchar{\}s (but @litchar{/} can
 be used in element names).}

 @item{@(MzAdd) A pathname that starts
 @litchar{\\?\REL\}@nonterm{element} or
 @litchar{\\?\REL\\}@nonterm{element} is a relative path, as long as the
 path does not end with two consecutive @litchar{\}s, and as long as the
 path contains no sequence of three or more @litchar{\}s. This
 Rhombus-specific path form supports relative paths with elements that
 are not normally expressible in Windows paths (e.g., a final element
 that ends in a space). The @as_indexed{@litchar{REL}} part must be exactly
 the three uppercase letters, and @litchar{/}s cannot be used in place of
 @litchar{\}s. If the path starts @litchar{\\?\REL\..} then for as long
 as the path continues with repetitions of @litchar{\..}, each element
 counts as an up-directory element; a single @litchar{\} must be used to
 separate the up-directory elements. As soon as a second @litchar{\} is
 used to separate the elements, or as soon as a non-@litchar{..} element
 is encountered, the remaining elements are all literals (never
 up-directory elements). When a @litchar{\\?\REL} path value is converted
 to a string (or when the path value is written or displayed), the string
 does not contain the starting @litchar{\\?\REL} or the immediately
 following @litchar{\}s; converting a path value to a byte string
 preserves the @litchar{\\?\REL} prefix.}

 @item{@(MzAdd) A pathname that starts
 @litchar{\\?\RED\}@nonterm{element} or
 @litchar{\\?\RED\\}@nonterm{element} is a drive-relative path, as long
 as the path does not end with two consecutive @litchar{\}s, and as long
 as the path contains no sequence of three or more @litchar{\}s. This
 Rhombus-specific path form supports drive-relative paths (i.e., absolute
 given a drive) with elements that are not normally expressible in
 Windows paths. The @as_indexed{@litchar{RED}} part must be exactly the
 three uppercase letters, and @litchar{/}s cannot be used in place of
 @litchar{\}s. Unlike @litchar{\\?\REL} paths, a @litchar{..} element is
 always a literal path element. When a @litchar{\\?\RED} path value is
 converted to a string (or when the path value is written or displayed),
 the string does not contain the starting @litchar{\\?\RED} and it
 contains a single starting @litchar{\}; converting a path value to a
 byte string preserves the @litchar{\\?\RED} prefix.}

)

Three additional Rhombus-specific rules provide meanings to character
sequences that are otherwise ill-formed as Windows paths:

@itemlist(

 @item{@(MzAdd) In a pathname of the form
 @litchar{\\?\}@nonterm{any}@litchar{\\} where @nonterm{any} is any
 non-empty sequence of characters other than @nonterm{letter}@litchar{:}
 or @litchar{\}@nonterm{letter}@litchar{:}, the entire path counts as the
 path's (non-existent) drive.}

 @item{@(MzAdd) In a pathname of the form
 @litchar{\\?\}@nonterm{any}@litchar{\\\}@nonterm{elements}, where
 @nonterm{any} is any non-empty sequence of characters and
 @nonterm{elements} is any sequence that does not start with a
 @litchar{\}, does not end with two @litchar{\}s, and does not contain a
 sequence of three @litchar{\}s, then
 @litchar{\\?\}@nonterm{any}@litchar{\\} counts as the path's
 (non-existent) drive.}

 @item{@(MzAdd) In a pathname that starts @litchar{\\?\} and does not
 match any of the patterns from the preceding bullets, @litchar{\\?\}
 counts as the path's (non-existent) drive.}

)

Outside of Rhombus, except for @litchar{\\?\} paths, pathnames are
typically limited to 259 characters when used as a file path and 247
characters when used as a directory path. Rhombus internally converts
pathnames longer than 247 characters to @litchar{\\?\} form to avoid the
limits; in that case, the path is first simplified syntactically (in the
sense of @rhombus(Path.simplify). The operating system cannot access
files through @litchar{\\?\} paths that are longer than 32,000
characters or so.

Where the above descriptions says ``character,'' substitute ``byte''
for interpreting byte strings as paths. The encoding of Windows paths
into bytes preserves ASCII characters, and all special characters
mentioned above are ASCII, so all of the rules are the same.

Beware that the @litchar{\} path separator is an escape character
in Rhombus strings. Thus, the path @litchar{\\?\REL\..\\..}  as
a string must be written @rhombus("\\\\?\\REL\\..\\\\..").

A path that ends with a directory separator syntactically refers to a
directory.  In addition, a path syntactically refers to a directory if
its last element is a same-directory or up-directory indicator (not
quoted by a @litchar{\\?\} form), or if it refers to a root.

Even on variants of Windows that support symbolic links, up-directory
@litchar{..} indicators in a path are resolved syntactically, not
sensitive to links. For example, if a path ends with @litchar{d\..\f}
and @litchar{d} refers to a symbolic link that references a directory
with a different parent than @litchar{d}, the path nevertheless refers
to @litchar{f} in the same directory as @litchar{d}. A relative-path
link is parsed as if prefixed with @litchar{\\?\REL} paths, except
that @litchar{..}  and @litchar{.} elements are allowed throughout the
path, and any number of redundant @litchar{\} separators are allowed.

Windows paths are @tech{cleanse}d as follows: In paths that start
@litchar{\\?\}, redundant @litchar{\}s are removed, an extra @litchar{\}
is added in a @litchar{\\?\REL} if an extra one is not already present
to separate up-directory indicators from literal path elements, and an
extra @litchar{\} is similarly added after @litchar{\\?\RED} if an extra
one is not already present. For other paths, multiple @litchar{/}s and
@litchar{\}s are converted to single @litchar{/}s or @litchar{\} (except
at the beginning of a shared folder name), and a @litchar{\} is inserted
after the colon in a drive specification if it is missing.

For the @rhombus(Path.Element) constructor, @litchar{/}s, colons,
trailing dots, trailing whitespace, and special device names (e.g.,
``aux'') in the argument byte string are encoded as a literal part of
the path element by using a @litchar{\\?\REL} prefix. The byte string
argument must not contain a @litchar{\}, otherwise a
@rhombus(Exn.Fail.Annot, ~annot) exception is thrown.

For @rhombus(Path.Element.Bytes) or @rhombus(Path.Element.String), if
the byte-string form of the path starts with a @litchar{\\?\REL}, the
prefix is not included in the result.

For @rhombus(Path.add) or @rhombus(+/), trailing spaces and periods are
removed from the last element of all but the last argument (unless the
element consists of only spaces and periods), except for those that
start with @litchar{\\?\}. If the first argument starts @litchar{\\?\},
then after each non-@litchar{\\?\REL\} and non-@litchar{\\?\RED\}
argument is added, all @litchar{/}s in the addition are converted to
@litchar{\}s, multiple consecutive @litchar{\}s are converted to a
single @litchar{\}, added @litchar{.} elements are removed, and added
@litchar{..} elements are removed along with the preceding element;
these conversions are not performed on the first argument's contribution
to the result or on any @litchar{\\?\REL\} or @litchar{\\?\RED\} or
argument. If a @litchar{\\?\REL\} or @litchar{\\?\RED\} path
is added to a non-@litchar{\\?\} initial path, the addition (with any
additions up to the @litchar{\\?\REL\} or @litchar{\\?\RED\} argument)
is simplified and converted to a @litchar{\\?\} path. In other cases, a
@litchar{\} may be added or removed before combining paths to avoid
changing the root meaning of the path (e.g., combining @litchar{//x} and
@litchar{y} produces @litchar{/x/y}, because @litchar{//x/y} would be a
UNC path instead of a drive-relative path).

For @rhombus(Path.simplify) and @rhombus(filesystem.simplify_path), the
path is expanded, and if it does not start with @litchar{\\?\}, trailing
spaces and periods are removed, a @litchar{/} is inserted after the
colon in a drive specification if it is missing, and a @litchar{\} is
inserted after @litchar{\\?\} as a root if there are elements and no
extra @litchar{\} already. Otherwise, if no indicators or redundant
separators are in the path, then the is returned as-is.

For @rhombus(Path.split), @rhombus(Path.parent), and
@rhombus(Path.name), splitting a path that does not start with
@litchar{\\?\} can produce parts that start with @litchar{\\?\}. For
example, splitting @litchar_{C:/x~/aux/} twice produces
@litchar_{\\?\REL\\x~} and @litchar{\\?\REL\\aux}; the @litchar{\\?\} is
needed in these cases to preserve a trailing space after @litchar{x} and
to avoid referring to the AUX device instead of an @filepath{aux} file.
