#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "unix-path"){Unix and Mac OS Paths}

A path on Unix or Mac OS is natively a byte string. For presentation to
users and for other string-based operations, a path is converted to/from
a string using the current locale's encoding with @litchar{?} (encoding)
or @rhombus(Char"\uFFFD") (decoding) in place of errors. Beware that the
encoding may not accommodate all possible paths as distinct strings.

A @litchar{/} separates elements of a Unix or Mac OS path, @litchar{.}
as a path element always means the directory indicated by preceding
path, and @litchar{..} as a path element always means the parent of the
directory indicated by the preceding path. A leading @litchar{~} in a
path is not treated specially, but @rhombus(filesystem.expand_user_path)
can be used to convert a leading @litchar{~} element to a user-specific
directory. No other character or byte has a special meaning within a
path. Multiple adjacent @litchar{/} are equivalent to a single
@litchar{/} (i.e., they act as a single path separator).

A path root is always @litchar{/}. A path starting with @litchar{/} is
an absolute, complete path, and a path starting with any other character
is a relative path.

Any pathname that ends with a @litchar{/} syntactically refers to a
directory, as does any path whose last element is @litchar{.} or
@litchar{..}.

A Unix or Mac OS path is @tech{cleanse}d by replacing multiple
adjacent @litchar{/}s with a single @litchar{/}.

For the @rhombus(Path.Element) constructor, the given byte string must
not contain any @litchar{/}, otherwise the
@rhombus(Exn.Fail.Annot, ~annot) exception is thrown. The result of
@rhombus(Path.Element.bytes) or @rhombus(Path.Element.string) is always
the same as the result of @rhombus(Path.bytes) and
@rhombus(Path.string). Since that is not the case for other platforms,
however, @rhombus(Path.Element.bytes) and @rhombus(Path.Element.string)
should be used when converting individual path elements.
