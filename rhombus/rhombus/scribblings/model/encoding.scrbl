#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/shrubbery
      rhombus/subprocess
      rhombus/bytes)

@title(~tag: "encoding"){Encodings and Locales}

When a character-based operation is used on a port, such as
@rhombus(Port.Input.read_char) or @rhombus(shrubbery.read), the port's
bytes are read and interpreted as a UTF-8 encoding of characters. Thus,
reading a single character may require reading multiple bytes, and an
operation like @rhombus(Port.Input.peek_char) may need to peek several
bytes into the stream to determine whether a character is available. In
the case of a byte stream that does not correspond to a valid UTF-8
encoding, operations such as @rhombus(Port.Input.read_char) may need to
peek one byte ahead in the stream to discover that the stream is not a
valid encoding.

When an input port produces a sequence of bytes that is not a valid
UTF-8 encoding in a character-reading context, then bytes that
constitute an invalid sequence are converted to the character
@rhombus(Char"\uFFFD"). Specifically, bytes @rhombus(255) and
@rhombus(254) are always converted to @rhombus(Char"\uFFFD"), bytes in
the range @rhombus(192) to @rhombus(253) produce @rhombus(Char"\uFFFD")
when they are not followed by bytes that form a valid UTF-8 encoding,
and bytes in the range @rhombus(128) to @rhombus(191) are converted to
@rhombus(Char"\uFFFD") when they are not part of a valid encoding that
was started by a preceding byte in the range @rhombus(192) to
@rhombus(253). To put it another way, when reading a sequence of bytes
as characters, a minimal set of bytes are changed to the encoding of
@rhombus(Char"\uFFFD") so that the entire sequence of bytes is a valid
UTF-8 encoding.

See @secref(~doc: ref_doc, "converter") for facilities to work with
UTF-8 or other encodings. See also @rhombus(port.reencode_input) and
@rhombus(port.reencode_output) for obtaining a UTF-8-based port from one
that uses a different encoding of characters.

A @deftech{locale} captures information about a user's language-specific
interpretation of character sequences. In particular, a locale
determines how strings are ``alphabetized,'' how a lowercase character
is converted to an uppercase character, and how strings are compared
without regard to case. String operations using the
@rhombus(StringCI, ~annot) veneer are @italic{not} sensitive to the
current locale, but operations using the @rhombus(StringLocale, ~annot)
or @rhombus(StringLocaleCI, ~annot) veneer produce results consistent
with the current locale.

A locale also designates a particular encoding of code-point sequences
into byte sequences. Rhombus generally ignores this aspect of the
locale, with a few notable exceptions: command-line arguments passed to
Rhombus as byte strings are converted to character strings using the
locale's encoding; command-line strings passed as byte strings to other
processes (through @rhombus(subprocess.run)) are converted to byte
strings using the locale's encoding; environment variables are converted
to and from strings using the locale's encoding; filesystem paths are
converted to and from strings (for display purposes) using the locale's
encoding; and, finally, Rhombus provides operations such as
@rhombus(String.locale_bytes) and @rhombus(Bytes.locale_string) to
specifically invoke a locale-specific encoding.

A Unix user selects a locale by setting environment variables, such as
@tt{LC_ALL}. On Windows and Mac OS, the operating system provides other
mechanisms for setting the locale. Within Racket, the current locale can
be changed by setting the @rhombus(bytes.current_locale) parameter. The
locale name within Rhombus is a string, and the available locale names
depend on the platform and its configuration, but the @rhombus("")
locale means the current user's default locale; on Windows and Mac OS,
the encoding for @rhombus("") is always UTF-8, and locale-sensitive
operations use the operating system's native interface. (In particular,
setting the @tt{LC_ALL} and @tt{LC_CTYPE} environment variables does not
affect the locale @rhombus("") on Mac OS. Use @rhombus(envvars.getenv)
and @rhombus(bytes.current_locale) to explicitly install the
environment-specified locale, if desired.) Setting the current locale to
@rhombus(#false) makes locale-sensitive operations locale-insensitive,
which means using the Unicode mapping for case operations and using
UTF-8 for encoding.
