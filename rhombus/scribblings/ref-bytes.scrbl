#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Byte Strings}

A @deftech{byte string} is a sequence of bytes (i.e., integers between 0
and 255 inclusive). A byte string works with map-referencing @brackets
to access a byte via @rhombus(#%index). A byte string also works with the
@rhombus(++) operator to append bytes strings. A byte string can be used
as @tech{sequence}, in which case it supplies its bytes in order.

A byte string is normally mutable, but byte-string literals are
immutable. The @rhombus(Bytes, ~annot) annotation is satisfied by both
mutable and immutable byte strings, while @rhombus(MutableBytes, ~annot)
and @rhombus(ImmutableBytes, ~annot) require one or the other.

@dispatch_table(
  "byte string"
  Bytes
  bstr.length()
  bstr.get(n)
  bstr.set(n, byte)
  bstr.append(bstr2, ...)
  bstr.subbytes(arg, ...)
  bstr.copy()
  bstr.copy_from(arg, ...)
  bstr.utf8_string(arg, ...)
  bstr.latin1_string(arg, ...)
  bstr.locale_string(arg, ...)
  bstr.to_sequence()
)

Byte strings are @tech{comparable}, which means that generic operations
like @rhombus(<) and @rhombus(>) work on byte strings.

@doc(
  annot.macro 'Bytes'
  annot.macro 'MutableBytes'
  annot.macro 'ImmutableBytes'
){

 Matches byte strings, where @rhombus(MutableBytes, ~annot) matches only
 mutable byte strings, and and @rhombus(ImmutableBytes, ~annot) matches
 only immutable byte strings.

 Static information associated by @rhombus(Bytes, ~annot), etc., makes
 an expression acceptable as a sequence to @rhombus(for) in static mode.

}


@doc(
  fun Bytes.make(length :: NonnegInt, byte :: Byte = 0)
    :: MutableBytes
){

 Creates a fresh byte string with @rhombus(length) bytes, where each
 byte is initialized to @rhombus(byte).

@examples(
  Bytes.make(5, 33)
)

}


@doc(
  fun Bytes.length(bstr :: Bytes) :: NonnegInt
){

 Returns the number of bytes in @rhombus(bstr).

@examples(
  Bytes.length(#"hello")
  #"hello".length()
)

}


@doc(
  fun Bytes.get(bstr :: Bytes, n :: NonnegInt) :: Byte
){

 Equivalent to @rhombus(bstr[n]) (with the default implicit
 @rhombus(#%index) form). Returns the @rhombus(n)th byte of
 @rhombus(bstr) (starting from @rhombus(0)).

@examples(
  #"abc"[0]
  #"abc".get(0)
)

}


@doc(
  fun Bytes.set(bstr :: MutableBytes,
                n :: NonnegInt, byte :: Byte)
    :: Void
){

 Equivalent to @rhombus(bstr[n] := byte) (with the default implicit
 @rhombus(#%index) form). Updates the @rhombus(n)th position of
 @rhombus(bstr) to @rhombus(byte).

@examples(
  def b = #"abc".copy()
  b[0] := 104
  b
  b.set(1, 104)
  b
)

}


@doc(
  fun Bytes.append(bstr :: Bytes, ...) :: MutableBytes
){

 Appends @rhombus(bstr)s by creating a new mutable byte string with
 all bytes.

@examples(
  #"abc".append(#"def", #"ghi")
)

}


@doc(
  fun Bytes.subbytes(bstr :: Bytes,
                     start :: NonnegInt,
                     end :: NonnegInt = Bytes.length(bstr))
    :: MutableBytes
){

 Returns the substring of @rhombus(bstr) from @rhombus(start) (inclusive)
 to @rhombus(end) (exclusive).

@examples(
  Bytes.subbytes(#"hello", 2, 4)
  Bytes.subbytes(#"hello", 2)
)

}

@doc(
  fun Bytes.copy(bstr :: Bytes) :: MutableBytes
){

 Returns a frash mutable byte string with the same initial content as
 @rhombus(bstr).

}

@doc(
  fun Bytes.copy_from(dest_bstr :: MutableBytes,
                      dest_start :: NonnegInt,
                      src_bstr :: Bytes,
                      src_start :: NonnegInt = 0,
                      src_end :: NonnegInt = Bytes.length(src_bstr))
    :: Void
){

 Copies bytes from @rhombus(src_bstr) at @rhombus(src_start) (inclusive) to
 @rhombus(src_end) (exclusive) into @rhombus(dest_bstr) starting at
 @rhombus(dest_start). The length of @rhombus(dest_bstr) must be at least
 @rhombus(dest_start + (src_end - src_start)).

}

@doc(
  fun Bytes.utf8_string(bstr :: Bytes,
                        err_char :: maybe(Char) = #false,
                        start :: NonnegInt = 0,
                        end :: NonnegInt = Bytes.length(bstr))
    :: String
  fun Bytes.latin1_string(bstr :: Bytes,
                          err_char :: maybe(Char) = #false,
                          start :: NonnegInt = 0,
                          end :: NonnegInt = Bytes.length(bstr))
    :: String
  fun Bytes.locale_string(bstr :: Bytes,
                          err_char :: maybe(Char) = #false,
                          start :: NonnegInt = 0,
                          end :: NonnegInt = Bytes.length(bstr))
    :: String
){

 Converts a byte string to a string, decoding as UTF-8, Latin-1, or the
 current locale's encoding. The @rhombus(err_char) argument provides a
 @tech{character} to use in place of an encoding error, where
 @rhombus(#false) means that an exception is thrown.

@examples(
  #"hello".utf8_string()
)

}

@doc(
  fun Bytes.to_sequence(bstr :: Bytes) :: Sequence
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(bstr)'s bytes in order.

}
