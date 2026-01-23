#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Byte Strings}

A @deftech{byte string} is a sequence of @tech{bytes}. A byte string
works with map-referencing @brackets to access a byte via
@rhombus(#%index). A byte string also works with the
@rhombus(++) operator to append bytes strings. A byte string can be used
as @tech{sequence}, in which case it supplies its bytes in order.

A byte string is normally mutable, but byte-string literals are
immutable. The @rhombus(Bytes, ~annot) annotation is satisfied by both
mutable and immutable byte strings, while @rhombus(MutableBytes, ~annot)
and @rhombus(ImmutableBytes, ~annot) require one or the other.

Two byte strings are equal by @rhombus(is_now) as long as they have
equal contents, even if one is mutable and the other is immutable.

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
  fun Bytes.make(length :: Nat, byte :: Byte = 0)
    :: MutableBytes
){

 Creates a fresh byte string with @rhombus(length) bytes, where each
 byte is initialized to @rhombus(byte).

@examples(
  Bytes.make(5, Byte#"!")
)

}


@doc(
  method (bstr :: Bytes).length() :: Nat
){

 Returns the number of bytes in @rhombus(bstr).

@examples(
  #"hello".length()
  Bytes.length(#"hello")
)

}


@doc(
  method (bstr :: Bytes).get(n :: Nat) :: Byte
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
  method (bstr :: Bytes).set(n :: Nat, byte :: Byte)
    :: Void
){

 Equivalent to @rhombus(bstr[n] := byte) (with the default implicit
 @rhombus(#%index) form). Updates the @rhombus(n)th position of
 @rhombus(bstr) to @rhombus(byte).

@examples(
  def b = #"abc".copy()
  b[0] := Byte#"h"
  b
  b.set(1, Byte#"h")
  b
)

}


@doc(
  method (bstr :: Bytes).append(bstr :: Bytes, ...) :: MutableBytes
  fun Bytes.append(bstr :: Bytes, ...) :: MutableBytes
){

 Appends @rhombus(bstr)s by creating a new mutable byte string with
 all bytes.

@examples(
  #"abc".append(#"def", #"ghi")
)

}


@doc(
  method (bstr :: Bytes).subbytes(rge :: Range)
    :: MutableBytes
  method (bstr :: Bytes).subbytes(start :: Nat,
                                  end :: Nat)
    :: MutableBytes
){

 When given two arguments, returns the substring of @rhombus(bstr)
 from @rhombus(start) (inclusive) to @rhombus(end) (exclusive).

 When given one argument, @rhombus(rge) is used to derive
 @rhombus(start) and @rhombus(end) as in @rhombus(String.substring).

@examples(
  #"hello".subbytes(2, 4)
  #"hello".subbytes(2..=4)
  #"hello".subbytes(2..)
  #"hello".subbytes(..4)
  #"hello".subbytes(..)
)

}

@doc(
  method (bstr :: Bytes).slice(start :: Int,
                               end :: Int = bstr.length())
    :: Bytes
){

 Similar to @rhombus(Bytes.subbytes) with integer arguments, but when
 @rhombus(start) or @rhombus(end) is negative, it is replaced by
 @rhombus(bstr.length()+start) or @rhombus(bstr.length()+end),
 respectively.

@examples(
  #"hello".slice(1, 3)
  #"hello".slice(1)
  #"hello".slice(1, -2)
  #"hello".slice(-3, 4)
)

}

@doc(
  method (bstr :: Bytes).copy() :: MutableBytes
){

 Returns a fresh mutable byte string with the same initial content as
 @rhombus(bstr).

@examples(
  def b = #"apple"
  b.copy()
  b.copy() is_now b
)

}


@doc(
  method Bytes.copy_from(dest_bstr :: MutableBytes,
                         dest_start :: Nat,
                         src_bstr :: Bytes,
                         src_start :: Nat = 0,
                         src_end :: Nat = Bytes.length(src_bstr))
    :: Void
){

 Copies bytes from @rhombus(src_bstr) at @rhombus(src_start) (inclusive) to
 @rhombus(src_end) (exclusive) into @rhombus(dest_bstr) starting at
 @rhombus(dest_start). The length of @rhombus(dest_bstr) must be at least
 @rhombus(dest_start + (src_end - src_start)).

}


@doc(
  method Bytes.fill(bstr :: MutableBytes, byte :: Byte) :: Void
){

 Sets every byte in @rhombus(bstr) to @rhombus(byte).

@examples(
  def b = #"apple".copy()
  b.fill(Byte#"x")
  b
)

}


@doc(
  method (bstr :: Bytes).snapshot(str :: Bytes) :: ImmutableBytes
){

 Returns an immutable byte string as-is or copies a mutable byte
 string's content to an immutable byte string.

@examples(
  ~repl:
    def b = #"apple"
    b.snapshot()
    b.snapshot() === b
  ~repl:
    def b = #"apple".copy()
    b.snapshot()
    b.snapshot() is_now b
)

}

@doc(
  method (bstr :: Bytes).utf8_string(
    err_char :: maybe(Char) = #false,
    start :: Nat = 0,
    end :: Nat = Bytes.length(bstr)
  ) :: String
  method (bstr :: Bytes).latin1_string(
    err_char :: maybe(Char) = #false,
    start :: Nat = 0,
    end :: Nat = Bytes.length(bstr)
  ) :: String
  method (bstr :: Bytes).locale_string(
    err_char :: maybe(Char) = #false,
    start :: Nat = 0,
    end :: Nat = Bytes.length(bstr)
  ) :: String
){

 Converts a byte string to a string, decoding as UTF-8, Latin-1, or the
 current locale's encoding. The @rhombus(err_char) argument provides a
 @tech{character} to use in place of an encoding error, where
 @rhombus(#false) means that an exception is thrown.

@examples(
  #"hello".utf8_string()
  #"hi \316\273".utf8_string()
  ~error:
    #"hi \316 xxx \273".utf8_string()
  #"hi \316 xxx \273".utf8_string(Char"?")
)

}


@doc(
  method (bstr :: Bytes).utf8_length(
    err_char :: maybe(Char) = #false,
    start :: Nat = 0,
    end :: Nat = Bytes.length(bstr)
  ) :: maybe(Nat)
  method (bstr :: Bytes).utf8_ref(
    skip :: Nat,
    err_char :: maybe(Char) = #false,
    start :: Nat = 0,
    end :: Nat = Bytes.length(bstr)
  ) :: maybe(Char)
  method (bstr :: Bytes).utf8_index(
    skip :: Nat,
    err_char :: maybe(Char) = #false,
    start :: Nat = 0,
    end :: Nat = Bytes.length(bstr)
  ) :: maybe(Nat)
){

 Returns information about the UTF-8 decoding of @rhombus(bstr)’s
 substring from @rhombus(start) to @rhombus(end), but without actually
 generating the decoded characters---except for the one character
 returned by @rhombus(Bytes.utf8_ref). If @rhombus(err_char) is
 @rhombus(#false) and the substring is not a UTF-8 encoding up to the
 point of getting a result value, the result is @rhombus(#false).
 Otherwise, @rhombus(err_char) is used to resolve decoding errors as in
 @rhombus(Bytes.utf8_string) and the result is never @rhombus(#false).

 The @rhombus(Bytes.utf8_length) method returns the length in characters
 of the decoded substring from @rhombus(start) to @rhombus(end).

 The @rhombus(Bytes.utf8_ref) method returns the character decoded in
 substring from @rhombus(start) to @rhombus(end), skipping @rhombus(skip)
 decoded characters.

 The @rhombus(Bytes.utf8_index) method returns the byte index starting
 the character that would be decoded in substring from @rhombus(start) to
 @rhombus(end), skipping @rhombus(skip) decoded characters. Note that a
 non-@rhombus(#false) result does not mean that bytes at the returned
 index are well formed as UTF-8, only that the bytes up to the returned
 index are available and well-formed. The returned index is relative to
 the start of @rhombus(bstr), not to @rhombus(start), and it is always
 less than @rhombus(end).

@examples(
  #"hi \316\273".utf8_length()
  #"hi \316 xxx \273".utf8_length()
  #"hi \316 xxx \273".utf8_length(Char"?")
  #"hi \316\273".utf8_ref(0)
  #"hi \316\273".utf8_ref(3)
  #"hi \316\273".utf8_index(0)
  #"hi \316\273!".utf8_index(4)
)

}


@doc(
  method (bstr :: Bytes).to_sequence(bstr :: Bytes) :: Sequence
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(bstr)'s bytes in order.

}
