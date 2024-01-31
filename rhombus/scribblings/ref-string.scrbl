#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Strings}

A @deftech{string} is a sequence of Unicode @tech{characters}. A string
works with map-referencing @brackets to access a character via
@rhombus(#%index). A string also works with the @rhombus(++) operator to
append strings, but a @rhombus(+&) can be used to append strings with
the static guaratee that the result is a string. A string can be used as
@tech{sequence}, in which case it supplies its bytes in order.

Although Racket supports mutable strings, the @rhombus(String, ~annot)
annotation recognizes only immutable strings, and Rhombus operations
generate immutable strings. Some operations allow mutable strings as
input, and @rhombus(ReadableString, ~annot) recognizes both mutable and
immutable strings.

@dispatch_table(
  "string"
  String
  str.append(str2, ...)
  str.length()
  str.get(n)
  str.substring(arg, ...)
  str.utf8_bytes(arg, ...)
  str.latin1_bytes(arg, ...)
  str.locale_bytes(arg, ...)
  str.to_int()
  str.to_number()
  str.to_string()
  str.upcase(arg)
  str.downcase(arg)
  str.foldcase(arg)
  str.titlecase(arg)
  str.normalize_nfd()
  str.normalize_nfkd()
  str.normalize_nfc()
  str.normalize_nfkc()
  str.grapheme_span(arg, ...)
  str.grapheme_count(arg, ...)
)

@doc(
  annot.macro 'String'
  annot.macro 'ReadableString'
  annot.macro 'ReadableString.to_string'
){

 Matches strings. The @rhombus(ReadableString, ~annot) annotation allows mutable
 Racket strings as well as immutable Rhombus strings.
 The @rhombus(ReadableString.to_string, ~annot) @tech{converter annotation}
 allows the same strings as @rhombus(ReadableString, ~annot), but converts
 a mutable Racket string to an immutable Rhombus string.

}

@doc(
  fun to_string(v :: Any,
                ~mode: mode :: Any.of(#'text, #'expr) = #'text)
    :: String
){

 Coerces @rhombus(v)  to a string.

 The string for of a value corresponds to the way that @rhombus(print)
 would print, which means that strings, symbols, identifiers, and
 keywords convert as their character content in the default @rhombus(#'text) mode.

@examples(
  to_string(10)
  to_string('hello')
  to_string([1, 2, 3])
  to_string('hello', ~mode: #'expr)
)

}


@doc(
  operator ((v1 :: Any) +& (v2 :: Any)) :: String
){

 Coerces @rhombus(v1) and @rhombus(v2) to a string, then appends the strings.

 The value is coerced to a string in the same way as by
 @rhombus(to_string).

@examples(
  "hello" +& "world"
  "it goes to " +& 11
  "the list " +& [1, 2, 3] +& " has " +& 3 +& " elements"
)

}


@doc(
  fun String.append(str :: ReadableString, ...) :: String
){

 Appends all @rhombus(str)s to create a new string.

@examples(
  String.append()
  String.append("this")
  String.append("this", " and ", "that")
)

}


@doc(
  fun String.length(str :: ReadableString) :: NonnegInt
){

 Returns the number of characters in @rhombus(str).

@examples(
  String.length("hello")
  "hello".length()
)

}


@doc(
  fun String.get(str :: ReadableString, n :: NonnegInt) :: Char
){

 Equivalent to @rhombus(str[n]) (with the default implicit
 @rhombus(#%index) form). Returns the @rhombus(n)th character of
 @rhombus(str) (starting from @rhombus(0)).

@examples(
  "abc"[0] +& "abc".get(0)
)

}


@doc(
  fun String.substring(str :: ReadableString,
                       start :: NonnegInt,
                       end :: NonnegInt = String.length(str))
    :: String
){

 Returns the substring of @rhombus(str) from @rhombus(start) (inclusive)
 to @rhombus(end) (exclusive).

@examples(
  String.substring("hello", 2, 4)
  String.substring("hello", 2)
)

}


@doc(
  fun String.utf8_bytes(str :: ReadableString,
                        err_byte :: maybe(Byte) = #false,
                        start :: NonnegInt = 0,
                        end :: NonnegInt = String.length(str))
    :: Bytes
  fun String.latin1_bytes(str :: ReadableString,
                          err_byte :: maybe(Byte) = #false,
                          start :: NonnegInt = 0,
                          end :: NonnegInt = String.length(str))
    :: Bytes
  fun String.locale_bytes(str :: ReadableString,
                          err_byte :: maybe(Byte) = #false,
                          start :: NonnegInt = 0,
                          end :: NonnegInt = String.length(str))
    :: Bytes
){

 Converts a string to a byte string, encoding by UTF-8, Latin-1, or the
 current locale's encoding. The @rhombus(err_byte) argument provides a
 byte to use in place of an encoding error, where @rhombus(#false) means
 that an exception is thrown. (No encoding error is possible with
 @rhombus(String.utf8_bytes), but @rhombus(err_byte) is accepted for
 consistency.)

@examples(
  "hello".utf8_bytes()
)

}




@doc(
  fun String.to_int(str :: ReadableString) :: maybe(Int)
){

 Parses @rhombus(str) as an integer, returning @rhombus(#false) if the
 string does not parse as an integer, otherwise returning the integer
 value.

@examples(
  String.to_int("-42")
  String.to_int("42.0")
  String.to_int("fourty-two")
  "100".to_int()
)

}


@doc(
  fun String.to_number(str :: ReadableString) :: maybe(Number)
){

 Parses @rhombus(str) as a number, returning @rhombus(#false) if the
 string does not parse as a number, otherwise returning the number
 value.

@examples(
  String.to_number("-42")
  String.to_number("42.0")
  String.to_number("fourty-two")
  "3/4".to_number()
)

}


@doc(
  fun String.to_string(str :: ReadableString)
    :: String
  fun ReadableString.to_string(str :: ReadableString)
    :: String
){

 The same as @rhombus(to_string), but constrained to a
 @rhombus(ReadableString, ~annot) argument. These functions exist for consistency with
 the @rhombus(ReadableString.to_string, ~annot) annotation.

}


@doc(
  fun String.upcase(str :: ReadableString) :: String
  fun String.downcase(str :: ReadableString) :: String
  fun String.foldcase(str :: ReadableString) :: String
  fun String.titlecase(str :: ReadableString) :: String
){

 Case-conversion functions.

}

@doc(
  fun String.normalize_nfd(str :: ReadableString) :: String
  fun String.normalize_nfkd(str :: ReadableString) :: String
  fun String.normalize_nfc(str :: ReadableString) :: String
  fun String.normalize_nfkc(str :: ReadableString) :: String
){

 Unicode normalization functions.

}

@doc(
  fun String.grapheme_span(str :: ReadableString,
                           start :: NonnegInt = 0,
                           end :: NonnegInt = String.length(str))
    :: NonnegInt
){

 Returns the number of @tech{characters} (i.e., code points) in the
 string that form a Unicode grapheme cluster starting at @rhombus(start),
 assuming that @rhombus(start) is the start of a grapheme cluster and
 extending no further than the character before @rhombus(end). The result
 is @rhombus(0) if @rhombus(start) equals @rhombus(end).

 The @rhombus(start) and @rhombus(end) arguments must be valid indices as
 for @rhombus(String.substring).

}

@doc(
  fun String.grapheme_count(str :: ReadableString,
                            start :: NonnegInt = 0,
                            end :: NonnegInt = String.length(str))
    :: NonnegInt
){

 Returns the number of grapheme clusters in
 @rhombus(String.substring(str, start, end)).

 The @rhombus(start) and @rhombus(end) arguments must be valid indices as
 for @rhombus(String.substring).

}
