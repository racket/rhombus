#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/rx open
    meta_label:
      rhombus/rx open)

@title{Strings}

A @deftech{string} is a sequence of Unicode @tech{characters}. A string
works with map-referencing @brackets to access a character via
@rhombus(#%index). A string also works with the @rhombus(++) operator to
append strings, but a @rhombus(+&) can be used to append strings with
the static guarantee that the result is a string. A string can be used as
@tech{sequence}, in which case it supplies its characters in order.

Although Racket supports mutable strings, the @rhombus(String, ~annot)
annotation recognizes only immutable strings, and Rhombus operations
generate immutable strings. Some operations allow mutable strings as
input, and @rhombus(ReadableString, ~annot) recognizes both mutable and
immutable strings.

Two strings are equal by @rhombus(==) only if they are both immutable
and have the same character content. Two strings are equal by
@rhombus(is_now) as long as they have equal contents, even if one is
mutable and the other is immutable.

Strings are @tech{comparable}, which means that generic operations like
@rhombus(<) and @rhombus(>) work on strings.

@doc(
  annot.macro 'String'
  annot.macro 'ReadableString':
    ~method_fallback: String
  annot.macro 'ReadableString.to_string'
  annot.macro 'MutableString':
    ~method_fallback: String
){

 Matches strings. The @rhombus(ReadableString, ~annot) annotation allows mutable
 Racket strings as well as immutable Rhombus strings, while @rhombus(MutableString, ~annot)
 matches only mutable strings.
 The @rhombus(ReadableString.to_string, ~annot) @tech(~doc: guide_doc){converter annotation}
 allows the same strings as @rhombus(ReadableString, ~annot), but converts
 a mutable Racket string to an immutable Rhombus string, like
 @rhombus(String.snapshot).

 Static information associated by @rhombus(String, ~annot), etc., makes
 an expression acceptable as a sequence to @rhombus(for) in static mode.

}

@doc(
  fun to_string(v :: Any,
                ~mode: mode :: PrintMode = #'text) :: String
  fun repr(v :: Any) :: String
){

 The @rhombus(to_string) function coerces @rhombus(v) to a string.

 The string form of a value corresponds to the way that @rhombus(print)
 would print, which means that strings, symbols, identifiers, and
 keywords convert as their character content in the default @rhombus(#'text) mode.

 The @rhombus(repr) function is a shorthand for @rhombus(to_string) with
 @rhombus(~mode: #'expr).

 For converting a syntax object to a string, see also
 @rhombus(Syntax.to_source_string).

@examples(
  to_string(10)
  to_string('hello')
  to_string([1, 2, 3])
  to_string('hello', ~mode: #'expr)
)

}


@doc(
  operator ((v1 :: Any) +& (v2 :: Any)) :: String:
    ~order: concatenation
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
  method String.append(str :: ReadableString, ...) :: String
){

 Appends all @rhombus(str)s to create a new string.

@examples(
  String.append()
  String.append("this")
  String.append("this", " and ", "that")
)

}

@doc(
  fun String.make(len :: Nat, c :: Char) :: String
){

 Creates a string of length @rhombus(n) where every position in the
 string contains @rhombus(c).

@examples(
  String.make(5, Char"x")
)

}


@doc(
  method String.length(str :: ReadableString) :: Nat
){

 Returns the number of characters in @rhombus(str).

@examples(
  String.length("hello")
  "hello".length()
)

}


@doc(
  method String.get(str :: ReadableString, n :: Nat) :: Char
){

 Equivalent to @rhombus(str[n]) (with the default implicit
 @rhombus(#%index) form). Returns the @rhombus(n)th character of
 @rhombus(str) (starting from @rhombus(0)).

@examples(
  "abc"[0] +& "abc".get(0)
)

}


@doc(
  method String.substring(str :: ReadableString,
                          rge :: Range)
    :: String
  method String.substring(str :: ReadableString,
                          start :: Nat,
                          end :: Nat)
    :: String
){

 When given two arguments, returns the substring of @rhombus(str) from
 @rhombus(start) (inclusive) to @rhombus(end) (exclusive).

 When given one argument, @rhombus(rge) is used to derive
 @rhombus(start) and @rhombus(end). The @rhombus(rge) range is first
 canonicalized in the sense of @rhombus(Range.canonicalize), then
 @rhombus(0) or @rhombus(str.length()) is added as the starting
 or ending point if @rhombus(rge) lacks one (i.e.,
 @rhombus(rge.start()) or @rhombus(rge.end()) is @rhombus(#neginf) or
 @rhombus(#inf)). The derived range's starting and ending points are
 then taken as @rhombus(start) and @rhombus(end).

@examples(
  String.substring("hello", 2, 4)
  "hello".substring(2..=4)
  "hello".substring(2..)
  "hello".substring(..4)
  "hello".substring(..)
)

}


@doc(
  method String.find(str :: ReadableString,
                     substr :: ReadableString)
    :: maybe(Nat)
  method String.contains(str :: ReadableString,
                         substr :: ReadableString)
    :: Boolean
  method String.starts_with(str :: ReadableString,
                            substr :: ReadableString)
    :: Boolean
  method String.ends_with(str :: ReadableString,
                          substr :: ReadableString)
    :: Boolean
){

 Checks whether @rhombus(str) contains @rhombus(substr) as a substring.
 The @rhombus(String.find) function reports the first position in
 @rhombus(str) where @rhombus(substr) starts, if @rhombus(substr) is
 found, while @rhombus(String.contains) reports only whether it was
 found. The @rhombus(String.starts_with) and
 @rhombus(String.ends_with) functions return @rhombus(#true) only when
 @rhombus(substr) is at the start or end of @rhombus(str),
 respectively. Note that strings do @emph{not} implement
 @rhombus(MembershipTestable, ~class), because
 @rhombus(String.contains) finds a substring instead of character.

@examples(
  String.contains("howdy", "how")
  "howdy".contains("how")
  "howdy".contains("nope")
  "say howdy".find("how")
  "say howdy".find("nope")
  "say howdy".starts_with("say")
  "say howdy".ends_with("dy")
)

}


@doc(
  method String.replace(str :: ReadableString,
                        from :: ReadableString || RX,
                        to :: ReadableString,
                        ~all: all = #false)
    :: String
){

 Replaces either the first or every (depending on the @rhombus(all)
 argument) non-overlapping instance of @rhombus(from) in @rhombus(str)
 with @rhombus(to).

@examples(
  String.replace("Hello", "l", "x")
  String.replace("Hello", "l", "x", ~all: #true)
)

}


@doc(
  fun String.join(
    strs :: List.of(ReadableString),
    sep :: ReadableString = " ",
    ~before_last: before_last :: ReadableString = sep
  ) :: String
){

 Appends the strings in @rhombus(strs) with @rhombus(sep) in between,
 except that @rhombus(before_last) is used between the next-to-last and
 last string when @rhombus(strs) has more than one element.

@examples(
  String.join(["Hello", "World"])
  String.join(["lions", "tigers", "bears"], ", ", ~before_last: ", and ")
)

}


@doc(
  method String.split(str :: ReadableString,
                      sep :: ReadableString || RX = rx'space+',
                      ~trim: trim = #true,
                      ~repeat: repeat = #false)
    :: List.of(String)
){

 Finds non-overlapping instances of @rhombus(sep) in @rhombus(str)
 and returns a list of substrings that appear between the @rhombus(sep)
 instances.

 If @rhombus(trim) is true, then the result list never starts or ends
 with an empty string. Otherwise, an instance of @rhombus(sep) at the
 start or end of @rhombus(str) implies an empty-string result.

 The @rhombus(repeat) argument is relevant only when @rhombus(sep) is a
 string instead of a @tech{regexp}. When @rhombus(repeat) is true, then
 @rhombus(sep) is converted to a pattern that matches one or more
 consecutive instances of @rhombus(sep).

@examples(
  ~hidden:
    import rhombus/rx open
  ~repl:
    "Hello  World".split()
    "Hello  World".split(" ")
    "Hello  World".split(" ", ~repeat: #true)
    "Hello  World".split(rx'upper')
    "Hello  World".split(rx'upper', ~trim: #false)
)

}


@doc(
  method String.trim(str :: ReadableString,
                     sep :: ReadableString || RX = rx'space+',
                     ~start: start = #true,
                     ~end: end = #true,
                     ~repeat: repeat = #false)
    :: List.of(String)
){

 Removes matches to @rhombus(sep) from the start (when @rhombus(start)
 is true) and end (when @rhombus(end) is true) of @rhombus(str).

 When @rhombus(repeat) is true, then repeated matches are found and
 removed at each end of the string that is trimmed. Note that repeating
 is not useful with regular expressions with repetition built in,
 including the default value of @rhombus(sep).

 When trimming both the start and end of a string, matches are found on
 the original string, as opposed to first trimming the start, then
 trimming that result at the end. If matches at the start and end of the
 string overlap, then the result is an empty string.

@examples(
  ~hidden:
    import rhombus/rx open
  ~repl:
    "  Hello World  ".trim()
    "  Hello World  ".trim(~start: #false)
    "  Hello World  ".trim(~end: #false)
    "_Hello World__".trim("_")
    "_Hello World__".trim("_", ~repeat: #true)
    "aBaBa".trim("aBa")
)

}


@doc(
  method String.utf8_bytes(str :: ReadableString,
                           err_byte :: maybe(Byte) = #false,
                           start :: Nat = 0,
                           end :: Nat = String.length(str))
    :: Bytes
  method String.latin1_bytes(str :: ReadableString,
                             err_byte :: maybe(Byte) = #false,
                             start :: Nat = 0,
                             end :: Nat = String.length(str))
    :: Bytes
  method String.locale_bytes(str :: ReadableString,
                             err_byte :: maybe(Byte) = #false,
                             start :: Nat = 0,
                             end :: Nat = String.length(str))
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
  method String.utf8_length(str :: ReadableString,
                            start :: Nat = 0,
                            end :: Nat = String.length(str))
    :: Nat
){

 Returns the length in bytes of the UTF-8 encoding of @rhombus(str)’s
 substring from @rhombus(start) to @rhombus(end), but without actually
 generating the encoded bytes.

@examples(
  "hello".utf8_length()
  "heλλo".utf8_length()
)

}


@doc(
  method String.to_int(
    str :: ReadableString,
    ~radix: radix :: Int.in(2 ..= 16) = 10
  ) :: Int
  annot.macro 'String.to_int'
  method String.maybe_to_int(
    str :: ReadableString,
    ~radix: radix :: Int.in(2 ..= 16) = 10
  ) :: maybe(Int)
){

 The @rhombus(String.to_int) function parses @rhombus(str) as a base-@rhombus(radix)
 integer and returns the integer value. If @rhombus(str) does not
 parse as an integer, the @rhombus(Exn.Fail.Contract, ~class)
 exception is thrown. The @rhombus(String.maybe_to_int) function is
 like @rhombus(String.to_int), but it returns @rhombus(#false) if
 @rhombus(str) does not parse as an integer.

@examples(
  String.to_int("-42")
  ~error:
    String.to_int("42.0")
  String.maybe_to_int("42.0")
  ~error:
    String.to_int("forty-two")
  String.maybe_to_int("forty-two")
  "100".to_int()
  "100".maybe_to_int()
  "100".to_int(~radix: 16)
)

 The @rhombus(String.to_int, ~annot) @tech(~doc: guide_doc){converter annotation} is
 satisfied by an immutable string that can be converted to an integer via
 @rhombus(String.to_int), and it converts to that integer.

@examples(
  def n :: String.to_int = "-42"
  n
  ~error:
    def m :: String.to_int && Int.in(0 ..= 40) = "-42"
)

}


@doc(
  method String.to_number(
    str :: ReadableString,
    ~radix: radix :: Int.in(2 ..= 16) = 10
  ) :: Number
  annot.macro 'String.to_number'
  method String.maybe_to_number(
    str :: ReadableString,
    ~radix: radix :: Int.in(2 ..= 16) = 10
  ) :: maybe(Number)
){

 The @rhombus(String.to_number) function parses @rhombus(str) as a base-@rhombus(radix)
 number and returns the number value. If @rhombus(str) does not
 parse as a number, the @rhombus(Exn.Fail.Contract, ~class)
 exception is thrown. The @rhombus(String.maybe_to_number) function is
 like @rhombus(String.to_number), but it returns @rhombus(#false) if
 @rhombus(str) does not parse as a number.

@examples(
  String.to_number("-42")
  String.to_number("42.0")
  ~error:
    String.to_number("forty-two")
  String.maybe_to_number("forty-two")
  "3/4".to_number()
  "3/4".maybe_to_number()
  String.to_number("42.0", ~radix: 16)
)

 The @rhombus(String.to_number, ~annot) @tech(~doc: guide_doc){converter annotation} is
 satisfied by an immutable string that can be converted to a number via
 @rhombus(String.to_number), and it converts to that number.

@examples(
  def n :: String.to_number = "42.0"
  n
)

}

@doc(
  fun String.from_int(
    n :: Int,
    ~radix: radix :: Int.in(2 ..= 16) = 10
  ) :: String
  fun String.from_number(n :: Number) :: String
){

 Like @rhombus(to_string) on @rhombus(n), but in the case of
 @rhombus(String.from_int), a base @rhombus(radix) can be supplied.

@examples(
  String.from_int(17)
  String.from_int(16, ~radix: 16)
  String.from_number(3/4)
)

}


@doc(
  method String.to_string(str :: ReadableString)
    :: String
  fun ReadableString.to_string(str :: ReadableString)
    :: String
){

 The same as @rhombus(to_string), but constrained to a
 @rhombus(ReadableString, ~annot) argument. In other words, these
 equivalent to @rhombus(String.snapshot). The
 @rhombus(ReadableString.to_string, ~annot) function exists for
 consistency with the @rhombus(ReadableString.to_string, ~annot)
 annotation.

}


@doc(
  method String.upcase(str :: ReadableString) :: String
  method String.downcase(str :: ReadableString) :: String
  method String.foldcase(str :: ReadableString) :: String
  method String.titlecase(str :: ReadableString) :: String
  method String.locale_upcase(str :: ReadableString) :: String
  method String.locale_downcase(str :: ReadableString) :: String
){

 Case-conversion functions. The @rhombus(locale_upcase) and
 @rhombus(locale_downcase) functions are sensitive to the current locale,
 but the other functions are locale-independent conversions defined by
 the Unicode standard.

}

@doc(
  method String.normalize_nfd(str :: ReadableString) :: String
  method String.normalize_nfkd(str :: ReadableString) :: String
  method String.normalize_nfc(str :: ReadableString) :: String
  method String.normalize_nfkc(str :: ReadableString) :: String
){

 Unicode normalization functions.

}

@doc(
  method String.grapheme_span(str :: ReadableString,
                              start :: Nat = 0,
                              end :: Nat = String.length(str))
    :: Nat
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
  method String.grapheme_count(str :: ReadableString,
                               start :: Nat = 0,
                               end :: Nat = String.length(str))
    :: Nat
){

 Returns the number of grapheme clusters in
 @rhombus(String.substring(str, start, end)).

 The @rhombus(start) and @rhombus(end) arguments must be valid indices as
 for @rhombus(String.substring).

}

@doc(
  method String.to_sequence(str :: ReadableString) :: Sequence
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(str)'s characters in order.

@examples(
  :
    for List (c in "hello"): // optimizing
      c
  :
    for List (c in "hello".to_sequence()): // non-optimizing
      c
)

}

@doc(
  method String.copy(str :: ReadableString) :: MutableString
){

 Creates a mutable copy of @rhombus(str).

@examples(
  def s = "apple"
  s.copy()
  s.copy() is_now s
)

}

@doc(
  method String.snapshot(str :: ReadableString) :: String
){

 Returns an immutable string as-is or copies a mutable string's content to
 an immutable string.

@examples(
  ~repl:
    def s = "apple"
    s.snapshot()
    s.snapshot() === s
  ~repl:
    def s = "apple".copy()
    s.snapshot()
    s.snapshot() is_now s
)

}

@doc(
  annot.macro 'StringCI'
  annot.macro 'ReadableStringCI'
){

 A @tech{veneer} for a string that redirects @tech{comparable}
 operations like @rhombus(<) and @rhombus(>) to case-insensitive
 comparisons, equivalent to using @rhombus(String.foldcase) on each
 string before comparing.

 As always for a veneer, @rhombus(StringCI, ~annot) and
 @rhombus(ReadableStringCI, ~annot) work only in static mode (see
 @rhombus(use_static)) to help ensure that they have the intended effect.

@examples(
  ~hidden:
    use_static
  ~repl:
    "apple" < "BANANA"
    ("apple" :: StringCI) < ("BANANA" :: StringCI)
)

}


@doc(
  annot.macro 'StringLocale'
  annot.macro 'ReadableStringLocale'
  annot.macro 'StringLocaleCI'
  annot.macro 'ReadableStringLocaleCI'
){

 Like @rhombus(StringCI, ~annot) and @rhombus(ReadableStringCI, ~annot),
 but for locale-sensitive case-sensitive and case-insensitive
 comparisons.

}
