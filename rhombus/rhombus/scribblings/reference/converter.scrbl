#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/bytes
      rhombus/bytes open)

@title(~tag: "converter"){Encoding and Conversion}

@docmodule(rhombus/bytes)

The @rhombusmodname(rhombus/bytes) module supports conversions between
string encodings at the byte-string level. A @rhombus(bytes.Converter)
object maintains conversion state, and it its
@rhombus(bytes.Converter.convert) method comverts from one encoding to
another. The set of available encodings and combinations varies by
platform, depending on available system libraries such as @tt{iconv}.

See @secref(~doc: model_doc, "encoding") for general information about
encodings.

@doc(
  class bytes.Converter():
    constructor (~from: from :: String,
                 ~to: to :: String)
){

 A @rhombus(Converter,~annot) object maintains state for converting a
 stream of bytes from the @rhombus(from) encoding to the @rhombus(to)
 encoding. The name @rhombus("") can be used as equivalent to the current
 locale's default encoding, which is also reported by
 @rhombus(system.locale_string_encoding).

 An exception is thrown if a @rhombus(from)--@rhombus(to) converter is
 not available. The @rhombus(from)--@rhombus(to) converters available
 mostly depend on system facilities, but number of pairs are always
 supported:

@itemlist(

 @item{@rhombus("UTF-8") to @rhombus("UTF-8"): The identity conversion,
  except that encoding errors in the input lead to a decoding failure.}

 @item{@rhombus("UTF-8-permissive") to @rhombus("UTF-8"): The identity
  conversion, except that any input byte that is not part of a valid
  encoding sequence is effectively replaced by the UTF-8 encoding sequence
  for @rhombus(Char"\uFFFD"). This handling of invalid sequences is
  consistent with the interpretation of port bytes streams into characters
  (see @secref(~doc: model_doc, "encoding")).}

 @item{@rhombus("") to @rhombus("UTF-8"): Converts from the current
  locale's default encoding (see @secref(~doc: model_doc, "encoding")) to
  UTF-8.}

 @item{@rhombus("UTF-8") to @rhombus(""): Converts from UTF-8 to the
  current locale's default encoding (see
  @secref(~doc: model_doc, "encoding")).}

 @item{@rhombus("platform-UTF-8") to @rhombus("platform-UTF-16"):
  Converts UTF-8 to UTF-16 on Unix and Mac OS, where each UTF-16 code unit
  is a sequence of two bytes ordered by the current platform’s endianness.
  On Windows, the conversion is the same as @rhombus("WTF-8")
  to @rhombus("WTF-16") to support unpaired surrogate code units.}

 @item{@rhombus("platform-UTF-8-permissive") to
  @rhombus("platform-UTF-16"): Like @rhombus("platform-UTF-8") to
  @rhombus("platform-UTF-16"), but an input byte that is not part of a
  valid UTF-8 encoding sequence (or valid for the unpaired-surrogate
  extension on Windows) is effectively replaced with
  @rhombus(Char"\uFFFD").}

 @item{@rhombus("platform-UTF-16") to @rhombus("platform-UTF-8"):
  Converts UTF-16 (bytes ordered by the current platform's endianness) to
  UTF-8 on Unix and Mac OS. On Windows, the conversion is the same as
  @rhombus("WTF-16") to @rhombus("WTF-8") to support unpaired surrogates.
  On Unix and Mac OS, surrogates are assumed to be paired: a pair of bytes
  with the bits @rhombus(0xD800) starts a surrogate pair, and the
  @rhombus(0x03FF) bits are used from the pair and following pair
  (independent of the value of the @rhombus(0xDC00) bits). On all
  platforms, performance may be poor when decoding from an odd offset
  within an input byte string.}

 @item{@rhombus("WTF-8") to @rhombus("WTF-16"): Converts the
  @hyperlink("https://wtf-8.codeberg.page"){WTF-8} superset of UTF-8 to a
  superset of UTF-16 to support unpaired surrogate code units, where each
  UTF-16 code unit is a sequence of two bytes ordered by the current
  platform’s endianness.}

 @item{@rhombus("WTF-8-permissive") to @rhombus("WTF-16"): Like
  @rhombus("WTF-8") to @rhombus("WTF-16"), but an input byte that is not
  part of a valid WTF-8 encoding sequence is effectively replaced with
  @rhombus(Char"\uFFFD").}

 @item{@rhombus("WTF-16") to @rhombus("WTF-8"): Converts the
  @hyperlink("https://wtf-8.codeberg.page"){WTF-8} superset of UTF-16 to
  the WTF-8 superset of UTF-8. The input can include UTF-16 code units
  that are unpaired surrogates, and the corresponding output includes an
  encoding of each surrogate in a natural extension of UTF-8.}

)

 Use @rhombus(Converter.convert) or @rhombus(Converter.convert_to) with
 the result to convert byte strings.

 A newly opened byte converter is registered with the current
 @tech{custodian}, so that the converter is closed as if by
 @rhombus(Converter.close) when the custodian is shut down. A converter
 is not registered with a custodian---and does not need to be closed---if
 it is one of the guaranteed combinations not involving @rhombus("") on
 Unix, or if it is any of the guaranteed combinations (including
 @rhombus("")) on Windows and Mac OS.

@examples(
  ~hidden:
    import rhombus/bytes
  ~repl:
    def cvt  = bytes.Converter(~from: "UTF-8", ~to: "UTF-16")
    cvt.convert(#"ABCD")
    cvt.convert(#"\303\247\303\260\303\266\302\243")
    cvt.close()
)

}

@doc(
  method (cvt :: bytes.Converter).close() :: Void
){

 Closes the converter and release its resources. A closed converter can
 no longer be used with @rhombus(Converter.convert),
 @rhombus(Converter.convert_to), @rhombus(Converter.end), or
 @rhombus(Converter.end_to).

}

@doc(
  method (cvt :: bytes.Converter).convert(
    src :: Bytes,
    ~start: src_start :: Nat = 0,
    ~end: src_end :: Nat = src.length()
  ) :: values(Bytes, Nat, Converter.Status)

  method (cvt :: bytes.Converter).convert_to(
    src :: Bytes,
    ~start: src_start :: Nat = 0,
    ~end: src_end :: Nat = src.length(),
    ~dest: dest :: MutableBytes,
    ~dest_start: dest_start :: Nat = 0,
    ~dest_end: dest_end :: Nat = dest.length()
  ) :: values(Nat, Nat, Converter.Status)

  enum bytes.Converter.Status
  | complete
  | continues
  | aborts
  | error
){

 Converts bytes in @rhombus(src) from offset @rhombus(src_start)
 (inclusive) to @rhombus(src_end) (exclusive). In the case of
 @rhombus(Converter.convert), the first result is a freshly allocated
 byte string. In the case of @rhombus(Converter.convert_to), the
 converted bytes are written to the given @rhombus(dest) (between
 @rhombus(dest_start) and @rhombus(dest_end)) and the number of written
 bytes is the first returned result.

 Conversion might not consume all of the @rhombus(src) bytes. The second
 result is the number of converted bytes, which can be no more than
 @rhombus(src_end-src_start). If it is less, the
 @rhombus(Converter.Status, ~annot) as the third result indicates why.

 The @rhombus(Converter.Status, ~annot) result is one of the following:

@itemlist(

 @item{@rhombus(#'complete): All supplied @rhombus(src) bytes (in the
  @rhombus(src_start) to @rhombus(src_end) range) where consumed for
  conversion.}

 @item{@rhombus(#'continues): Conversion stopped due to the limit on the
  result size or the space in @rhombus(dest). In this case, fewer than
  @rhombus(dest_end-dest_start) bytes may be written if more space is
  needed to process the next complete encoding sequence in
  @rhombus(src).}

 @item{@rhombus(#'aborts): The input stopped part-way through an
  encoding sequence, and more input bytes are necessary to continue. For
  example, if the last byte of input is @rhombus(0o303) for a
  @rhombus("UTF-8-permissive") decoding, the result is @rhombus(#'aborts),
  because another byte is needed to determine how to use the
  @rhombus(0o303) byte.}

 @item{@rhombus(#'error): The @rhombus(src) contains bytes that do not
  form a legal encoding sequence. Specifically, the bytes starting at
  @rhombus(src_start) plus the second result are ill-formed. This result
  is never produced for some encodings, where all byte sequences are valid
  encodings. For example, since @rhombus("UTF-8-permissive") handles an
  invalid UTF-8 sequence by dropping characters or generating @litchar{?},
  every byte sequence is effectively valid.}

)

 Applying a converter accumulates state in the converter (even when the
 third result of @rhombus(Converter.convert) or
 @rhombus(Converter.convert_to) is @rhombus(#'complete)). This state can
 affect both further processing of input and further generation of
 output, but only for conversions that involve ``shift sequences'' to
 change modes within a stream. To terminate an input sequence and reset
 the converter, use @rhombus(Converter.end) or
 @rhombus(Converter.end_to).

}

@doc(
  method (cvt :: bytes.Converter).end() :~ values(Bytes, Status)

  method (cvt :: bytes.Converter).end_to(
    ~dest: dest :: MutableBytes,
    ~dest_start: dest_start :: Nat = 0,
    ~dest_end: dest_end :: Nat = dest.length()
  ) :: values(Nat, Status)
){

 Like @rhombus(Converter.convert) and @rhombus(Converter.convert_to),
 but instead of converting bytes, this procedure generates an ending
 sequence for the conversion (sometimes called a ``shift sequence''), if
 any. Few encodings use shift sequences, so this function will succeed
 with no output for most encodings. In any case, successful output of a
 (possibly empty) shift sequence resets the converter to its initial
 state.

 In the case of @rhombus(Converter.end), the first result is a freshly
 allocated byte string. In the case of @rhombus(Converter.end_to), the
 converted bytes are written to the given @rhombus(dest) (between
 @rhombus(dest_start) and @rhombus(dest_end)) and the number of written
 bytes is the first returned result.

 The second result is @rhombus(#'complete) or @rhombus(#'continues) to
 indicate whether conversion completed. If @rhombus(#'complete), then an
 entire ending sequence was produced. If @rhombus(#'continues), then the
 conversion could not complete due to the limit on the result size or the
 space in @rhombus(dest), and the first result is either an empty byte
 string or @rhombus(0).

}

@doc(
  fun bytes.Converter.maybe_open(~from: from :: String, ~to: to :: String)
    :: maybe(Converter)
){

 Like the @rhombus(Converter) constructor, but returning @rhombus(#false)
 instead of throwing an exception if a @rhombus(from)--@rhombus(to)
 converter is not available.

}

@doc(
  Parameter.def bytes.current_locale :: maybe(String)
){

 A @tech{context parameter} for the current
 @tech(~doc: model_doc){locale}. See @secref(~doc: model_doc, "encoding")
 for more information.

}
