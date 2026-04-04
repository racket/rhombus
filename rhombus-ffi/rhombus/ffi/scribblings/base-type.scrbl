#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def ffi_eval = make_rhombus_eval())
@examples(
  ~eval: ffi_eval
  ~hidden:
    import ffi open
)

@title(~tag: "ffi-base-type"){Base Foreign Types}

@doc(
  ~nonterminal_key:
    * ~at rhombus/ffi/type
  grammar type
){

 A @rhombus(type) can only be written in specific type positions, such
 as in @rhombus(foreign.fun), @rhombus(foreign.struct), and
 @rhombus(mem).

 Every @rhombus(type) has a C representation and a Rhombus
 representation and conversion functions to get from one to the other.
 For example, the base type @rhombus_t(int_t) is represented on the C
 side as a @tt{int}, while it is represented in the racket side by a
 @rhombus(Int, ~annot) that fits into a 32-bit two's complement
 representation.

}

@doc(
  foreign.type ptr_t
){

 The @rhombus_t(ptr_t) type describes a generic pointer. On the C side,
 a generic pointer is represented as an address with the same
 representation as @tt{void*}. On the Rhombus side, a generic pointer is
 represented as a @tech{pointer} object. See also @secref("pointer").

 When an address is converted from C to Rhombus, then @rhombus_t(ptr_t)
 produces a pointer object that references memory (assumed to be) not
 managed by Racket's garbage collector. The @rhombus_t(ptr_t/gcable) type
 implies that a pointer converted from C should be treated as
 (potentially) managed by Rhombus's garbage collector. In both cases,
 conversion from Rhombus to C allows any pointer object.

 The @rhombus_t(void_t*) type is equivalent to @rhombus_t(ptr_t), and
 the @rhombus_t((void_t*)/gcable) type is equivalent to
 @rhombus_t(ptr_t/gcable).

}

@doc(
  foreign.type int8_t
  foreign.type uint8_t
  foreign.type int16_t
  foreign.type uint16_t
  foreign.type int32_t
  foreign.type uint32_t
  foreign.type int64_t
  foreign.type uint64_t
){

 Signed and unsigned integer @tech{scalar} types of specific bit widths
 on the C side. All are represented as exact integers on the Rhombus side,
 constrained to a range that fits in the unsigned or two's complement bit
 representation.

}

@doc(
  foreign.type byte_t
  foreign.type short_t
  foreign.type ushort_t
  foreign.type int_t
  foreign.type uint_t
  foreign.type long_t
  foreign.type ulong_t
  foreign.type intptr_t
  foreign.type uintptr_t
  foreign.type size_t
  foreign.type ssize_t
){

 Signed and unsigned integer @tech{scalar} types of platform-specific
 bit widths. For consistently, a @litchar{_t} is added to the end of C
 type names like @tt{int} to form a type name like @rhombus_t(int_t).

 All are represented as exact integers on the Rhombus side, constrained
 to a range that fits in the platform-specific C representation.

}

@doc(
  foreign.type float_t
  foreign.type double_t
){

 IEEE floating-point number @tech{scalar} types. On the C side, a
 @rhombus_t(float_t) is 8 bytes, and a @rhombus_t(double_t) is 16 bytes.
 On the Rhombus side, both are represented as
 @tech(~doc: ref_doc){flonums}.

}

@doc(
  foreign.type wchar_t
  foreign.type intwchar_t
){

 On the C side, both @rhombus_t(wchar_t) and @rhombus_t(intwchar_t)
 occupy the same number of bytes. On the Rhombus side, a
 @rhombus_t(wchar_t) is represented as a character, while a
 @rhombus_t(intwchar_t) is a @tech{scalar} type that is represented as an
 exact integer that fits into the platform-specific C representation.

 The range of @rhombus_t(wchar_t) on the C side may include integers
 that do not correspond to a Rhombus character, and it may omit values
 that do correspond to a Rhombus character. The Rhombus representation of
 a @rhombus_t(wchar_t) is constrained to characters that fit in the C
 representation, and values from C that are are not representable as
 Rhombus characters are converted to the Unicode replacement character,
 @rhombus(Char"\uFFFD").

}

@doc(
  foreign.type bool_t
  foreign.type boolint_t
){

 Boolean @tech{scalar} types. On the C side, @rhombus_t(bool_t)
 corresponds to the C @tt{bool} type from @tt{<stdbool.h>}, while
 @rhombus_t(boolint_t) corresponds to @tt{int} (which is often used for a
 boolean representation in C-based libraries). On the Rhombus side, both
 are represented by boolean values when received from C, and and Rhombus
 value is allowed when converting to C (where @rhombus(#false) is treated
 as false and all other values are treated as true).

}

@doc(
  foreign.type void_t
){

 A type with no representation on the C side and a @rhombus(#void)
 representation on the Rhombus side. The @rhombus_t(void_t) type can only
 be used for the result of a foreign procedure for foreign callback.

}

@doc(
  foreign.type string_t
  foreign.type bytes_t
  foreign.type bytes_ptr_t
  foreign.type path_t
){

 Types that are represented on the C side like @rhombus_t(ptr_t), but
 that are represented in Rhombus by conversion to and from strings, byte
 strings, and paths---or by @rhombus(#false) to represent a @tt{NULL}
 pointer.

 For Rhombus to C conversion, the @rhombus_t(string_t) type converts a
 Rhombus string to a null-terminated byte string and passes the address
 of the start of the byte string to the C side. The @rhombus_t(bytes_t)
 type similarly copies a Rhombus byte string to add a null terminator,
 while @rhombus_t(bytes_ptr_t) passes the start of a Rhombus byte string
 as-is, without adding a terminator (and where mutation of pointer
 content on the C side is reflected as changes to the byte string
 content). The @rhombus_t(path_t) is like @rhombus_t(string_t), but for
 paths in the sense of @rhombus(CrossPath, ~annot). All of these types
 convert @rhombus(#false) on the Racket side to @tt{NULL} on the C side.

 When converting from C to Rhombus, the non-@tt{NULL} pointer received
 from C is treated as a reference to a null-terminated C string, and a
 fresh Racket byte string is created to hold the content up to the null
 terminator. The @rhombus_t(string_t) or @rhombus_t(path_t) types then
 convert that byte string to a string or path, respectively. A @tt{NULL}
 from C is converted to @rhombus(#false) for Rhombus.

@examples(
  ~eval: ffi_eval
  ~repl:
    cast ~from (bytes_t) ~to (bytes_t) #"apple\0pie"
    cast ~from (bytes_t) ~to (string_t) #"apple\0pie"
    mem (cast ~from (bytes_ptr_t) ~to (byte_t*) #"apple\0pie")[6]
    cast ~from (string_t) ~to (path_t) "source"
    cast ~from (path_t) ~to (bytes_t) Path("source")
    cast ~to (string_t) uintptr_to_ptr(0)
)

}


@doc(
  foreign.type racket_t
){

 A type that is represented on the C side like @rhombus_t(ptr_t), but on
 the Rhombus side by an arbitrary value. This type can only be used for a
 procedure argument or result, and it will make sense only when
 interacting with a foreign procedure that is specifically aware of the
 Rhombus/Racket runtime system and cooperating with it.

}

@close_eval(ffi_eval)
