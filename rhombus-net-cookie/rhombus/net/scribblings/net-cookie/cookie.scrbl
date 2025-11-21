#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus open
      net/cookie)

@title(~tag: "common"){Cookie Annotations}

@docmodule(net/cookie)

The @rhombusmodname(net/cookie) provides annotations that are common
to servers and user agents (i.e., clients).

@doc(
  annot.macro 'cookie.Name'
){

 Recognizes cookie-name strings and byte strings (as valid UTF-8
 encodings) that consist only of characters that are not control
 characters (see @rhombus(Char.is_iso_control)), whitespace (see
 @rhombus(Char.is_whitespace)), @litchar{"}, @litchar("@"), @litchar{?},
 @litchar{(}, @litchar{)}, @litchar{[}, @litchar{]}, @litchar("{"),
 @litchar("}"), @litchar{,}, @litchar{:}, @litchar{;}, @litchar{=},
 @litchar{<}, @litchar{>}, @litchar{/}, or @litchar{\}.

}

@doc(
  annot.macro 'cookie.Value'
){

 Recognizes cookie-value strings and byte strings that consist only of
 ASCII characters that are not control characters (see
 @rhombus(Char.is_iso_control)), whitespace (see
 @rhombus(Char.is_whitespace)), @litchar{"}, @litchar{,}, @litchar{;}, or
 @litchar{\}, except that @litchar{"} is allowed at the beginning and at
 the end of the string if it appears in both places.

}

@doc(
  annot.macro 'cookie.PathOrExtension'
){

 Recognizes strings that can be used as the value of @litchar{Path}
 attribute accoridng to @RFC6265 or as an additional attribute (or
 attribute/value pair) whose meaning is not specified by @RFC6265.

}

@doc(
  annot.macro 'cookie.Domain'
){

 Recognizes strings that contains a (sub)domain name as defined by
 @hyperlink("http://tools.ietf.org/html/rfc1034.html"){RFC 1034} (Section
 3.5) and @hyperlink("http://tools.ietf.org/html/rfc1123.html"){RFC
  1123}.

}
