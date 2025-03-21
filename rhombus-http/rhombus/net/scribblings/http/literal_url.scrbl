#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~tag: "literal_url"){Literal URLs}

@doc(
  class LiteralURL():
    constructor (s :: String):
      super(easy.#{string->url/literal})

  method LiteralURL.to_string() :: String
){

 A @rhombus(LiteralURL, ~class) object represents Literal URL string
 that may not conform to the parsing implemented by @rhombus(url.URL).
 for user, path, query and fragment components. When converting to a
 string, only the components of those components that are not already
 percent encoded are encoded. A component is considered to be percent
 encoded if all of its percent characters are followed by two hexadecimal
 characters.

 Literal URLs are used automatically when handling redirects to avoid
 issues that may pop up when decoding an re-encoding URLs from
 standards-non-compliant servers.

}
