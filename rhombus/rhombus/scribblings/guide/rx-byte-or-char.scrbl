#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/rx open    
    meta_label:
      rhombus/rx open)

@(def rx_eval = make_rhombus_eval())

@examples(
  ~eval: rx_eval
  ~hidden:
    import:
      rhombus/rx open
)

@title(~tag: "rx-byte-or-char"){String, Byte String, and Port Matching}

A @tech{regexp} produces by @rhombus(rx) matches in either character
mode or byte mode. The mode is inferred from the @rhombus(rx) pattern.
For example, it a literal string is part of the pattern, then it must be
in character mode, but if a literal byte string is part of the pattern,
it must be in byte mode. The @rhombus(string, ~at rhombus/rx) and
@rhombus(bytes, ~at rhombus/rx) forms can be used to make the choice
explicit.

Either mode can work with a string input to match, and either can work
with a byte string input to match. In the case of string mode,
@rhombus(., ~at rhombus/rx) and @rhombus(any, ~at rhombus/rx) match a
Unicode character, so given a byte string input, they match UTF-8
encoding sequences, only. Along similar lines, a byte-based regexp given
a string input matches against the UTF-8 encoding of the string.

@examples(
  ~eval: rx_eval
  ~defn:
    def char_rx = rx'string: . . .'
    def byte_rx = rx'bytes: . . .'
  ~repl:
    char_rx.match("abc")
    char_rx.match("λλλ")
    byte_rx.match("abc")
    :
      byte_rx.match("λλλ") // six bytes in UTF-8
    char_rx.match(#"abc")
    :
      char_rx.match(#"a\xFF\xFF") // not valid UTF-8
    char_rx.match(#"\316\273\316\273\316\273")
)

A regexp match can be applied directly to an input @tech{port}, as
opposed to reading bytes or strings from the port and then matching.
Direct port matching is especially useful with @rhombus(rx_in) or
@rhombus(RX.match_in) to find the first match, because bytes can be read
from the port lazily to find a match, and no further bytes will be
consumed after a match ends. A port is treated like a byte string for
input, so even if a character-based regexp is used, results are reported
in terms of bytes.

@examples(
  ~eval: rx_eval
  ~defn:
    def inp = Port.Input.open_string("abcdef")
  ~repl:
    char_rx.match_in(inp)
    char_rx.match_in(inp)
    char_rx.match_in(inp)
  ~defn:
    def inp = Port.Input.open_string("abcdef")
  ~repl:
    rx'"d"'.match_range_in(inp)
)
