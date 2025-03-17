#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/rx open)

@title(~tag: "rx", ~style: #'toc){Regular Expressions}

The @rhombusmodname(rhombus/rx) library supports @deftech{regular
 expression} (i.e., @deftech{regexp}) matching on strings, byte
strings, and input @tech{ports}.

The @rhombus(rx) form creates a @rhombus(RX, ~class) object, which has
@rhombus(RX.match) and @rhombus(RX.match_in) methods to match a complete
string or to find the first matching position within a string. A
successful match is represented as a @rhombus(RXMatch, ~class) object,
which reports a matching (byte) string and as well as matches for
capture groups within the regexp pattern.

@local_table_of_contents()

@include_section("rx-lang.scrbl")
@include_section("rx-partial.scrbl")
@include_section("rx-byte-or-char.scrbl")
@include_section("rx-quickref.scrbl")