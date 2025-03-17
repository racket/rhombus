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

@title(~tag: "rx-partial"){Full versus Partial Regexp Matching}

By default, a @tech{regexp} matches an input string only when it matches
the entire string.

@examples(
  ~eval: rx_eval
  rx'any'.match("x")
  rx'any'.match("xy")
)

A match to only part of the input can be enabled in either of two ways:
by using @rhombus(rx_in) to create the regexp, or by using the
@rhombus(RX.match_in) method to match to input. Using both
@rhombus(rx_in) and @rhombus(RX.match_in) has the same effect as using
only one of them. A partial match locates the the earliest point in the
input where a successful match can start.

@examples(
  ~eval: rx_eval
  rx'any'.match_in("xy")
  rx'any "x"'.match_in("xyxzx")
)

The @rhombus(bof, ~at rhombus/rx) and @rhombus(eof, ~at rhombus/rx)
pattern operators explicitly match the beginning or end of an input. A
pattern that uses those operators at the start and end will match the
same with @rhombus(rx), @rhombus(rx_in), @rhombus(RX.match) and
@rhombus(RX.match_in).

@examples(
  ~eval: rx_eval
  rx'bof any eof'.match_in("xy")
)

The @rhombus(bol, ~at rhombus/rx) and @rhombus(eol, ~at rhombus/rx)
operators match the start or end of a line, which includes the start and
end of an input, but also includes the positions just after and just
before a newline character.

@examples(
  ~eval: rx_eval
  rx'bol any eol'.match_in("xy")
  rx'bol any eol'.match_in("x\ny")
  rx'bol any eol'.match_in("\ny")
)

For partial matches or for locating capture groups within an input,
@rhombus(RX.match_range) and @rhombus(RX.match_range_in) return a
@rhombus(Range, ~annot) for each match, instead of the matching
characters.

@examples(
  ~eval: rx_eval
  rx'any "x"'.match_range_in("xyx")
  rx'any "x"* ($between: any) "x"*'.match_range("xxxxyxx")
)
