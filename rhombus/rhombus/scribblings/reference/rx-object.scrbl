#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/rx open)

@(def rx_eval = make_rhombus_eval())
@examples(
  ~eval: rx_eval
  ~hidden:
    import rhombus/rx open
)

@title{Regexp Objects}

@doc(
  class RX()
){

 Represents a @tech{regexp} as created with @rhombus(rx) or
 @rhombus(rx_in). This class cannot be instantiated directly.

}

@doc(
  property (regexp :: RX).num_captures :: Int
  property (regexp :: RX).capture_names :: Map
  property (regexp :: RX).has_backreference :: Boolean
  property (regexp :: RX).element :: (#'char || #'byte)
){

 Properties of a @tech{regexp}: its number of @tech{capture groups}, a
 mapping of symbolic capture-group names to indices (counting from 1),
 whether the regexp is implemented with backreferences (which affects
 regexp splicing via @rhombus($, ~at rhombus/rx)), and whether a match is
 in terms of characters or bytes.

}

@doc(
  method (regexp :: RX).match(input :: String || Bytes || Port.Input,
                              ~start: start :: Int = 0,
                              ~end: end :: maybe(Int) = #false,
                              ~input_prefix: input_prefix :: Bytes = #"",
                              ~unmatched_out: out :: maybe(Port.Output)
                                                = #false)
    :: maybe(RXMatch)
  method (regexp :: RX).match_in(....) :: maybe(RXMatch)
  method (regexp :: RX).match_range(....) :: maybe(RXMatch)
  method (regexp :: RX).match_range_in(....) :: maybe(RXMatch)
  method (regexp :: RX).is_match(....) :: Boolean
  method (regexp :: RX).is_match_in(....) :: Boolean
){

 Attempts to match a regular expression to @rhombus(input). For a regexp
 created with @rhombus(rx), the entire content (between @rhombus(start)
 and @rhombus(end)) must match for @rhombus(RX.match), while
 @rhombus(RX.match_in) can match a portion of the input. For a regexp
 created with @rhombus(rx_in), both @rhombus(RX.match) and
 @rhombus(RX.match_in) can match a portion of the input.

 The @rhombus(RX.match_range) and @rhombus(RX.match_range_in) methods
 are like @rhombus(RX.match) and @rhombus(RX.match_in), but the resulting
 @rhombus(RXMatch, ~annot) object reports @rhombus(Range) results instead
 of @rhombus(String) or @rhombus(Bytes) results. Range results are in
 terms of the start of the input, so if @rhombus(start) is not
 @rhombus(0), matching ranges will have only values of @rhombus(start)
 and greater.

 The @rhombus(RX.is_match) and @rhombus(RX.is_match_in) methods are like
 @rhombus(RX.match) and @rhombus(RX.match_in), but report just a boolean
 instead of assembling a @rhombus(RXMatch, ~annot) value in the case of a
 match.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'"a"'.match("a")
    rx'"a"'.match("ab")
    rx'"a"'.match_in("ab")
    rx'"a"'.is_match("ab")
    rx'"a"'.is_match_in("ab")
)

 The @rhombus(start) and @rhombus(end) arguments select a portion of the
 input to apply the match, where @rhombus(false) for @rhombus(end)
 corresponds to the end of @rhombus(input). The @rhombus(start) and
 @rhombus(end) positions correspond to characters for a string as
 @rhombus(input), and they correspond to bytes for a byte string or input
 port as @rhombus(input). Portions of @rhombus(input) outside of that
 range are ignored. For example, @rhombus(bof, ~at rhombus/rx) matches
 the @rhombus(start) offset of the full @rhombus(input).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'"a"*'.match_in("a aa aaa", ~start: 2)
)

 The @rhombus(input_prefix) argument specifies bytes that effectively
 precede input for the purposes of @rhombus(bol, ~at rhombus/rx) and
 other lookbehind matching. For example, a @rhombus(#"") prefix means
 that @rhombus(bof, ~at rhombus/rx) matches at the beginning of the
 input, while a @rhombus(#"\n") prefix means that
 @rhombus(bol, ~at rhombus/rx) can match the beginning of the input,
 while a @rhombus(bof, ~at rhombus/rx) cannot.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'bol "a"*'.match_in("aaa")
    rx'bol "a"*'.match_in("aaa", ~input_prefix: #"x")
)

 If @rhombus(out) is provided as an output port for the
 @rhombus(~unmatched_out) argument, the part of @rhombus(input) from its
 beginning (including before @rhombus(start)) that precedes the match is
 written to the port. All input up to @rhombus(end) is written to
 @rhombus(out) if no match is found. This functionality is most useful
 when @rhombus(input) is an input port.

@examples(
  ~eval: rx_eval
  ~repl:
    def out = Port.Output.open_string()
    rx'"a"+'.match_in("before aaa after", ~unmatched_out: out)
    out.get_string()
)

}

@doc(
  method (regexp :: RX).try_match(input :: Port.Input,
                                  ~start: start :: Int = 0,
                                  ~end: end :: maybe(Int) = #false,
                                  ~input_prefix: input_prefix :: Bytes = #"",
                                  ~unmatched_out: out :: maybe(Port.Output)
                                                    = #false)
    :: maybe(RXMatch)
  method (regexp :: RX).try_match_in(....) :: maybe(RXMatch)
){

 Like @rhombus(RX.match) and @rhombus(RX.match_in), but no bytes are
 consumed from @rhombus(input) if the pattern does not match.

@examples(
  ~eval: rx_eval
  def p = Port.Input.open_string("hello")
  rx'"hi"'.try_match(p)
  p.peek_char()
  rx'"hi"'.match(p)
  p.peek_char()
)

}




@doc(
  method (regexp :: RX).matches(input :: String || Bytes || Port.Input,
                                ~start: start :: Int = 0,
                                ~end: end :: maybe(Int) = #false,
                                ~input_prefix: input_prefix :: Bytes = #"")
    :: List.of(String || Bytes)
  method (regexp :: RX).split(input :: String || Bytes || Port.Input,
                              ~start: start :: Int = 0,
                              ~end: end :: maybe(Int) = #false,
                              ~input_prefix: input_prefix :: Bytes = #"")
    :: List.of(String || Bytes)
){

 Like @rhombus(RX.match_in), but finding all non-overlapping matches.
 The @rhombus(RX.matches) method returns the found matches, and
 @rhombus(RX.split) returns the complement, i.e., the strings that are
 between matches. The result from @rhombus(RX.split) will start or end
 with empty strings if the regexp matches the start or end of the input,
 respectively.


@examples(
  ~eval: rx_eval
  rx'any ["abc"] any'.matches("xbx ycy")
  rx'any ["abc"] any'.matches(#"xbx ycy")
)

}


@doc(
  method (regexp :: RX).replace(
    input :: String || Bytes,
    insert :: (String || Bytes || Function.of_arity(1+num_captures)),
    ~input_prefix: input_prefix :: Bytes = #""
  ) :: String || Bytes
  method (regexp :: RX).replace_all(....)
    :: String || Bytes
){

 Like @rhombus(RX.match_in), but restricted to string and byte string
 inputs, and returning the input with the partial matches replaced by
 @rhombus(insert). The @rhombus(RX.replace) method replaces only the
 first partial match, while @rhombus(RX.replace_all) replaces all
 non-overlapping partial matches.

 If @rhombus(insert) is a string or byte string, then it is used in
 place of a match for the output. If @rhombus(insert) is a function, then
 it receives at least one argument, plus an additional argument for each
 @tech{capture group} in the regular expression; the result of calling
 @rhombus(input) for each match is used as the replacement for the match.

@examples(
  ~eval: rx_eval
  rx'any "x" any'.replace("extra text", "_")
  rx'any "x" any'.replace_all("extra text", "_")
  rx'any "x" any'.replace("extra text", fun (s): "(" ++ s ++ ")")
  rx'any "x" any'.replace_all("extra text", fun (s): "(" ++ s ++ ")")
  rx'any "x" ($last: any)'.replace_all("extra text",
                                       fun (s, l): "(" ++ l ++ ")")
)

}


@doc(
  method (regexp :: RX).max_lookbehind()
){

 Reports the maximum number of characters or bytes needed before the
 start of a match.

@examples(
  ~eval: rx_eval
  rx'lookbehind("abc")'.max_lookbehind()
  rx'any lookbehind("abc")'.max_lookbehind()
  rx'any'.max_lookbehind()
)

}


@doc(
  property (regexp :: RX).handle
  property (regexp :: RX).in_handle
){

 The @rhombus(RX.handle) and @rhombus(RX.in_handle) properties produce a
 Racket-level regular expression object that corresponds to
 @rhombus(RX.match) and @rhombus(RX.match_in), respectively.
}

@doc(
  fun RX.from_handles(handle,
                      in_handle,
                      num_captures :: NonnegInt,
                      vars :: Map.of(Symbol, NonnegInt),
                      ~has_backref: has_backref = #false,
                      ~source: source :: String = "rx '....'")
    :: RX
){

 Constructs an @rhombus(RX, ~annot) object given Racket-level regular
 expressions for whole-input and partial-input matching, the number of
 capture groups in the pattern (which should be the same for both
 handles), and a mapping from capture-group names, if any, to indices.
 The optional @rhombus(has_backref) argument determines whether the
 @rhombus(RX, ~annot) can be spliced into other @tech{regexp} patterns.
 The optional @rhombus(source) string is the printed representation of the
 pattern.

}


@close_eval(rx_eval)
