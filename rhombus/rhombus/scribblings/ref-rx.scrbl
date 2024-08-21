#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    rhombus/rx open
    scribble/rx open
    meta_label:
      rhombus/rx open)

@(def rx_eval = make_rhombus_eval())
@examples(
  ~eval: rx_eval
  ~hidden:
    import rhombus/rx open
)

@title(~style: #'toc, ~tag: "regexp"){Regular Expressions}

@docmodule(~use_sources: lib("rhombus/rx.rhm"),
           rhombus/rx)

A @deftech{regular expression}, or @deftech{regexp} can be matched
against the content of a @tech{string}, @tech{byte string}, or
@tech{input port}. The @rhombus(rx) and @rhombus(rx_in) forms create
regexps, which are represented as @rhombus(RX, ~annot) objects. A
successful match is represented as a @rhombus(RXMatch, ~annot) object,
which reports either a matching (byte) string or a @tech{range} of the
input.

The @rhombus(rx, ~bind) and @rhombus(rx_in, ~bind) binding forms match
input while directly binding named @tech{capture groups} within the
regexp pattern, instead of returning an @rhombus(RXMatch, ~annot)
object.

A regexp matches in either character or byte mode. The mode is inferred
by the elements of the pattern, but @rhombus(bytes, ~at rhombus/rx) or
@rhombus(string, ~at rhombus/rx) can force a choice of mode. A regexp in
character mode can be matched against a byte string or input port, in
which case it matches UTF-8 sequences whose decoding matches the
character regexp. A regexp in byte mode can similarly be matched against
strings, where it matches a string whose UTF-8 encoding matches the
string. Regexp matches are reported in terms of strings when the regexp
is in character mode and when the input is a string; otherwise, matches
are reported in terms of bytes.

@doc(
  expr.macro '«rx'$pat'»'
  expr.macro '«rx_in'$pat'»'
){

 A @tech{regexp}, which is represented as an instance of
 @rhombus(RX, ~annot).

 See @secref("rx-pattern") for patterns that can be used in
 @rhombus(pat).

 The @rhombus(rx) form produces a regexp that matches with
 @rhombus(RX.match) only when the whole input string, byte string, or
 port content matches the pattern. An @rhombus(rx_in) regexp matches
 with @rhombus(RX.match) the same as with @rhombus(RX.match_in), which
 means that it always can match against a portion of the input.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'any*'
    rx'any*'.match("abc")
  ~repl:
    rx'any ($more: any*)'.match("abc")
    rx'any ($more: any*)'.match("abc")[#'more]
  ~repl:
    rx'["a"-"z"]*'.match("abc")
    rx'["a"-"z"]*'.match("_abc_")
  ~repl:
    rx_in'["a"-"z"]+'.match("_abc_")
    rx_in'["a"-"z"]+'.match_range("_abc_")
)

}

@doc(
  bind.macro '«rx'$pat'»'
  bind.macro '«rx_in'$pat'»'
){

 Matches a string, byte string or input port whose content matches, and
 binds @tech{capture-group} names in @rhombus(pat) to their corresponding
 matches.

 See @secref("rx-pattern") for patterns that can be used in
 @rhombus(pat).

 The @rhombus(rx, ~bind) and @rhombus(rx_in, ~bind) bindings forms as
 analogous to the @rhombus(rx) and @rhombus(rx_in) expression forms,
 where @rhombus(rx, ~bind) matches only when the whole input matches, and
 @rhombus(rx_in, ~bind) can match a part of the input.


@examples(
  ~eval: rx_eval
  ~repl:
    def rx'"hello " ($name: any*)' = "hello alice"
    name
  ~repl:
    ~error:
      def rx'alpha+' = "!!! alice ???"
    def rx_in'$who: alpha+' = "!!! alice ???"
    who
)

}

@include_section("ref-rx-pattern.scrbl")
@include_section("ref-rx-charset.scrbl")
@include_section("ref-rx-object.scrbl")
@include_section("ref-rx-match.scrbl")

@close_eval(rx_eval)
