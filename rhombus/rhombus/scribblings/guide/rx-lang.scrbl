#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/rx open    
    meta_label:
      rhombus/rx open)

@(def rx_eval = make_rhombus_eval())

@title(~tag: "rx-lang"){Regular Expression Language}

A regexp pattern with @rhombus(rx) is written between @quotes using a
language that is distinct from Rhombus's normal expression language. A
pair of @quotes does not form a string, but instead groups a set of
shrubbery terms, and the @rhombus(rx) form interprets those terms
differently than Rhombus expressions, even though they use some of the
same operators.

For example,

@rhombusblock(
  rx'.*'
)

is a regular expression that matches any number of repetitions of any
non-newline character, since the @rhombus(., ~at rhombus/rx) operator
matches any non-newline character, and the @rhombus(*, ~at rhombusrx)
operator combines with the preceding pattern to match any number (zero
or more) repetitions of that pattern.

@margin_note_block{More precisely, @rhombus(.*, ~at rhombus/rx) is a single
 shrubbery operator, but @rhombus(.*, ~at rhombus/rx) is defined as an
 alias of @rhombus(., ~at rhombus/rx) followed by
 @rhombus(.*, ~at rhombus/rx).}

Literal characters are written as strings, so the regexp

@rhombusblock(
  rx'"a"* "b"*'
)

matches any number of @litchar{a}s followed by any number of
@litchar{b}s. Juxtaposed patterns, such as
@rhombus("a"*, ~at rhombus/rx) followed by
@rhombus("b"*, ~at rhombus/rx), match an input that is a match to the
first pattern followed by a match to the second.

@examples(
  ~eval: rx_eval
  ~defn:
    import:
      rhombus/rx open
  ~repl:
    rx'"a"* "b"*'.match("aaabb")
    rx'"a"* "b"*'.match("xxaaabb")
    rx'"a"* "b"*'.match("aaabbxx")
    :
      rx'"a"* "b"*'.match_in("xx") // matches 0 repetitions at start
)

The @rhombus(+,  ~at rhombus/rx) and @rhombus(?,  ~at rhombus/rx)
operators have their conventional meanings of ``one or more repetitions''
and ``zero or one repetitions.''

@examples(
  ~eval: rx_eval
  ~repl:
    rx'"a"+ "b"+'.match("aaabb")
    rx'"a"+ "b"+'.match_in("xxaaabbxx")
)

A string matches a sequence of characters. To match any character in a
set, use @brackets to create the set, and list characters in the set as
strings. The @rhombus(-, ~at rhombus/rx_charset) operator in a character
set adds an inclusive range of characters to the set.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'"a" "b"+'.match("ba")
    rx'["a" "b"]+'.match("ba")
    rx'["a"-"z"]+'.match("ba")
)

A character-set pattern is a special case of pattern alternatives, but
the @rhombus(||, ~at rhombus/rx) operator supports alternatives more
generally. A pattern formed with @rhombus(||, ~at rhombus/rx) matches
when the pattern to its left matches or the pattern to its right
matches. The precedence of @rhombus(||, ~at rhombus/rx) is weaker than
juxtaposition. Parentheses are simply grouping forms, and they do not
imply not @tech{capture groups} as in some regexp notations.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'"a"+ "b"+ || "x"+ "y"+'.match("aaabb")
    rx'"a"+ "b"+ || "x"+ "y"+'.match("xxyyy")
    rx'"a"+ "b"+ || "x"+ "y"+'.match("xxbbb")
    rx'("a"+ "b"+ || "x"+ "y"+) "z"'.match("abz")
    rx'("a"+ "b"+ || "x"+ "y"+) "z"'.match("xyz")
)

The @rhombus($, ~at rhombus/rx) operator creates a @deftech{capture
 group} when it is followed by an identifier and @colon block containing
a pattern. It matches the same as the pattern in the block, but it also
associates that match with the specified identifier, which is useful
when the @rhombus($, ~at rhombus/rx) pattern is part of a larger
pattern. The @rhombus(RXMatch, ~class) result of @rhombus(RX.match) can
be indexed in the same way as a map (see @secref("map")) using the
symbol forms of the identifier, and that extract the matching portion of
the input. Alternatively, a match can be indexed by position of the
capture group, where index @rhombus(0) corresponds to the whole pattern.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'($prefix: "a"+ "b"+ || "x"+ "y"+) "z"'.match("xyz")
    def m = rx'($prefix: "a"+ "b"+ || "x"+ "y"+) "z"'.match("xyz")
    m[#'prefix]
    m[0]
    m[1]
)

When @rhombus($, ~at rhombus/rx) is followed by and identifier without a
subsequence @colon block, then it is a @deftech{backreference} to a
capture group. The backreference matches input that is exactly the same
as the captured match.

@margin_note_block{Backreferences extend the matching capability of
 regexps beyond the automata-theory definition of regular expressions.
 but backreference support is commonly included in regexp libraries,
 anyway.}

@examples(
  ~eval: rx_eval
  ~repl:
    rx'($prefix: "a"+ "b"+ || "x"+ "y"+) $prefix'.match("xyxy")
    rx'($prefix: "a"+ "b"+ || "x"+ "y"+) $prefix'.match("xyxxy")
)

The @rhombus($, ~at rhombus/rx) operator support backreferences by
integer index, too. The @rhombus(~~, ~at rhombus/rx) operator creates an
anonymous capture group that can only be referenced by index.

The @rhombus($, ~at rhombus/rx) operator supports one more overloading:
when not followed by an identifier or immediate integer, it serves as an
escape back to Rhombus expressions. The result of the expression must be
a @tech{regexp} object that can be spliced into the enclosing pattern.

@examples(
  ~eval: rx_eval
  ~defn:
    def num_rx = rx'["1"-"9"]* ["0"-"9"]'
  ~repl:    
    rx'$num_rx " through " $num_rx'.match("1 through 99")
    rx'$num_rx ", " $num_rx ", and " $num_rx'.match("1, 10, and 100")
)

We've only touched on a few of the regexp pattern and character set
operators provided by @rhombusmodname(rhombus/rx).
@Secref("rx-quickref"), provides a quick reference to the full set of
exported operators. New operators can be defined outside of
@rhombusmodname(rhombus/rx) using @rhombus(rx.macro) or
@rhombus(rx_charset.macro).
