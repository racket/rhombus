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
    import:
      rhombus/rx open
      rhombus/meta open
)

@title(~tag: "rx-charset"){Regexp Character Sets}

A @deftech{character set} is written with @brackets in a @{regexp}
pattern (via the implicit
@rhombus(#%brackets, ~at rhombus/rx) operator). A character set
represents a set of @rhombus(Char)s, but as long as the characters range
in Unicode value from 0 to 255, a character set can be used as a set of
bytes to match for a byte-mode @tech{regexp}.

@doc(
  space.enforest rx_charset
){

 The @tech{space} for character set operators that can be used within
 @brackets in a regexp pattern.

}


@doc(
  rx_charset.macro '#%literal $string'
  rx_charset.macro '#%literal $bytes'
){

 A literal @tech{string} or @tech{byte string} can be used as a character set.
 Each character or byte is part of the set.

@examples(
  ~eval: rx_eval
  rx'["a"]'.is_match("a")
  rx'["a"]'.is_match("b")
  rx'["abc"]'.is_match("b")
)

}


@doc(
  rx_charset.macro '$charset #%juxtapose $charset'
  rx_charset.macro '$charset || $charset'
  rx_charset.macro '$charset #%call ($charset)'
  operator_order:
    ~order: rx_concatenation
){

 Character sets that are adjacent or joined with
 @rhombus(||, ~at rhombus/rx_charset) form a larger character set that
 includes all combined elements, i.e., a union of the sets. An implicit
 @rhombus(#%call, ~at rhombus/rx_charset) form is treated like
 @rhombus(#%juxtapose, ~at rhombus/rx_charset), consistent with implicit uses of
 parentheses for grouping as handled by
 @rhombus(#%parens, ~at rhombus/rx_charset).

@examples(
  ~eval: rx_eval
  rx'["a" "b"]'.is_match("a")
  rx'["a" "b"]'.is_match("b")
  rx'["a" "b"]'.is_match("c")
  rx'["a" || "b"]'.is_match("b")
)

}


@doc(
  rx_charset.macro '#%parens ($charset)'
  operator_order:
    ~order: rx_concatenation
){

 A parenthesized character set is equivalent to the @rhombus(charset)
 inside the parentheses. That is, parentheses are just for grouping and
 resolving precedence mismatches.

@examples(
  ~eval: rx_eval
  rx'["a" "b" "c"]'.is_match("a")
  rx'[("a" "b") "c"]'.is_match("a")
)

}


@doc(
  rx_charset.macro '$charset - $charset'
  operator_order:
    ~order: rx_enumeration
){

 Assuming that each @rhombus(charset) contains a single character,
 creates a charset that has those two characters and all characters in
 between (based on @rhombus(Char.to_int) values). An error is reported if
 either @rhombus(charset) has zero or multiple characters.

@examples(
  ~eval: rx_eval
  rx'["a" - "y"]'.is_match("a")
  rx'["a" - "y"]'.is_match("x")
  rx'["a" - "y"]'.is_match("z")
)

}


@doc(
  rx_charset.macro '$charset && $charset'
  operator_order:
    ~order: rx_conjunction
){

 Creates a character set that has each character in both the first
 @rhombus(charset) and the second @rhombus(charset), i.e., an
 intersection of the sets.

@examples(
  ~eval: rx_eval
  rx'[("a" - "f") && ("c" - "h")]'.is_match("a")
  rx'[("a" - "f") && ("c" - "h")]'.is_match("d")
)

}



@doc(
  rx_charset.macro '$charset -- $charset'
  operator_order:
    ~order: rx_subtraction
){

 Creates a character set that starts with the character of the first
 @rhombus(charset) and removes each character of the second
 @rhombus(charset), i.e., set difference.

@examples(
  ~eval: rx_eval
  rx'[("a" - "z") -- ("m" - "p")]'.is_match("n")
  rx'[("a" - "z") -- ("m" - "p")]'.is_match("a")
)

}


@doc(
  rx_charset.macro '! $charset'
  operator_order:
    ~weaker_than: ~other
){

 Inverts @rhombus(charset) by creating a character set that has every
 character not in @rhombus(charset).

@examples(
  ~eval: rx_eval
  rx'[! "a" - "z"]'.is_match("n")
  rx'[! "a" - "z"]'.is_match("0")
)

}


@doc(
  rx_charset.macro 'any'
){

 A character set that has all characters.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'[any]'.is_match("a")
)

}

@doc(
  rx_charset.macro 'alpha'
  rx_charset.macro 'upper'
  rx_charset.macro 'lower'
){

 The @rhombus(alpha, ~at rhombus/rx_charset) character set has all ASCII
 letters: @litchar{a}-@litchar{z} and @litchar{A}-@litchar{Z}. The
 @rhombus(upper, ~at rhombus/rx_charset) character set has just
 @litchar{A}-@litchar{Z}, while the
 @rhombus(lower, ~at rhombus/rx_charset) character set has just
 @litchar{a}-@litchar{z}.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'[alpha]'.is_match("a")
    rx'[alpha]'.is_match("0")
    rx'[alpha]'.is_match("λ")
  ~repl:
    rx'[upper]'.is_match("A")
    rx'[upper]'.is_match("a")
  ~repl:
    rx'[lower]'.is_match("a")
    rx'[lower]'.is_match("A")
)

}

@doc(
  rx_charset.macro 'digit'
  rx_charset.macro 'xdigit'
){

 The @rhombus(digit, ~at rhombus/rx_charset) character set has all ASCII
 digits: @litchar{0}-@litchar{9}.
 The @rhombus(xdigit, ~at rhombus/rx_charset) character set adds the remaining hexadecimal digits:
 @litchar{a}-@litchar{f}, and
 @litchar{A}-@litchar{F}.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'[digit]'.is_match("0")
    rx'[digit]'.is_match("a")
  ~repl:
    rx'[xdigit]'.is_match("0")
    rx'[xdigit]'.is_match("a")
    rx'[xdigit]'.is_match("z")
)

}

@doc(
  rx_charset.macro 'alnum'
  rx_charset.macro 'word'
){

 The @rhombus(alnum, ~at rhombus/rx_charset) character set has all ASCII
 letters and digits: @litchar{0}-@litchar{9}, @litchar{a}-@litchar{z},
 and @litchar{A}-@litchar{Z}. The @rhombus(word, ~at rhombus/rx_charset)
 character set adds @litchar{_}.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'[alnum]'.is_match("0")
    rx'[alnum]'.is_match("z")
    rx'[alnum]'.is_match("_")
    rx'[word]'.is_match("_")
)

}


@doc(
  rx_charset.macro 'newline'
  rx_charset.macro 'blank'
  rx_charset.macro 'space'
){

 The @rhombus(newline, ~at rhombus/rx_charset) character set has just
 the newline character (@rhombus(Char.to_int) value 10). The
 @rhombus(blank, ~at rhombus/rx_charset) character set has space
 (@rhombus(Char.to_int) value 32) and tab (@rhombus(Char.to_int) value
 7). The @rhombus(space, ~at rhombus/rx_charset) character set combines
 those and adds return (@rhombus(Char.to_int) value 10) and form feed
 (@rhombus(Char.to_int) value 12).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'[blank]'.is_match(" ")
)

}


@doc(
  rx_charset.macro 'graph'
  rx_charset.macro 'print'
){

 The @rhombus(graph, ~at rhombus/rx_charset) character set has all ASCII
 characters that print with ink. The
 @rhombus(print, ~at rhombus/rx_charset) character set adds space
 (@rhombus(Char.to_int) value 32) and tab (@rhombus(Char.to_int).
}


@doc(
  rx_charset.macro 'cntrl'
){

 All ASCII control characters (@rhombus(Char.to_int) values 0 through
 31).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'[cntrl]'.is_match("\n")
    rx'[cntrl]'.is_match("a")
)

}


@doc(
  rx_charset.macro 'ascii'
  rx_charset.macro 'latin1'
){

 The @rhombus(ascii, ~at rhombus/rx_charset) character set has all ASCII
 characters (@rhombus(Char.to_int) values 0 through 127), and the
 @rhombus(latin1, ~at rhombus/rx_charset) character set has all Latin-1
 characters (@rhombus(Char.to_int) 0 through 255).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'[ascii]'.is_match("a")
    rx'[ascii]'.is_match("é")
    rx'[latin1]'.is_match("é")
    rx'[latin1]'.is_match("λ")
)

}


@doc(
  rx_charset.macro 'unicode.Ll'
  rx_charset.macro 'unicode.Lu'
  rx_charset.macro 'unicode.Lt'
  rx_charset.macro 'unicode.Lm'
  rx_charset.macro 'unicode.Lx'
  rx_charset.macro 'unicode.Lo'
  rx_charset.macro 'unicode.L'
  rx_charset.macro 'unicode.Nd'
  rx_charset.macro 'unicode.Nl'
  rx_charset.macro 'unicode.No'
  rx_charset.macro 'unicode.N'
  rx_charset.macro 'unicode.Ps'
  rx_charset.macro 'unicode.Pe'
  rx_charset.macro 'unicode.Pi'
  rx_charset.macro 'unicode.Pf'
  rx_charset.macro 'unicode.Pc'
  rx_charset.macro 'unicode.Pd'
  rx_charset.macro 'unicode.Po'
  rx_charset.macro 'unicode.P'
  rx_charset.macro 'unicode.Mn'
  rx_charset.macro 'unicode.Mc'
  rx_charset.macro 'unicode.Me'
  rx_charset.macro 'unicode.M'
  rx_charset.macro 'unicode.Sc'
  rx_charset.macro 'unicode.Sk'
  rx_charset.macro 'unicode.Sm'
  rx_charset.macro 'unicode.So'
  rx_charset.macro 'unicode.S'
  rx_charset.macro 'unicode.Zl'
  rx_charset.macro 'unicode.Zp'
  rx_charset.macro 'unicode.Zs'
  rx_charset.macro 'unicode.Z'
  rx_charset.macro 'unicode.Cc'
  rx_charset.macro 'unicode.Cf'
  rx_charset.macro 'unicode.Cs'
  rx_charset.macro 'unicode.Cn'
  rx_charset.macro 'unicode.Co'
  rx_charset.macro 'unicode.C'
){

 Each of these character sets contains all Unicode characters that have
 the named general category, such as Ll for lowercase letters. Each
 single-letter name, such as @rhombus(unicode.L, ~at rhombus/rx_charset),
 unions all of the other general categories that start with the same
 letter. The @rhombus(unicode.Lx, ~at rhombus/rx_charset) character set
 unions @rhombus(unicode.Ll, ~at rhombus/rx_charset),
 @rhombus(unicode.Lu, ~at rhombus/rx_charset),
 @rhombus(unicode.Lt, ~at rhombus/rx_charset), and
 @rhombus(unicode.Lm, ~at rhombus/rx_charset).

@examples(
  ~eval: rx_eval
  rx'[unicode.Ll]'.is_match("λ")
)

}


@doc(
  ~meta
  def rx_charset_meta.space :: SpaceMeta
){

 A compile-time value that identifies the same space as
 @rhombus(rx_charset, ~space). See also @rhombus(SpaceMeta, ~annot).

}


@doc(
  ~nonterminal:
    macro_patterns: expr.macro ~defn
  defn.macro 'rx_charset.macro macro_patterns'
){

 Like @rhombus(expr.macro, ~defn), but defines a @tech{character set}
 operator.

@examples(
  ~eval: rx_eval
  ~defn:
    rx_charset.macro 'octal': '"0"-"7"'
    rx_charset.macro 'maybe $charset': '$charset "?"'
  ~repl:
    rx'[maybe(octal) "!"]*'.match("3?!4")
    rx'[maybe(octal)]'.match("8")
)

}

@doc(
  ~meta
  syntax_class rx_charset_meta.Parsed
  syntax_class rx_charset_meta.AfterPrefixParsed(name :: Name)
  syntax_class rx_charset_meta.AfterInfixParsed(name :: Name)
){

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), etc., but for
 regexp character ranges.

}


@close_eval(rx_eval)
