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

@title(~tag: "rx-pattern"){Regexp Patterns}

@margin_note_block{See also @secref(~doc: guide_doc, "rx-quickref").}

The portion of a @rhombus(rx) or @rhombus(rx_in) form within @quotes is
a pattern that is written with regexp pattern operators. Some pattern
operators overlap with expression operators, but they have different
meanings and precedence in a pattern. For example, the pattern operator
@rhombus(*, ~at rhombus/rx) creates a repetition pattern, instead of
multiplying like the expression @rhombus(*) operator.

@doc(
  space.enforest rx
){

 The @tech(~doc: meta_doc){space} for pattern operators that can be used within
 @rhombus(rx) and @rhombus(rx_in) forms.

}


@doc(
  rx.macro '#%literal $string'
  rx.macro '#%literal $bytes'
){

 A literal @tech{string} or @tech{byte string} can be used as a pattern.
 It matches the string's characters or bytes literally. See also
 @rhombus(case_insensitive, ~at rhombus/rx).

@examples(
  ~eval: rx_eval
  rx'"hello"'.match("hello")
  rx'"hello"'.match("olleh")
  rx'#"a"'.match(#"a")
)

}


@doc(
  rx.macro '$pat #%juxtapose $pat'
  rx.macro '$pat ++ $pat'
  rx.macro '$pat #%call ($pat)'
  operator_order:
    ~order: rx_concatenation
){

 Patterns that are adjacent in a larger pattern match in sequence. The
 @rhombus(++, ~at #'rhombus/rx) operator can be used to make sequencing
 explicit. An implicit @rhombus(#%call, ~at rhombus/rx) form is treated
 like @rhombus(#%juxtapose, ~at rhombus/rx), consistent with implicit
 uses of parentheses for grouping as handled by
 @rhombus(#%parens, ~at rhombus/rx).

@examples(
  ~eval: rx_eval
  rx'"hello" " " "world"'.match("hello world")
  rx'"hello" ++ " " ++ "world"'.match("hello world")
  rx'"hello"
       ++ " "
       ++ "world"'.match("hello world")
)

}


@doc(
  rx.macro '$pat || $pat'
  operator_order:
    ~order: rx_disjunction
){

 Matches as either the first @rhombus(pat) or second @rhombus(pat). The
 first @rhombus(pat) is tried first.

@examples(
  ~eval: rx_eval
  rx'"a" || "b"'.match("a")
  rx'"a" || "b"'.match("b")
  rx'"a" || "b"'.match("c")
)

}


@doc(
  rx.macro '#%parens ($pat)'
  operator_order:
    ~order: rx_concatenation
){

 A parenthesized pattern is equivalent to the @rhombus(pat) inside the
 parentheses. That is, parentheses are just for grouping and resolving
 precedence mismatches. See @rhombus($, ~at rhombus/rx) for information
 about @tech{capture groups}, which are not implicitly created by
 parentheses (as they are in some traditional regexp languages).

@examples(
  ~eval: rx_eval
  rx'"a" || "b" ++ "c"'.match("ac")
  rx'("a" || "b") ++ "c"'.match("ac")
)

}


@doc(
  rx.macro '#%brackets [$charset]'
  rx.macro '$pat #%index [$charset]'
  operator_order:
    ~order: rx_concatenation
){

 A @brackets pattern, which is an implicit use of
 @rhombus(#%brackets, ~at rhombus/rx), matches a single character or
 byte, where @rhombus(charset) determines the matching characters or
 bytes. An implicit @rhombus(#%index, ~at rhombus/rx) form (see
 @secref("implicit")) is treated as a sequence of a @rhombus(pat) and
 @rhombus(#%brackets, ~at rhombus/rx).

 See @secref("rx-charset") for @tech{character set} forms that can be
 used in @rhombus(charset).

@examples(
  ~eval: rx_eval
  rx'["a"-"z"]'.match("m")
  rx'["a"-"z"]'.match("0")
)

}


@doc(
  rx.macro '$pat *'
  rx.macro '$pat * $mode'
  operator_order:
    ~order: rx_repetition
  grammar mode
  | ~greedy
  | ~nongreedy
  | ~possessive
){

 Matches a sequence of 0 or more matches to @rhombus(pat).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'any*'.match("abc")
    rx'any*'.match("")
)

 By default, the match uses @rhombus(~greedy) mode, where a larger number
 of matches is tried first---but subsequent patterns may cause
 backtracking to a shorter match. In @rhombus(~nongreedy) mode, shorter
 matches are tried first. The @rhombus(~possessive) mode is like
 @rhombus(~greedy), but without backtracking (i.e., the longest match
 must succeed overall for the enclosing pattern); see also
 @rhombus(cut, ~at rhombus/rx).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'($head: any*) ($tail: any*)'.match("abc")
    rx'($head: any* ~nongreedy) ($tail: any*)'.match("abc")
  ~repl:
    rx'any* ~greedy "z"'.match("abcz")
    rx'any* ~possessive "z"'.match("abcz")
)

}

@doc(
  ~nonterminal:
    mode: * ~at rhombus/rx
  rx.macro '$pat +'
  rx.macro '$pat + $mode'
  operator_order:
    ~order: rx_repetition
){

 Like @rhombus(*, ~at rhombus/rx), but matches 1 or more instances of @rhombus(pat).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'any+'.match("abc")
    rx'any+'.match("")
)

}


@doc(
  ~nonterminal:
    mode: * ~at rhombus/rx
  rx.macro '$pat ?'
  rx.macro '$pat ? $mode'
  operator_order:
    ~order: rx_repetition
){

 Similar to @rhombus(*, ~at rhombus/rx), but matches 0 or 1 instances of @rhombus(pat).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'any?'.match("a")
    rx'any?'.match("")
    rx'any?'.match("abc")
)

}


@doc(
  rx.macro '$pat #%comp {$count}'
  rx.macro '$pat #%comp {$min #,(@rhombus(.., ~at rhombus/rx))}'
  rx.macro '$pat #%comp {$min #,(@rhombus(..=, ~at rhombus/rx)) $max}'
  operator_order:
    ~order: rx_repetition
){

 Using @braces after a pattern, which is use of the implicit
 @rhombus(#%comp, ~at rhombus/rx) form, specifies a repetition like
 @rhombus(*, ~at rhombus/rx) or @rhombus(+, ~at rhombus/rx) more
 generally. If a single @rhombus(count) is provided, it specifies an
 exact number of repetitions. If just @rhombus(min) is provided, then it
 specifies a minimum number of repetitions, and there is no maximum.
 Finally, @rhombus(min) and @rhombus(max) both can be specified.
 @margin_note{Write @rhombus(0 #,(@rhombus(..=, ~at rhombus/rx)) max) to provide only an upper bound.
  Note that the expression form @rhombus(..= max) creates a range that
  starts at @rhombus(#neginf), and the intent of requiring a @rhombus(min) for
  a regexp repetition is to avoid suggesting that negative counts are
  possible.} A @rhombus(count), @rhombus(min), or @rhombus(max) must be a
 literal nonnegative integer.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'any{2}'.match("aa")
    rx'any{2}'.match("aaa")
  ~repl:
    rx'any{2..}'.match("aa")
    rx'any{2..}'.match("aaa")
  ~repl:
    rx'any{2..=3}'.match("aa")
    rx'any{2..=3}'.match("aaa")
    rx'any{2..=3}'.match("aaaa")
)

}

@doc(
  rx.macro '..'
  rx.macro '..='
){

 Only allowed within a @braces repetition form.

}


@doc(
  rx.macro '.'
  rx.macro 'any'
  rx.macro 'char'
  rx.macro 'byte'
){

 Matches a single character or byte. The @rhombus(., ~at rhombus/rx)
 pattern matches any character or byte except a newline, while
 @rhombus(any, ~at rhombus/rx) also matches a newline. The
 @rhombus(char, ~at rhombus/rx) and @rhombus(byte, ~at rhombus/rx) forms
 are like @rhombus(any, ~at rhombus/rx) and also imply that that the
 enclosing @tech{regexp} matches strings or byte strings, respectively.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'.'.match("a")
    rx'.'.match("\n")
    rx'any'.match("\n")
  ~repl:
    rx'char'.match("\n")
    rx'byte'.match("\n")
)

}

@doc(
  ~nonterminal:
    mode: * ~at rhombus/rx
  rx.macro '.*'
  rx.macro '.* $mode'
  rx.macro '.+'
  rx.macro '.+ $mode'
  rx.macro '.?'
  rx.macro '.? $mode'
){

 Equivalent to @rhombus(. *, ~at rhombus/rx), @rhombus(. +, ~at rhombus/rx), and
 @rhombus(. ?, ~at rhombus/rx), but allowing the space between the
 operators to be omitted.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'.*'.match("abc")
)

}

@doc(
  rx.macro 'bof'
  rx.macro 'bol'
){

 Matches the start of input with @rhombus(bof, ~at rhombus/rx) or the
 position after a newline with @rhombus(bol, ~at rhombus/rx).

 A regexp created with @rhombus(rx) (as opposed to @rhombus(rx_in)) is
 implicitly prefixed with @rhombus(bof) for use with methods like
 @rhombus(Regexp.match) (as opposed to @rhombus(Regexp.match_in)).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'bof "a"'.match_in("a")
    rx'bol "a"'.match_in("x\na")
    rx'bof "a"'.match_in("x\na")
)

}

@doc(
  rx.macro 'eof'
  rx.macro 'eol'
){

 Matches the end of input with @rhombus(eof, ~at rhombus/rx) or the
 position before a newline with @rhombus(eol, ~at rhombus/rx).

 A regexp created with @rhombus(rx) (as opposed to @rhombus(rx_in)) is
 implicitly suffixed with @rhombus(eof) for use with methods like
 @rhombus(Regexp.match) (as opposed to @rhombus(Regexp.match_in)).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'"a" eof'.match_in("a")
    rx'"a" eol'.match_in("a\nx")
    rx'"a" eof'.match_in("a\nx")
)

}

@doc(
  rx.macro '$ $identifier: $pat'
  rx.macro '$ $identifier'
  rx.macro '$ $int'
  rx.macro '$ $expr'
){

 The @rhombus($, ~at rhombus/rx) operator is overloaded for related
 uses:

@itemlist(

 @item{When followed by an identifier and a @colon for a block
  containing @rhombus(pat), @rhombus($, ~at rhombus/rx) creates a
  @deftech{capture group}. The portion of input that is matched against
  @rhombus(pat) is recorded and associated with the name
  @rhombus(identifier). If the enclosing pattern uses @rhombus(pat) zero
  or multiple times, then @rhombus(identifier) is associated to
  @rhombus(#false) if the pattern is used zero times, or it is associated
  to the latest match if used multiple times.

  @examples(
  ~eval: rx_eval
  ~repl:
    rx'any ($m: any)'.match("ab")
    rx'any ($m: any)'.match("ab")[#'m]
    rx'any ($m: any)*'.match("a")
  ~repl:
    def rx'any ($m: any)' = "ab"
    m
  )}

 @item{When followed by an identifier and no subsequent block, then
  @rhombus($, ~at rhombus/rx) is either a @deftech{backreference} to a
  named @tech{capture group}, or it is a @deftech{splice} of a regexp that
  is bound to @rhombus(identifier).

  The use of @rhombus($, ~at rhombus/rx) forms a backreference if
  @rhombus(identifier) is associated to a capture group anywhere in the
  enclosing pattern; the backreference matches input that is the same as
  the most recent match for the capture group (and never matches if the
  capture group does not yet have a match).

  @examples(
  ~eval: rx_eval
  ~repl:
    rx'any ($m: any) $m'.match("abb")
    rx'any ($m: any) $m'.match("abc")
  )

  When @rhombus($, ~at rhombus/rx) forms a splice, then a regular
  expression is formed dynamically by merging the referenced regexp into
  the enclosing pattern. A limitation: both the merged regexp and
  enclosing pattern must be free of backreferences, because backreferences
  need to be converted from names to absolute positions eagerly.
  Spliced regular expressions and non-spliced patterns must consistently
  imply string or byte string matching.

  @examples(
  ~eval: rx_eval
  ~defn:
    fun labeled(key) :: RX:
      rx'$key ": " $name: .*'
  ~repl:
    labeled(rx'"fruit"').match("fruit: apple")
    labeled(rx'"veggie"').match("veggie: carrot")
  )}

 @item{When followed by a literal integer, then
  @rhombus($, ~at rhombus/rx) forms a @tech{backreference} that refers
  to a @tech{capture group} by index instead of by name. Capture groups
  are numbered from 1, since 0 is reserved to refer to the entire match.

  @examples(
  ~eval: rx_eval
  ~repl:
    rx'any ($m: any) $1'.match("abb")
    rx'any ($m: any) $1'.match("abc")
  )}

 @item{When followed by an expression other than an identifier or
  literal integer, then @rhombus($, ~at rhombus/rx) always forms a
  @tech{splice}.}

)

}

@doc(
  rx.macro '~~ $pat'
){

 Matches @rhombus(pat) as an unnamed @tech{capture group}. The capture
 group's match can only be referenced by index (counting from 1).

@examples(
  ~eval: rx_eval
  ~repl:
    rx'any ~~any any*'.match("abc")[1]
    rx'any ~~any $1'.match("abb")
)

}


@doc(
  rx.macro 'lookahead($pat)'
  rx.macro 'lookbehind($pat)'
  rx.macro '! #,(@rhombus(lookahead, ~at rhombus/rx))($pat)'
  rx.macro '! #,(@rhombus(lookbehind, ~at rhombus/rx))($pat)'
){

 Matches an empty position in the input where the subsequent (for
 @rhombus(lookahead, ~at rhombus/rx)) or preceding (for
 @rhombus(lookbehind, ~at rhombus/rx)) input matches @rhombus(pat)---or
 does not match, when a @rhombus(!, ~at rhombus/rx) prefix is used.

@examples(
  ~eval: rx_eval
  rx'. "a" lookahead("p")'.match_in("cat nap")
  rx'. "a" !lookahead("t")'.match_in("cat nap")
  rx'lookbehind("n") "a" .'.match_in("cat nap")
  rx'!lookbehind("c") "a" .'.match_in("cat nap")
)
}


@doc(
  rx.macro 'word_boundary'
  rx.macro 'word_continue'
){

 Matches an empty position in the input. The
 @rhombus(word_boundary, ~at rhombus/rx) pattern matches between an
 alphanumeric ASCII character (@litchar{a}-@litchar{z},
 @litchar{A}-@litchar{Z}, or @litchar{0}-@litchar{9}) or @litchar{_} and
 another character that is not alphanumeric or @litchar{_}. The
 @rhombus(word_continue, ~at rhombus/rx) pattern matches positions that
 do not match @rhombus(word_boundary, ~at rhombus/rx).

@examples(
  ~eval: rx_eval
  rx'any+ ~nongreedy word_boundary'.match_in("cat nap")
  rx'any+ ~nongreedy word_continue'.match_in("cat nap")
)

}


@doc(
  rx.macro 'if #,(@rhombus(lookahead, ~at rhombus/rx))($pat) | $then_pat | $else_pat'
  rx.macro 'if #,(@rhombus(lookbehind, ~at rhombus/rx))($pat) | $then_pat | $else_pat'
  rx.macro 'if #,(@rhombus(!, ~at rhombus/rx)) #,(@rhombus(lookahead, ~at rhombus/rx))($pat) | $then_pat | $else_pat'
  rx.macro 'if #,(@rhombus(!, ~at rhombus/rx)) #,(@rhombus(lookbehind, ~at rhombus/rx))($pat) | $then_pat | $else_pat'
  rx.macro 'if #,(rhombus($, ~at rhombus/rx)) $identifier | $then_pat | $else_pat'
  rx.macro 'if #,(rhombus($, ~at rhombus/rx)) $int | $then_pat | $else_pat'
){

 Matches as @rhombus(then_pat) or @rhombus(else_pat), depending on the
 form immediately after @rhombus(if), which must be either a
 @rhombus(lookahead, ~at rhombus/rx), @rhombus(lookbehind, ~at rhombus/rx),
 or @tech{backreference} pattern.

@examples(
  ~eval: rx_eval
  rx'($x: "x")* if $x | "s" | "."'.match_in("xxxs")
  rx'($x: "x")* if $x | "s" | "."'.match_in(".")
)

}

@doc(
  rx.macro 'cut'
){

 Matches an empty position in the input. The first potential match that
 reaches @rhombus(cut, ~at rhombus/rx) is the only one that is allowed to
 succeed. Note that a possessive repetition mode like
 @rhombus(* ~possessive, ~at rhombus/rx) is equivalent to using
 @rhombus(cut, ~at rhombus/rx) after the repetition.

 In the case of a @rhombus(rx_in) pattern or use of
 @rhombus(RX.match_in), @rhombus(cut, ~at rhombus/rx) applies only to a
 match attempt at a given input position. It does not prevent trying the
 match at a later position.

@examples(
  ~eval: rx_eval
  rx'("ax" || "a") cut "x"'.match("ax")
  rx'("a" || "ax") cut "x"'.match("ax")
)

}

@doc(
  rx.macro 'bytes: $pat'
  rx.macro 'string: $pat'
){

 Matches he same as @rhombus(pat), but specifies explicitly either
 byte-string mode or string mode.

@examples(
  ~eval: rx_eval
  rx'string: "a"'.match("a")
  rx'bytes: "a"'.match("a")
  rx'string: any'.match(#"\x80") // not UTF-8
  rx'bytes: any'.match(#"\x80")
)

}


@doc(
  rx.macro 'case_sensitive: $pat'
  rx.macro 'case_insensitive: $pat'
){

 Adjusts the treatment of literal strings and ranges in @rhombus(pat) to
 match case-sensitive (the default) or case-insensitive. In
 case-insensitive mode, characters are folded individually (as opposed for
 folding a string sequence, which can change its length).

@examples(
  ~eval: rx_eval
  rx'"hello"'.match("HELLO")
  rx'case_insensitive: "hello"'.match("HELLO")
)

}

@doc(
  rx.macro 'alpha'
  rx.macro 'upper'
  rx.macro 'lower'
  rx.macro 'digit'
  rx.macro 'xdigit'
  rx.macro 'alnum'
  rx.macro 'word'
  rx.macro 'blank'
  rx.macro 'newline'
  rx.macro 'space'
  rx.macro 'graph'
  rx.macro 'print'
  rx.macro 'cntrl'
  rx.macro 'ascii'
  rx.macro 'latin1'
  rx.macro 'unicode.Ll'
  rx.macro 'unicode.Lu'
  rx.macro 'unicode.Lt'
  rx.macro 'unicode.Lm'
  rx.macro 'unicode.Lx'
  rx.macro 'unicode.Lo'
  rx.macro 'unicode.L'
  rx.macro 'unicode.Nd'
  rx.macro 'unicode.Nl'
  rx.macro 'unicode.No'
  rx.macro 'unicode.N'
  rx.macro 'unicode.Ps'
  rx.macro 'unicode.Pe'
  rx.macro 'unicode.Pi'
  rx.macro 'unicode.Pf'
  rx.macro 'unicode.Pc'
  rx.macro 'unicode.Pd'
  rx.macro 'unicode.Po'
  rx.macro 'unicode.P'
  rx.macro 'unicode.Mn'
  rx.macro 'unicode.Mc'
  rx.macro 'unicode.Me'
  rx.macro 'unicode.M'
  rx.macro 'unicode.Sc'
  rx.macro 'unicode.Sk'
  rx.macro 'unicode.Sm'
  rx.macro 'unicode.So'
  rx.macro 'unicode.S'
  rx.macro 'unicode.Zl'
  rx.macro 'unicode.Zp'
  rx.macro 'unicode.Zs'
  rx.macro 'unicode.Z'
  rx.macro 'unicode.Cc'
  rx.macro 'unicode.Cf'
  rx.macro 'unicode.Cs'
  rx.macro 'unicode.Cn'
  rx.macro 'unicode.Co'
  rx.macro 'unicode.C'
){

 Each of these names is bound both as a @tech{character set} and as a
 pattern that can be used directly, instead of wrapping in @brackets. See
 the @rhombus(alpha, ~at rhombus/rx_charset), etc., character set for
 more information.

@examples(
  ~eval: rx_eval
  rx'alpha'.match("m")
  rx'alpha'.match("0")
)

}

@doc(
  operator_order.def rx_repetition
  operator_order.def rx_subtraction
  operator_order.def rx_enumeration
  operator_order.def rx_conjunction:
    ~weaker_than:
      rx_repetition
      rx_enumeration
  operator_order.def rx_disjunction:
    ~weaker_than:
      rx_conjunction
      rx_repetition
      rx_enumeration
  operator_order.def rx_negation:
    ~weaker_than:
      ~other
  operator_order.def rx_concatenation:
    ~weaker_than:
      ~other
    ~stronger_than:
      rx_conjunction
      rx_disjunction
      rx_negation
){

 @tech(~doc: meta_doc){Operator orders} for @tech{regexp} and @tech{character set}
 operators.

}

@doc(
  ~meta
  def rx_meta.space :: SpaceMeta
){

 A compile-time value that identifies the same space as
 @rhombus(rx, ~space). See also @rhombus(SpaceMeta, ~annot).

}


@doc(
  ~nonterminal:
    macro_patterns: expr.macro ~defn
  defn.macro 'rx.macro macro_patterns'
){

 Like @rhombus(expr.macro, ~defn), but defines a new @tech{regexp} operator.

@examples(
  ~eval: rx_eval
  ~defn:
    // match number from 0 to 10**n-1:
    rx.macro 'upto_e($(n :: Int))':
      let n = n.unwrap()
      if n == 1
      | 'digit'
      | '["1"-"9"] digit{$(n-1)} || upto_e($(n-1))'
    rx.macro 'pct':
      '("100" || upto_e(2)) "%"'
  ~repl:
    rx'pct "/" pct "/" pct'.is_match("1%/42%/100%")
)

}


@doc(
  ~meta
  syntax_class rx_meta.Parsed
  syntax_class rx_meta.AfterPrefixParsed(name :: Name)
  syntax_class rx_meta.AfterInfixParsed(name :: Name)
){

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), etc., but for
 regexp patterns.

}


@close_eval(rx_eval)
