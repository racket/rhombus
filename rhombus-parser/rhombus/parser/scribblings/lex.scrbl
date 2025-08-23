#lang rhombus/scribble/manual
@(import:
    "lex_doc.rhm" open
    meta_label:
      rhombus/meta open
      rhombus open
      parser/lex open
      rhombus/rx:
        expose:
          rx
          rx_charset
          rx_concatenation
          rx_disjunction
          rx_conjunction
          rx_subtraction)

@(def ref_doc = ModulePath 'lib("rhombus/scribblings/reference/rhombus-reference.scrbl")')

@(def lex_eval = make_rhombus_eval())
@examples(
  ~eval: lex_eval
  ~hidden:
    import:
      parser/lex open
      rhombus/meta open
)

@title(~tag: "lex"){Lexers for Token Parsing}

@docmodule(parser/lex)

A @deftech{lexer} tokenizes an input stream using regular expressions.
The @rhombus(lexer) form creates a lexer based on a set of
regular-expression @deftech{lexer patterns} and an action for each
pattern to produce its result.

The lexer pattern language of @rhombus(lexer) resembles the pattern
language of @rhombus(rx), but it is different in some ways, because the
matching engine implementing @rhombus(lexer) is tuned for tokenization:
it finds the longest match among a set of regular expression, and it
uses a different algorithm than @rhombus(rx) internally to find that
match. Still, the same syntax is used as much as possible, and the
@rhombus(rx_charset, ~space) space is used directly to express
@tech(~doc: ref_doc){character sets}.

@doc(
  expr.macro 'lexer $maybe_option
              | $trigger:
                  $body
                  ...
              | ...'
  grammar trigger:
    $pat
    ~eof
  grammar maybe_option:
    ϵ
    : ~allow_empty
){

 Returns a @tech{lexer} as a function
 @rhombus(Port.Input -> Any, ~annot) that takes an
 @rhombus(input_port, ~var) and returns the result of one of the
 @rhombus(body) sequences:

@itemlist(

  @item{If reading from @rhombus(input_port, ~var) immediately produces
  @rhombus(Port.eof), then the @rhombus(body) whose @rhombus(trigger) is
  @rhombus(~eof) is evaluated. If there is no such @rhombus(trigger) among
  the @rhombus(lexer) clauses, @rhombus(Port.eof) is returned. The port's
  @rhombus(Port.eof) is consumed. At most one @rhombus(trigger) can be
  @rhombus(~eof).}

  @item{If reading some number of characters from
  @rhombus(input_port, ~var) matches a @rhombus(trigger) as @rhombus(pat),
  and if it is either the longest such match or the first among
  @rhombus(pat)s that match the same input, then then corresponding
  @rhombus(body) result is produced. If reading from
  @rhombus(input_port, ~var) produces an immediate end-of-file, no
  empty-string @rhombus(pat) matches are attemped, and a @rhombus(~eof)
  clause (if any) is used, instead. The matched characters from
  @rhombus(input_port, ~var) are consumed, and only those characters are
  consumed.}

  @item{If reading some number of characters from
  @rhombus(input_port, ~var) matches no @rhombus(trigger), then an
  exception is thrown after a unspecified number of characters are
  consumed.}

)

 If any @rhombus(pat) as a @rhombus(trigger) could match an empty
 string, a syntax error is reported, unless the @rhombus(~allow_empty)
 option is present.

 A @rhombus(body) sequence can use @rhombus(lexeme) to refer to the
 matched string (except in an @rhombus(~eof) clause), and it can use
 @rhombus(line), @rhombus(column), @rhombus(position), @rhombus(span),
 @rhombus(srcloc), @rhombus(line_span), and/or @rhombus(end_column) for
 location information relative to @rhombus(input_port, ~var) for the read
 @rhombus(lexeme); note that line and column counting need to have been
 enabled for the port via @rhombus(Port.locations_enabled). The
 @rhombus(input_port) form can be used to refer to the
 @rhombus(input_port, ~var) provided to the lexer, which is useful for
 recursive parsing using another lexer or some other parser.

 A @rhombus(pat) is a @tech{lexer pattern} whose operators are bound in
 the @rhombus(lex_pattern, ~space) space. A literal string matches the
 same string as input (via
 @rhombus(#%literal, ~at rhombus/parser/lex_pattern)), operators like
 @rhombus(*, ~at rhombus/parser/lex_pattern) and
 @rhombus(+, ~at rhombus/parser/lex_pattern) support repetitions,
 character sets are support in @rhombus([]) (via
 @rhombus(#%brackets, ~at rhombus/parser/lex_pattern)), adjacent patterns
 are treated as concatenation (via
 @rhombus(#%juxtapose, ~at rhombus/parser/lex_pattern)), and so on.
 Reusable shorthands and new pattern forms can be defined with
 @rhombus(lex_pattern.macro).

@examples(
  ~eval: lex_eval
  def lex:
    lexer
    | "he" ["l" "L"]+ "o": [lexeme, position, span]
    | ~eof: #'done
  def i = Port.Input.open_string("heLlo")
  lex(i)
  lex(i)
)

}

@doc(
  expr.macro 'lexeme'
  expr.macro 'line'
  expr.macro 'column'
  expr.macro 'position'
  expr.macro 'span'
  expr.macro 'srcloc'
  expr.macro 'line_span'
  expr.macro 'end_column'
  expr.macro 'input_port'
){

 Thexe identifiers are for use within a @rhombus(lexer) clause's
 @rhombus(body, ~var) sequence, and using them elsewhere is a syntax
 error. They provide information about the match that reached the
 @rhombus(body, ~var) sequence of a lexer clause.

@itemlist(

  @item{@rhombus(lexeme :: String): the matched input string, not
  allowed in a @rhombus(~eof) clause.}

  @item{@rhombus(line :: maybe(PosInt)): the line number within the
  input for the start of @rhombus(lexeme), assuming that the input port
  has location counting enabled.}

  @item{@rhombus(column :: maybe(NonnegInt)): the column within the
  input for the start of @rhombus(lexeme), assuming that the input port
  has location counting enabled.}

  @item{@rhombus(position :: maybe(PosInt)): the position within the
  input for the start of @rhombus(lexeme), which is normally available
  even without location counting enabled (in which case it's a byte count,
  instead of a character count).}

  @item{@rhombus(span :: maybe(NonnegInt)): the length of
  @rhombus(lexeme) in characters or bytes, depending on whether location
  counting is enabled.}

  @item{@rhombus(line_span :: maybe(NonnegInt)): difference between the
  ending and starting lines for @rhombus(lexeme) if location counting is
  enabled.}

  @item{@rhombus(end_column :: maybe(NonnegInt)): the column for the end
  of @rhombus(lexeme) if location counting is enabled.}

  @item{@rhombus(input_port :: Port.Input): the input port provided to
  the lexer for the current call.}

)

}

@doc(
  space.enforest lex_pattern
){

 The space for @tech{lexer pattern} operators that can be used within
 @rhombus(lexer) forms.

}


@doc(
  lex_pattern.macro '#%literal $string'
  operator_order:
    ~stronger_than: ~other
){

 A literal string as a pattern matches the string's characters literally.

@examples(
  ~eval: lex_eval
  def lex:
    lexer
    | "hello": #'hi
    | "bye": #'bye
  def i = Port.Input.open_string("hellobye")
  lex(i)
  lex(i)
)

}


@doc(
  lex_pattern.macro '$pat #%juxtapose $pat'
  lex_pattern.macro '$pat ++ $pat'
  lex_pattern.macro '$pat #%call ($pat)'
  operator_order:
    ~order: rx_concatenation
){

 Patterns that are adjacent in a larger pattern match in sequence. The
 @rhombus(++, ~at rhombus/parser/lex_pattern) operator can be used to
 make sequencing explicit. An implicit
 @rhombus(#%call, ~at rhombus/parser/lex_pattern) form is treated like
 @rhombus(#%juxtapose, ~at rhombus/parser/lex_pattern), consistent with
 implicit uses of parentheses for grouping as handled by
 @rhombus(#%parens, ~at rhombus/parser/lex_pattern).

@examples(
  ~eval: lex_eval
  def lex:
    lexer
    | "hel" "lo": #'hi
    | "b" ++ "y" ("e"): #'bye
  def i = Port.Input.open_string("hellobye")
  lex(i)
  lex(i)
)

}


@doc(
  lex_pattern.macro '$pat || $pat'
  operator_order:
    ~order: rx_disjunction
){

 Matches the union of charater sequences matched by the first @rhombus(pat) and
 second @rhombus(pat).

@examples(
  ~eval: lex_eval
  def lex:
    lexer
    | "hello" || "bye": lexeme
  def i = Port.Input.open_string("hellobye")
  lex(i)
  lex(i)
)

}


@doc(
  lex_pattern.macro '$pat && $pat'
  operator_order:
    ~order: rx_conjunction
){

 Matches the intersection of charater sequences matched by the first
 @rhombus(pat) and second @rhombus(pat).

}

@doc(
  lex_pattern.macro '$pat - $pat'
  operator_order:
    ~order: rx_subtraction
){

 Matches the charater sequences matched by the first @rhombus(pat) that
 are not also matched by the second @rhombus(pat).

}

@doc(
  lex_pattern.macro '! $pat'
){

 Matches the charater sequences that are @emph{not} matched by
 @rhombus(pat).

}

@doc(
  lex_pattern.macro '#%parens ($pat)'
  operator_order:
    ~order: rx_concatenation
){

 A parenthesized pattern is equivalent to the @rhombus(pat) inside the
 parentheses.

}


@doc(
  lex_pattern.macro '#%brackets [$charset]'
  lex_pattern.macro '$pat #%index [$charset]'
  operator_order:
    ~order: rx_concatenation
){

 A @rhombus([]) pattern, which is an implicit use of
 @rhombus(#%brackets, ~at rhombus/parser/lex_pattern), matches a single
 character, where @rhombus(charset) determines the matching characters or
 bytes. An implicit @rhombus(#%index, ~at rhombus/parser/lex_pattern)
 form is treated as a sequence of a @rhombus(pat) and
 @rhombus(#%brackets, ~at rhombus/parser/lex_pattern).

 See @secref(~doc: ref_doc, "rx-charset") for @tech(~doc: ref_doc){character set} forms that can be
 used in @rhombus(charset).

@examples(
  ~eval: lex_eval
  def lex:
    lexer
    | ["a"-"z"]: #'alpha
  def i = Port.Input.open_string("amB")
  lex(i)
  lex(i)
  ~error:
    lex(i)
)

}


@doc(
  lex_pattern.macro '$pat *'
  lex_pattern.macro '$pat +'
  lex_pattern.macro '$pat ?'
  lex_pattern.macro '$pat #%comp {$count}'
  lex_pattern.macro '$pat #%comp {$min #,(@rhombus(.., ~at rhombus/parser/lex_pattern))}'
  lex_pattern.macro '$pat #%comp {$min #,(@rhombus(..=, ~at rhombus/parser/lex_pattern)) $max}'
  operator_order:
    ~order: rx_repetition
){

 Matches a sequence of matches to @rhombus(pat):

@itemlist(

  @item{@rhombus(*, ~at rhombus/parser/lex_pattern): 0 or more}
  @item{@rhombus(+, ~at rhombus/parser/lex_pattern): 1 or more}
  @item{@rhombus(?, ~at rhombus/parser/lex_pattern): 0 or 1}
  @item{@rhombus({count}): exactly @rhombus(count)}
  @item{@rhombus({min #,(@rhombus(.., ~at rhombus/parser/lex_pattern))}): @rhombus(min) or more}
  @item{@rhombus({min #,(@rhombus(..=, ~at rhombus/parser/lex_pattern)) max}): between @rhombus(min) and @rhombus(max) (inclusive)}

)

@examples(
  ~eval: lex_eval
  def lex:
    lexer
    | "x" "a"+: ["+", lexeme]
    | "x" "a"*: ["*", lexeme]
    | "y" "b"?: ["?", lexeme]
    | "z" "c"{3}: ["3", lexeme]
    | "z" "c"{1 ..= 2}: ["1-2", lexeme]
    | "z" "c"{4 ..}: ["4+", lexeme]
  def i = Port.Input.open_string("xaaxybyzcccccczccczc")
  lex(i)
  lex(i)
  lex(i)
  lex(i)
  lex(i)
  lex(i)
  lex(i)
)

}

@doc(
  lex_pattern.macro '..'
  lex_pattern.macro '..='
){

 Only allowed within a @rhombus({}) repetition form.

}


@doc(
  lex_pattern.macro 'any'
){

 Matches a single character.

@examples(
  ~eval: lex_eval
  def lex:
    lexer
    | "a" any* "z": lexeme
  def i = Port.Input.open_string("aBC\n0_z!")
  lex(i)
)

}

@doc(
  lex_pattern.macro 'alpha'
  lex_pattern.macro 'upper'
  lex_pattern.macro 'lower'
  lex_pattern.macro 'digit'
  lex_pattern.macro 'xdigit'
  lex_pattern.macro 'alnum'
  lex_pattern.macro 'word'
  lex_pattern.macro 'blank'
  lex_pattern.macro 'newline'
  lex_pattern.macro 'space'
  lex_pattern.macro 'graph'
  lex_pattern.macro 'print'
  lex_pattern.macro 'cntrl'
  lex_pattern.macro 'ascii'
  lex_pattern.macro 'latin1'
  lex_pattern.macro 'unicode.Ll'
  lex_pattern.macro 'unicode.Lu'
  lex_pattern.macro 'unicode.Lt'
  lex_pattern.macro 'unicode.Lm'
  lex_pattern.macro 'unicode.Lx'
  lex_pattern.macro 'unicode.Lo'
  lex_pattern.macro 'unicode.L'
  lex_pattern.macro 'unicode.Nd'
  lex_pattern.macro 'unicode.Nl'
  lex_pattern.macro 'unicode.No'
  lex_pattern.macro 'unicode.N'
  lex_pattern.macro 'unicode.Ps'
  lex_pattern.macro 'unicode.Pe'
  lex_pattern.macro 'unicode.Pi'
  lex_pattern.macro 'unicode.Pf'
  lex_pattern.macro 'unicode.Pc'
  lex_pattern.macro 'unicode.Pd'
  lex_pattern.macro 'unicode.Po'
  lex_pattern.macro 'unicode.P'
  lex_pattern.macro 'unicode.Mn'
  lex_pattern.macro 'unicode.Mc'
  lex_pattern.macro 'unicode.Me'
  lex_pattern.macro 'unicode.M'
  lex_pattern.macro 'unicode.Sc'
  lex_pattern.macro 'unicode.Sk'
  lex_pattern.macro 'unicode.Sm'
  lex_pattern.macro 'unicode.So'
  lex_pattern.macro 'unicode.S'
  lex_pattern.macro 'unicode.Zl'
  lex_pattern.macro 'unicode.Zp'
  lex_pattern.macro 'unicode.Zs'
  lex_pattern.macro 'unicode.Z'
  lex_pattern.macro 'unicode.Cc'
  lex_pattern.macro 'unicode.Cf'
  lex_pattern.macro 'unicode.Cs'
  lex_pattern.macro 'unicode.Cn'
  lex_pattern.macro 'unicode.Co'
  lex_pattern.macro 'unicode.C'
){

 Each of these names is bound both as a character set and as a
 pattern that can be used directly, instead of wrapping in @rhombus([]). See
 the @rhombus(alpha, ~at rhombus/rx_charset), etc., character set for
 more information.


@examples(
  ~eval: lex_eval
  lex_pattern.macro 'octal':
    '["0"-"7"]'
  def lex:
    lexer
    | "0" octal+: [lexeme, String.to_int(lexeme, ~radix: 8)]
  def i = Port.Input.open_string("04448")
  lex(i)
)

}

@doc(
  ~meta
  def lex_pattern_meta.space :: SpaceMeta
){

 A compile-time value that identifies the same space as
 @rhombus(lex_pattern, ~space). See also @rhombus(SpaceMeta, ~annot).

}


@doc(
  ~nonterminal:
    macro_patterns: expr.macro ~defn
  defn.macro 'lex_pattern.macro macro_patterns'
){

 Like @rhombus(expr.macro, ~defn), but defines a new @tech{lexer pattern} operator.

}


@doc(
  ~meta
  syntax_class lex_pattern_meta.Parsed
  syntax_class lex_pattern_meta.AfterPrefixParsed(name :: Name)
  syntax_class lex_pattern_meta.AfterInfixParsed(name :: Name)
){

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), etc., but for
 @tech{lexer patterns}.

}


@close_eval(lex_eval)
