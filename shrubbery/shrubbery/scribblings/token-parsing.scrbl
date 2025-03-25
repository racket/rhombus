#lang rhombus/scribble/manual
@(import:
    "grammar.rhm" open
    "quote.rhm" open)

@(def notecol = italic)

@title(~tag: "token-parsing"){Token Parsing}

The tokens used for grouping and indentation are distinct from other categories:

@verbatim(~indent: 2)|{
( ) [ ] { } '   ; ,   : |   « »  \
}|

Other tokens are described by the grammar below, where a
star (★) in the left column indicates the productions that correspond to
@tech{terms} or comments.

@deftech{Numbers} are supported directly in simple forms---decimal integers,
decimal floating point, hexadecimal/octal/binary integers, and fractions---in all cases allowing
@litchar{_}s between digits. A @s_exp_braces escape
provides access to the full Racket S-expression number grammar. Special
floating-point values use a @litchar{#} notation: @litchar{#inf},
@litchar{#neginf}, and @litchar{#nan}.

@deftech{Boolean literals} are @litchar{#true} and @litchar{#false}.
The @deftech{void} value is @litchar{#void}.

@deftech{Identifiers} are formed from Unicode alphanumeric characters plus @litchar{_}
and emoji sequences, where the initial character must not be a numeric character
(unless that numeric character starts an emoji sequence, as in @litchar{1}
followed by U+FE0F and U+20E3). An identifier can also be prefixed with @litchar{#%};
such identifiers are intended for use for ``internal'' names that are not normally
visible. An identifier prefixed with @litchar{~} (and without @litchar{#%})
forms a @deftech{keyword}, analogous to prefixing an identifier with @litchar{#:} in Racket.

@deftech{Operators} are formed from Unicode symbolic and punctuation characters
other than the ones listed above as distinct tokens (plus a few more,
like @litchar{"}, @litchar{'}, and single-character emoji sequences), but @litchar{|} or @litchar{:} is
also allowed in an operator name as long as it is not by itself, and
some @litchar{#} combinations like @litchar{#'} and @litchar{#,} are also operators.
A multi-character operator
cannot end in @litchar{:}, since that creates an ambiguity with an
operator just before a block, except that a sequence containing only
@litchar{:} is allowed. A multi-character operator can
end with @litchar{/} only when followed by a character other than
@litchar{/} or @litchar{*}, and an operator cannot contain @litchar{//} or @litchar{/*};
those constraints avoid ambiguities with comments.

Implicit in the grammar is the usual convention of choosing the largest
possible match at the start of a stream. Not reflected in the grammar is
a set of @deftech{delimiter} requirements: numbers, @litchar{#true}, and
@litchar{#false} must be followed by a delimiter. For example,
@litchar{1x} is a lexical error, because the @litchar{x} after
@litchar{1} is not a delimiter. Non-alphanumeric characters other than
@litchar{_} are delimiters.

Certain ambiguities related to number and operator parsing are
resolved by special rules. A number ends with a trailing @litchar{.}
only if the @litchar{.} cannot be treated as the start of a
multi-character operator; also, a @litchar{.} that is not part of a
multi-character operator cannot appear @emph{after} a number. The
@litchar{+} and @litchar{-} characters as a number prefix versus an
operator are similarly treated as part of a multi-character operator
when possible, and they subject to one additional rule: they are parsed as
a single-character operator when immediately preceded by an alphanumeric character,
@litchar{_}, @litchar{.}, @litchar{)}, @litchar{]}, or @litchar("}")
with no whitespace in between. For example, @litchar{1+2} is
@litchar{1} plus @litchar{2}, but @litchar{1 +2} is @litchar{1}
followed by the number @litchar{+2}.

When a @s_exp_braces escape describes an identifier
S-expression, it is an identifier in the same sense as a
shrubbery-notation identifier. The same holds for numbers, booleans,
strings, byte strings, and keywords. A @s_exp_braces
escape must @emph{not} describe a pair, because pairs are used to represent a
parsed shrubbery, and allowing pairs would create ambiguous or
ill-formed representations. The @s_exp_kw_braces shorthand always
produces a keyword, where the content of @s_exp_kw_braces must be
an S-expression identifier that is converted to a keyword.

Lines and indentation-influencing whitespace are not represented as
tokens. Instead, each token conceptually has a line and column derived
from its position in the input sequence of characters. The line for an
input sequence increments at a linefeed character (code point 0x0A), a
two-character sequence of return (code point 0x0C) and linefeed, or a
return character that is not followed by a linefeed character. The
column of an input sequence for measuring @deftech{indentation}
increments once per Unicode @defterm{grapheme cluster}, except that tabs
are treated specially.@margin_note{Note that the use of grapheme
 clusters is a different counting of columns than built into a Racket or
 Rhombus input port, which counts by Unicode code points.} More
generally, a column corresponds to a sequence of spaces and tabs, where
all non-tab grapheme clusters are treated like a space. A column is more
indented than another only if it extends the other column's sequence.
When neither of two columns is a prefix of the other, then the columns
are incomparable; if parsing depends on an order between incomparable
columns, then it fails with a ``mix tabs'' error.

For more details on @litchar("@") parsing, see @secref("at-parsing"),
but the table below describes the shape of @litchar("@") forms.

@(def is_lex: @elem{★@hspace(1)})
@(def no_lex: "")
@(def empty_line: ["", @hspace(1), "", "", ""])

@tabular(
  [
    [is_lex, @nonterm{identifier}, bis, @nonterm{plainident}, ""],
    ["", "", bor, @bseq(@litchar{#%}, @nonterm{plainident}), ""],
    empty_line,
    [is_lex, @nonterm{plainident}, bis, bseq(@nonterm{alpha}, kleenestar(@nonterm{alphanum})), ""],
    empty_line,
    [no_lex, @nonterm{alpha}, bis, @elem{@notecol{alphabetic Unicode character or} @litchar{_}}, ""],
    ["", "", bor, @elem{@notecol{Unicode emoji sequence}}, ""],
    empty_line,
    [no_lex, @nonterm{alphanum}, bis, @nonterm{alpha}, ""],
    ["", "", bor, @notecol{numeric Unicode character}, ""],
    empty_line,
    [is_lex, @nonterm{keyword}, bis, bseq(@litchar{~}, @nonterm{plainident}), ""],
    empty_line,
    [is_lex, @nonterm{operator}, bis, bseq(kleenestar(@nonterm{opchar}), @nonterm{tailopchar}),
     @elem{@notecol{not} @litchar{|}, @litchar{:}, @litchar{~},  @notecol{...}}],
    ["", "", bor, bseq(@litchar{:}, kleeneplus(@litchar{:})), @elem{@notecol{... or containing} @litchar{//} @notecol{...}}],
    ["", "", bor, bseq(@litchar{#}, @nonterm{hashopchar}), @elem{@notecol{... or containing} @litchar{/*}}],
    empty_line,
    [no_lex, @nonterm{opchar}, bis, @elem{@italic{symbolic Unicode character not in} @nonterm{special}}, ""],
    ["", "", bor, @elem{@italic{punctuation Unicode character not in} @nonterm{special}}, ""],
    ["", "", bor, @elem{@italic{one of} @litchar{:} @litchar{|}}, ""],
    empty_line,
    [no_lex, @nonterm{tailopchar}, bis, @elem{@italic{anything in} @nonterm{opchar} @italic{except}
                                              @litchar{:}},
     @elem{@notecol{not} @litchar{/} @notecol{followed by} @litchar{/} @notecol{or} @litchar{*}}],
    empty_line,
    [no_lex, @nonterm{hashopchar}, bis, @elem{@italic{one of} @litchar{'}, @litchar{,}, @litchar{;},
                                              @litchar{:}, @litchar{|}}, ""],
    empty_line,
    [no_lex, @nonterm{special}, bis, @elem{@italic{one of} @litchar{(}, @litchar{)}, @litchar{[},
                                           @litchar{]}, @litchar("{"), @litchar("}"), @litchar{'},
                                           @litchar{«}, @litchar{»}}, ""],
    ["", "", bor, @elem{@italic{one of} @litchar{"}, @litchar{;}, @litchar{,}, @litchar{#},
                        @litchar{\}, @litchar{_}, @litchar("@")}, ""],
    ["", "", bor, @elem{@italic{single-character Unicode emoji sequence}}, ""],
    empty_line,
    [is_lex, @nonterm{number}, bis, @nonterm{integer}, ""],
    ["", "", bor, @nonterm{float}, ""],
    ["", "", bor, @nonterm{hexinteger}, ""],
    ["", "", bor, @nonterm{octalinteger}, ""],
    ["", "", bor, @nonterm{binaryinteger}, ""],
    ["", "", bor, @nonterm{fraction}, ""],
    empty_line,
    [no_lex, @nonterm{integer}, bis, bseq(boptional(@nonterm{sign}), @nonterm{nonneg}), ""],
    empty_line,
    [no_lex, @nonterm{sign}, bis, @elem{@italic{one of} @litchar{+} @italic{or} @litchar{-}}, ""],
    empty_line,
    [no_lex, @nonterm{nonneg}, bis, bseq(@nonterm{decimal}, kleeneplus(@nonterm{usdecimal})), ""],
    empty_line,
    [no_lex, @nonterm{decimal}, bis, @elem{@italic{one of} @litchar{0} @italic{through} @litchar{9}}, ""],
    empty_line,
    [no_lex, @nonterm{usdecimal}, bis, @nonterm{decimal}, ""],
    ["", "", bor, bseq(@litchar{_}, @nonterm{decimal}), ""],
    empty_line,
    [no_lex, @nonterm{float}, bis, bseq(boptional(@nonterm{sign}),
                                        @nonterm{nonneg},
                                        @litchar{.},
                                        boptional(@nonterm{nonneg}),
                                        boptional(@nonterm{exp})), ""],
    ["", "", bor, bseq(boptional(@nonterm{sign}),
                       @litchar{.},
                       @nonterm{nonneg},
                       boptional(@nonterm{exp})), ""],
    ["", "", bor, bseq(boptional(@nonterm{sign}),
                       @nonterm{nonneg},
                       @nonterm{exp}), ""],
    ["", "", bor, @litchar{#inf}, ""],
    ["", "", bor, @litchar{#neginf}, ""],
    ["", "", bor, @litchar{#nan}, ""],
    empty_line,
    [no_lex, @nonterm{exp}, bis, bseq(@litchar{e},
                                      boptional(@nonterm{sign}),
                                      @nonterm{nonneg}), ""],
    ["", "", bor, bseq(@litchar{E},
                       boptional(@nonterm{sign}),
                       @nonterm{nonneg}), ""],
    empty_line,
    [no_lex, @nonterm{hexinteger}, bis, bseq(boptional(@nonterm{sign}),
                                             @litchar{0x},
                                             @nonterm{hex},
                                             kleenestar(@nonterm{ushex})), ""],
    empty_line,
    [no_lex, @nonterm{hex}, bis, @elem{@italic{one of} @litchar{0} @italic{through} @litchar{9}}, ""],
    ["", "", bor, @elem{@italic{one of} @litchar{a} @italic{through} @litchar{f}}, ""],
    ["", "", bor, @elem{@italic{one of} @litchar{A} @italic{through} @litchar{F}}, ""],
    empty_line,
    [no_lex, @nonterm{ushex}, bis, @nonterm{hex}, ""],
    ["", "", bor, bseq(@litchar{_}, @nonterm{hex}), ""],
    empty_line,


    [no_lex, @nonterm{octalinteger}, bis, bseq(boptional(@nonterm{sign}),
                                               @litchar{0o},
                                               @nonterm{octal},
                                               kleenestar(@nonterm{usoctal})), ""],
    empty_line,
    [no_lex, @nonterm{octal}, bis, @elem{@italic{one of} @litchar{0} @italic{through} @litchar{7}}, ""],
    empty_line,
    [no_lex, @nonterm{usoctal}, bis, @nonterm{octal}, ""],
    ["", "", bor, bseq(@litchar{_}, @nonterm{octal}), ""],
    empty_line,

    [no_lex, @nonterm{binaryinteger}, bis, bseq(boptional(@nonterm{sign}),
                                                @litchar{0b},
                                                @nonterm{bit},
                                                kleenestar(@nonterm{usbit})), ""],
    empty_line,
    [no_lex, @nonterm{bit}, bis, @elem{@italic{one of} @litchar{0} @italic{or} @litchar{1}}, ""],
    empty_line,
    [no_lex, @nonterm{usbit}, bis, @nonterm{bit}, ""],
    ["", "", bor, bseq(@litchar{_}, @nonterm{bit}), ""],
    empty_line,

    [no_lex, @nonterm{fraction}, bis, bseq(@nonterm{integer}, @litchar{/}, @nonterm{nonneg}), @elem{@nonterm{nonneg} @notecol{not 0}}],
    empty_line,

    [is_lex, @nonterm{boolean}, bis, @litchar{#true}, ""],
    ["", "", bor, @litchar{#false}, ""],
    empty_line,
    [is_lex, @nonterm{void}, bis, @litchar{#void}, ""],
    empty_line,
    [is_lex, @nonterm{string}, bis, bseq(@litchar{"}, kleenestar(@nonterm{strelem}), @litchar{"}), ""],
    empty_line,
    [no_lex, @nonterm{strelem}, bis, @italic{like Racket, but no literal newline}, @notecol{@litchar{\U} ≤ 6 digits}],
    empty_line,
    [is_lex, @nonterm{bytestring}, bis, bseq(@litchar{#"}, kleenestar(@nonterm{bytestrelem}), @litchar{"}), ""],
    empty_line,
    [no_lex, @nonterm{bytestrelem}, bis, @italic{like Racket, but no literal newline}, ""],
    empty_line,
    [is_lex, @nonterm{sexpression}, bis, bseq(@litchar("#{"), @nonterm{racket}, @litchar("}")), ""],
    ["", "", bis, bseq(@litchar("~#{"), @nonterm{racket-identifier}, @litchar("}")), ""],
    empty_line,
    [no_lex, @nonterm{racket}, bis, @italic{any non-pair Racket S-expression}, ""],
    empty_line,
    [is_lex, @nonterm{comment}, bis, bseq(@litchar{//}, kleenestar(@nonterm{nonnlchar})), ""],
    ["", "", bor, bseq(@litchar{/*}, kleenestar(@nonterm{anychar}), @litchar{*/}), @notecol{nesting allowed}],
    ["", "", bor, bseq(@litchar("@//"), kleenestar(@nonterm{nonnlchar})), @elem{@notecol{only within} @nonterm{text}}],
    ["", "", bor, bseq(@litchar("@//"), @nonterm{atopen}, kleenestar(@nonterm{anychar}), @nonterm{atopen}), @elem{@notecol{only within} @nonterm{text}}],
    ["", "", bor, bseq(@litchar{#! }, kleenestar(@nonterm{nonnlchar}), kleenestar(@nonterm{continue})), ""],
    empty_line,
    [no_lex, @nonterm{nonnlchar}, bis, @italic{any character other than newline}, ""],
    empty_line,
    [no_lex, @nonterm{continue}, bis, bseq(@litchar{\}, kleenestar(@nonterm{nonnlchar})), ""],
    empty_line,
    [is_lex, @nonterm{atexpression}, bis, bseq(@litchar("@"),
                                               @nonterm{command},
                                               boptional(@nonterm{arguments}),
                                               kleenestar(@nonterm{text})), @notecol{no space between parts}],
    ["", "", bor, bseq(@litchar("@"), kleenestar(@nonterm{text})), @notecol{no space between parts}],
    ["", "", bor, bseq(@litchar("@"), @nonterm{splice}), @notecol{no space between parts}],
    empty_line,
    [no_lex, @nonterm{command}, bis, @bseq(@kleenestar(@nonterm{prefix}), @nonterm{identifier}), @notecol{no space between parts}],
    ["", "", bor, @nonterm{keyword}, ""],
    ["", "", bor, @nonterm{operator}, ""],
    ["", "", bor, @nonterm{number}, ""],
    ["", "", bor, @nonterm{boolean}, ""],
    ["", "", bor, @nonterm{string}, ""],
    ["", "", bor, @nonterm{bytestring}, ""],
    ["", "", bor, @nonterm{racket}, ""],
    ["", "", bor, bseq(@litchar{(}, @kleenestar(@nonterm{group}), @litchar{)}), @italic{usual @litchar{,}-separated}],
    ["", "", bor, bseq(@litchar{[}, @kleenestar(@nonterm{group}), @litchar{]}), @italic{usual @litchar{,}-separated}],
    ["", "", bor, bseq(@litchar{«}, @nonterm{group}, @litchar{»}), ""],
    empty_line,
    [no_lex, @nonterm{splice}, bor, bseq(@litchar{(«}, @nonterm{group}, @litchar{»)}), ""],
    empty_line,
    [no_lex, @nonterm{prefix}, bis, @bseq(@nonterm{identifier}, @nonterm{operator}), @notecol{no space between parts}],
    empty_line,
    [no_lex, @nonterm{arguments}, bis, bseq(@litchar{(}, @kleenestar(@nonterm{group}), @litchar{)}),
     @notecol{optional @litchar{,}-separated}],
    empty_line,
    [no_lex, @nonterm{text}, bis, bseq(@nonterm{atopen}, @nonterm{text}, @nonterm{atclose}),
     @elem{@notecol{escapes in} @nonterm{text}}],
    empty_line,
    [no_lex, @nonterm{atopen}, bis, @litchar("{"), ""],
    ["", "", bor, bseq(@litchar{|}, kleenestar(@nonterm{asciisym}), @litchar("{")), ""],
    empty_line,
    [no_lex, @nonterm{atclose}, bis, @litchar("}"), ""],
    ["", "", bor, bseq(@litchar("}"), kleenestar(@nonterm{asciisym}), @litchar{|}),
     @notecol{flips opener chars}],

  ]
)
