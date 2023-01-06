#lang scribble/rhombus/manual
@(import: "grammar.rhm" open)

@title(~tag: "lexeme-parsing"){Lexeme Parsing}

The tokens used for grouping and indentation are distinct lexemes:

@verbatim(~indent: 2)|{
( ) [ ] { } '   ; ,   : |   « »  \
}|

Other lexemes are described by the grammar in the table below, where a
star (★) in the left column indicates the productions that correspond to
@tech{terms} or comments.

Numbers are supported directly in in simple forms---decimal integers,
decimal floating point, and hexadecimal integers---in all cases allowing
@litchar{_}s between digits. A @litchar("#{")...@litchar("}") escape
provides access to the full Racket S-expression number grammar. Special
floating-point values use a @litchar{#} notation: @litchar{#inf},
@litchar{#neginf}, and @litchar{#nan}.

Boolean literals are Racket-style @litchar{#true} and @litchar{#false}.
The void value is @litchar{#void}.

Identifiers are formed from Unicode alphanumeric characters plus @litchar{_}
and emoji sequences, where the initial character must not be a numeric character
(unless that numeric character starts an emoji sequence, as in @litchar{1}
followed by U+FE0F and U+20E3). An
identifier prefixed with @litchar{~} forms a keyword, analogous to prefixing an
identifier with @litchar{#:} in Racket.

Operators are formed from Unicode symbolic and punctuation characters
other than the ones listed above as distinct lexemes (plus a few more,
like @litchar{"}, @litchar{'}, and single-character emoji sequences), but @litchar{|} or @litchar{:} is
also allowed in an operator name as long as it is not by itself, and
some @litchar{#} combinations like @litchar{#'} and @litchar{#,} are also operators. A
multi-character operator cannot end in @litchar{+}, @litchar{-}, or
@litchar{.} to avoid ambiguity in cases like @litchar{1+-2} (which is
@litchar{1} plus @litchar{-2}, not @litchar{1} and @litchar{2} combined
with a @litchar{+-} operator), unless the operator contains only
@litchar{+}, @litchar{-}, or @litchar{.} (so @litchar{++}, @litchar{--},
and @litchar{...} are allowed). Also, a multi-character operator cannot
end with @litchar{/} or contain @litchar{//} or @litchar{/*}, because
that can create ambiguities with comments.

Implicit in the grammar is the usual convention of choosing the largest
possible match at the start of a stream. Not reflected in the grammar is
a set of delimiter requirements: numbers, @litchar{#true}, and
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
operator are also subject to a special rule: they are parsed as
operators when immediately preceded by an alphanumeric character,
@litchar{_}, @litchar{.}, @litchar{)}, @litchar{]}, or @litchar("}")
with no whitespace in between. For example, @litchar{1+2} is
@litchar{1} plus @litchar{2}, but @litchar{1 +2} is @litchar{1}
followed by the number @litchar{+2}.

When a @litchar("#{")...@litchar("}") escape describes an identifier
S-expression, it is an identifier in the same sense as a
shrubbery-notation identifier. the same holds for numbers, booleans,
strings, byte strings, and keywords. A @litchar("#{")...@litchar("}")
escape must _not_ describe a pair, because pairs are used to represent a
parsed shrubbery, and allowing pairs would create ambiguous or
ill-formed representations.

For more details on @litchar("@") parsing, see @secref("at-parsing"),
but the table below describes the shape of @litchar("@") forms.

@(def is_lex: @elem{★@hspace(1)})
@(def no_lex: "")
@(def empty_line: ["", @hspace(1), "", "", ""])

@tabular(
  [
    [is_lex, @nonterm{identifier}, bis, bseq(@nonterm{alpha}, kleenestar(@nonterm{alphanum})), ""],
    empty_line,
    [no_lex, @nonterm{alpha}, bis, @elem{@italic{alphabetic Unicode character or} @litchar{_}}, ""],
    ["", "", bor, @elem{@italic{Unicode emoji sequence}}, ""],
    empty_line,
    [no_lex, @nonterm{alphanum}, bis, @nonterm{alpha}, ""],
    ["", "", bor, @italic{numeric Unicode character}, ""],
    empty_line,
    [is_lex, @nonterm{keyword}, bis, bseq(@litchar{~}, @nonterm{alpha}), ""],
    empty_line,
    [is_lex, @nonterm{operator}, bis, bseq(kleenestar(@nonterm{opchar}), @nonterm{tailopchar}),
     @elem{@italic{not} @litchar{|}, @litchar{:}, @litchar{~},  @italic{...}}],
    ["", "", bor, kleeneplus(@litchar{.}), @elem{@italic{... or containing} @litchar{//} @italic{...}}],
    ["", "", bor, kleeneplus(@litchar{+}), @elem{@italic{... or containing} @litchar{/*}}],
    ["", "", bor, kleeneplus(@litchar{-}), ""],
    ["", "", bor, bseq(@litchar{#}, @nonterm{hashopchar}), ""],
    empty_line,
    [no_lex, @nonterm{opchar}, bis, @elem{@italic{symbolic Unicode character not in} @nonterm{special}}, ""],
    ["", "", bor, @elem{@italic{punctuation Unicode character not in} @nonterm{special}}, ""],
    ["", "", bor, @elem{@italic{one} @litchar{:} @litchar{|}}, ""],
    empty_line,
    [no_lex, @nonterm{tailopchar}, bis, @elem{@italic{anything in} @nonterm{opchar} @italic{except}
                                              @litchar{+}, @litchar{-}, @litchar{.}, @litchar{/}}, ""],
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
    [no_lex, @nonterm{hexinteger}, bis, bseq(@litchar{0x},
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
    [is_lex, @nonterm{boolean}, bis, @litchar{#true}, ""],
    ["", "", bor, @litchar{#false}, ""],
    empty_line,
    [is_lex, @nonterm{void}, bis, @litchar{#void}, ""],
    empty_line,
    [is_lex, @nonterm{string}, bis, bseq(@litchar{"}, kleenestar(@nonterm{strelem}), @litchar{"}), ""],
    empty_line,
    [no_lex, @nonterm{strelem}, bis, @italic{like Racket, but no literal newline}, @elem{@litchar{\U} ≤ 6 digits}],
    empty_line,
    [is_lex, @nonterm{bytestring}, bis, bseq(@litchar{#"}, kleenestar(@nonterm{bytestrelem}), @litchar{"}), ""],
    empty_line,
    [no_lex, @nonterm{bytestrelem}, bis, @italic{like Racket, but no literal newline}, ""],
    empty_line,
    [is_lex, @nonterm{sexpression}, bis, bseq(@litchar("#{"), @nonterm{racket}, @litchar("}")), ""],
    empty_line,
    [no_lex, @nonterm{racket}, bis, @italic{any non-pair Racket S-expression}, ""],
    empty_line,
    [is_lex, @nonterm{comment}, bis, bseq(@litchar{//}, kleenestar(@nonterm{nonnlchar})), ""],
    ["", "", bor, bseq(@litchar{/*}, kleenestar(@nonterm{anychar}), @litchar{*/}), "nesting allowed"],
    ["", "", bor, bseq(@litchar("@//"), kleenestar(@nonterm{nonnlchar})), @elem{only within @nonterm{text}}],
    ["", "", bor, bseq(@litchar("@//"), @nonterm{atopen}, kleenestar(@nonterm{anychar}), @nonterm{atopen}), @elem{only within @nonterm{text}}],
    ["", "", bor, bseq(@litchar{#! }, kleenestar(@nonterm{nonnlchar}), kleenestar(@nonterm{continue})), ""],
    empty_line,
    [no_lex, @nonterm{nonnlchar}, bis, @italic{any character other than newline}, ""],
    empty_line,
    [no_lex, @nonterm{continue}, bis, bseq(@litchar{\}, kleenestar(@nonterm{nonnlchar})), ""],
    empty_line,
    [is_lex, @nonterm{atexpression}, bis, bseq(@litchar("@"),
                                               @nonterm{command},
                                               boptional(@nonterm{arguments}),
                                               kleenestar(@nonterm{text})), "no space between parts"],
    ["", "", bor, bseq(@litchar("@"), kleenestar(@nonterm{text})), "no space between parts"],
    ["", "", bor, bseq(@litchar("@"), @nonterm{splice}), "no space between parts"],
    empty_line,
    [no_lex, @nonterm{command}, bis, @bseq(@kleenestar(@nonterm{prefix}), @nonterm{identifier}), "no space between parts"],
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
    [no_lex, @nonterm{prefix}, bis, @bseq(@nonterm{identifier}, @nonterm{operator}), "no space between parts"],
    empty_line,
    [no_lex, @nonterm{arguments}, bis, bseq(@litchar{(}, @kleenestar(@nonterm{group}), @litchar{)}),
     @italic{optional @litchar{,}-separated}],
    empty_line,
    [no_lex, @nonterm{text}, bis, bseq(@nonterm{atopen}, @nonterm{text}, @nonterm{atclose}),
     @elem{@italic{escapes in} @nonterm{text}}],
    empty_line,
    [no_lex, @nonterm{atopen}, bis, @litchar("{"), ""],
    ["", "", bor, bseq(@litchar{|}, kleenestar(@nonterm{asciisym}), @litchar("{")), ""],
    empty_line,
    [no_lex, @nonterm{atclose}, bis, @litchar("}"), ""],
    ["", "", bor, bseq(@litchar("}"), kleenestar(@nonterm{asciisym}), @litchar{|}),
     @italic{flips opener chars}],
    
  ]
)
