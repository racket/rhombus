#lang scribble/rhombus/manual
@(import: "grammar.rhm": no_prefix)

@title[~tag: "lexeme-parsing"]{Lexeme Parsing}

The tokens used for grouping and indentation are distinct lexemes:

@verbatim[~indent: 2]|{
( ) [ ] { }   ; ,   : |   « »  \
}|

Other lexemes are described by the grammar in the table below, where an
asterisk in the left column indicates the productions that correspond to
@tech{terms} or comments.

Numbers are supported directly in in simple forms---decimal integers,
decimal floating point, and hexadecimal integers---in all cases allowing
@litchar{_}s between digits. A @litchar["#{"]...@litchar["}"] escape
provides access to the full Racket S-expression number grammar. Special
floating-point values use a @litchar{#} notation: @litchar{#inf},
@litchar{#neginf}, and @litchar{#nan}.

Boolean literals are Racket-style @litchar{#true} and @litchar{#false}.
The void value is @litchar{#void}.

Identifiers are formed from Unicode alphanumeric characters plus @litchar{_},
where the initial character must not be a numeric character. An
identifier prefixed with @litchar{~} forms a keyword, analogous to prefixing an
identifier with @litchar{#:} in Racket.

Operators are formed from Unicode symbolic and punctuation characters
other than the ones listed above as distinct lexemes (plus a few more,
like @litchar{"} and @litchar{'}), but @litchar{|} or @litchar{:} is
also allowed in an operator name as long as it is not by itself. A
multi-character operator cannot end in @litchar{+}, @litchar{-}, or
@litchar{.} to avoid ambiguity in cases like @litchar{1+-2} (which is
@litchar{1} plus @litchar{-2}, not @litchar{1} and @litchar{2} combined
with a @litchar{+-} operator), unless the operator contains only
@litchar{+}, @litchar{-}, or @litchar{.} (so @litchar{++}, @litchar{--},
and @litchar{...} are allowed). Also, multi-character operator cannot
end with @litchar{/} or contain @litchar{//} or @litchar{/*}, because
that can create ambiguities with comments.

Implicit in the grammar is the usual convention of choosing the largest
possible match at the start of a stream. Not reflected in the grammar is
a set of delimiter requirements: numbers, @litchar{#true}, and
@litchar{#false} must be followed by a delimiter. For example,
@litchar{1x} is a lexical error, because the @litchar{x} after
@litchar{1} is not a delimiter. Non-alphanumeric characters other than
@litchar{_} and @litchar{.} are delimiters. Finally, the treatment of
@litchar{+} and @litchar{-} as a number prefix versus an operator is
subject to a special rule: they are parsed as operators when immediately
preceded by an alphanumeric character, @litchar{_}, @litchar{)},
@litchar{]}, or @litchar["}"] with no whitespace in between. For
example, @litchar{1+2} is @litchar{1} plus @litchar{2}, but @litchar{1
 +2} is @litchar{1} followed by the number @litchar{+2}.

When a @litchar["#{"]...@litchar["}"] escape describes an identifier
S-expression, it is an identifier in the same sense as a
shrubbery-notation identifier. the same holds for numbers, booleans,
strings, byte strings, and keywords. A @litchar["#{"]...@litchar["}"]
escape must _not_ describe a pair, because pairs are used to represent a
parsed shrubbery, and allowing pairs would create ambiguous or
ill-formed representations.

A @litchar["@"] starts an at-expression form similar to the notaton
supported by @litchar{#lang at-exp} (which oriented toward S-expressions
and readtable-based). @Secref["at-notation"] explains in more detail, but
the table below sketches the shape of @litchar["@"] forms.

@(def is_lex: @elem{★@hspace[1]})
@(def no_lex: "")
@(def empty_line: ["", @hspace[1], "", "", ""])

@tabular[
  [
    [is_lex, @nonterm{identifier}, bis, bseq(@nonterm{alpha}, kleenestar(@nonterm{alphanum})), ""],
    empty_line,
    [no_lex, @nonterm{alpha}, bis, @elem{@italic{an alphabetic Unicode character or} @litchar{_}}, ""],
    empty_line,
    [no_lex, @nonterm{alphanum}, bis, @nonterm{alpha}, ""],
    ["", "", bor, @italic{a numeric Unicode character}, ""],
    empty_line,
    [is_lex, @nonterm{keyword}, bis, bseq(@litchar{~}, @nonterm{alpha}), ""],
    empty_line,
    [is_lex, @nonterm{operator}, bis, bseq(kleenestar(@nonterm{opchar}), @nonterm{tailopchar}),
     @elem{@italic{not} @litchar{|} @italic{or} @litchar{:} @italic{...}}],
    ["", "", bor, kleeneplus(@litchar{.}), @elem{@italic{... or containing} @litchar{//} @italic{...}}],
    ["", "", bor, kleeneplus(@litchar{+}), @elem{@italic{... or containing} @litchar{/*}}],
    ["", "", bor, kleeneplus(@litchar{-}), ""],
    empty_line,
    [no_lex, @nonterm{opchar}, bis, @elem{@italic{a symbolic Unicode character not in} @nonterm{special}}, ""],
    ["", "", bor, @elem{@italic{a punctuation Unicode character not in} @nonterm{special}}, ""],
    ["", "", bor, @elem{@italic{one} @litchar{:} @litchar{|}}, ""],
    empty_line,
    [no_lex, @nonterm{tailopchar}, bis, @elem{@italic{anything in} @nonterm{opchar} @italic{except}
                                              @litchar{+}, @litchar{-}, @litchar{.}, @litchar{/}}, ""],
    empty_line,
    [no_lex, @nonterm{special}, bis, @elem{@italic{one of} @litchar{(}, @litchar{)}, @litchar{[},
                                           @litchar{]}, @litchar["{"], @litchar["}"], @litchar{«}, @litchar{»}}, ""],
    ["", "", bor, @elem{@italic{one of} @litchar{"}, @litchar{;}, @litchar{,}, @litchar{~}, @litchar{#},
                        @litchar{\}, @litchar{_}, @litchar["@"]}, ""],
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
    [is_lex, @nonterm{sexpression}, bis, bseq(@litchar["#{"], @nonterm{racket}, @litchar["}"]), ""],
    empty_line,
    [no_lex, @nonterm{racket}, bis, @italic{any non-pair Racket S-expression}, ""],
    empty_line,
    [is_lex, @nonterm{comment}, bis, bseq(@litchar{//}, kleenestar(@nonterm{nonnlchar})), ""],
    ["", "", bor, bseq(@litchar{/*}, kleenestar(@nonterm{anychar}), @litchar{*/}), "nesting allowed"],
    empty_line,
    [no_lex, @nonterm{nonnlchar}, bis, @italic{any character other than newline}, ""],
    empty_line,
    [is_lex, @nonterm{atexpression}, bis, bseq(@litchar["@"],
                                               boptional(@nonterm{command}),
                                               boptional(@nonterm{arguments}),
                                               boptional(@nonterm{body})), "no space between parts"],
    empty_line,
    [no_lex, @nonterm{command}, bis, @nonterm{identifier}, ""],
    ["", "", bor, @nonterm{keyword}, ""],
    ["", "", bor, @nonterm{operator}, ""],
    ["", "", bor, @nonterm{number}, ""],
    ["", "", bor, @nonterm{boolean}, ""],
    ["", "", bor, @nonterm{string}, ""],
    ["", "", bor, @nonterm{bytestring}, ""],
    ["", "", bor, @nonterm{racket}, ""],
    ["", "", bor, bseq(@litchar{(}, @kleenestar(@nonterm{group}), @litchar{)}), ""],
    ["", "", bor, bseq(@litchar{«}, @nonterm{group}, @litchar{»}), ""],
    empty_line,
    [no_lex, @nonterm{arguments}, bis, bseq(@litchar{[}, @kleenestar(@nonterm{group}), @litchar{]}),
     @italic{usual @litchar{,}-separated}],
    empty_line,
    [no_lex, @nonterm{body}, bis, bseq(@litchar["{"], @nonterm{text}, @litchar["}"]),
     @elem{@italic{escapes in} @nonterm{text}}],
    ["", "", bor, bseq(@nonterm{atopen}, @nonterm{text}, @nonterm{atclose}),
     @elem{@nonterm{atclose} @italic{match} @nonterm{atopen}}],
    empty_line,
    [no_lex, @nonterm{atopen}, bis, bseq(@litchar{|}, kleenestar(@nonterm{asciisym}), @litchar["{"]), ""],
    empty_line,
    [no_lex, @nonterm{atclose}, bis, bseq(@litchar["}"], kleenestar(@nonterm{asciisym}), @litchar{|}),
     @italic{flips paren-line}],
    
  ]
]