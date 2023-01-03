#lang scribble/rhombus/manual
@(import: "grammar.rhm" open)

@title(~tag: "at-parsing"){At-Notation Parsing}

@margin_note{See @secref("at-notation") for an overview of @litchar("@") notation.}

As shown in @secref("lexeme-parsing"), each use of @litchar("@") has
one of these forms:

@itemlist(

 @item{@bseq(@litchar("@"), @italic{command}, @litchar("("), @italic{argument}, @litchar(","), @elem{...}, @litchar(")"), @italic{braced_text}, @elem{...})

      Converts to 
      @verbatim(~indent: 2){
        @italic{command_splice}@litchar{(}@italic{argument}@litchar{,} @elem{...}@litchar{,} @litchar{[}@italic{converted_text}, @elem{...}@litchar{]}, @elem{...}@litchar{)}
      }

      where @italic{command_splice} is the same as @italic{command}, except
      that an immediately wrapping @litchar("«»") (if any) is removed, and the
      conversion of @italic{braced_text} is described below.

      The allowed form of @italic{command} is described in
      @secref("lexeme-parsing") as @nonterm{command}, and it always
      corresponds to a group. A group that ends in a block is
      disallowed. Each @italic{argument} is also a group, with no
      additional constraints, but the separating @litchar{,} is
      optional for arguments that are newline-separated groups.
      The form of @italic{braced_text} is described below.

      No space is allowed between @litchar("@") and @litchar{command},
      between @italic{command} and @litchar("("), between
      @litchar(")") and the first @italic{braced_text}, or between
      successive @italic{braced_text}s. Zero or more @italic{braced_text}s
      are allowed.

      Although a character other than @litchar{(} after
      @italic{command} would normally, mean that one of the other @litchar("@")
      forms is being used, a @litchar("[") in that position is treated
      as a parse error. (This prohibition is intended to support
      better error reporting when S-expression @litchar("@") syntax is
      misused in a shrubbery context.)},
      
 @item{@bseq(@litchar("@"), @italic{command}, @italic{braced_text}, @italic{braced_text}, @elem{...})

      Converts to 
      @verbatim(~indent: 2){
        @italic{command_splice}@litchar{(}@litchar{[}@italic{converted_text}, @elem{...}@litchar{]}, @elem{...}@litchar{)}
      }

      The allowed forms of @italic{command} and @italic{braced_text}
      and the spacing rules are the same as for the first case of
      @litchar("@"), with no space allowed between @italic{command}
      and the first @italic{braced_text}. One or more
      @italic{braced_text}s are allowed.},
      
 @item{@bseq(@litchar("@"), @italic{braced_text}, @italic{braced_text}, @elem{...})

      Converts to 
      @verbatim(~indent: 2){
        @litchar{(}@litchar{[}@italic{converted_text}, ...@litchar{]}, ...@litchar{)}
      }

      The allowed forms of @italic{braced_text} and the spacing rules
      are the same as preceding @litchar("@") cases, with no space allowed
      between @litchar("@") and the first @italic{braced_text}. One or
      more @italic{braced_text}s are allowed.},
      
 @item{@bseq(@litchar("@"), @italic{command})

      Converts to
      @verbatim(~indent: 2){
        @italic{command_splice}
      }

      The allowed forms of @italic{command} are the same as for the
      preceding cases of @litchar("@"), except that @italic{command}
      is allowed to be a group that ends in a block if the
      @litchar("@") is at the end of the enclosing group. If
      parentheses or a @italic{braced_text} follow @italic{command},
      then one of the preceding @litchar("@") cases applies, instead.},

 @item{@bseq(@litchar("@(«"), @italic{command}, @litchar("»)"))

      Converts to 
      @verbatim(~indent: 2){
        @italic{command}
      }

      where @italic{command} is allowed to be any group (but ending in
      a block only when @litchar("@") is at the end of the enclosing
      group), and space is allowed between @litchar("@(«") and
      @italic{command} or between @italic{command} and @litchar("»)").},

 @item{@bseq(@litchar("@//"), @italic{braced_text})

       A block-comment form, but allowed only within a
       @italic{braced_text}. No space is allowed between
       @litchar("@//") and @italic{braced_text}.},

 @item{@bseq(@litchar("@//"), @italic{line}, @italic{newline})

       A line-comment form, but allowed only within a
       @italic{braced_text}, where @italic{line} does not contain
       newline characters, and @italic{newline} is a newline
       character. (Return characters are @emph{not} treated as newline
       characters.) The comment also consumes leading whitespace on
       the next line.

       This form applies only when @italic{line} does not start with a
       @italic{braced_text} @italic{opener} as described below.}

)

A @italic{braced_text} starts with an @italic{opener}, ends with a
@italic{closer}, and has a @italic{escape} from literal-text mode:

@itemlist(

 @item{The @italic{opener} can be @litchar("{"), in which case the
       @italic{closer} is @litchar("}") and the @italic{escape} is
       @litchar("@").},

 @item{The @italic{opener} can be @litchar("|"), a sequence of
       non-@litchar("{") ASCII symbol and punctuation characters, and
       @litchar("{"). In that case, the @italic{closer} is
       @litchar("}"), the @italic{flipped} sequence of ASCII
       characters from the opener, and @litchar("|"). In a flipped
       sequence, not only are the characters in the reverse order, but
       certain individual characters are flipped: @litchar{(} to
       @litchar{)}, @litchar{)} to @litchar{(}, @litchar{[} to
       @litchar{]}, @litchar{]} to @litchar{[}, @litchar{<} to
       @litchar{>}, and @litchar{>} to @litchar{<}. The
       @italic{escape} is the same sequence as @italic{opener}, but
       with @litchar("@") in place of @litchar("{"). When a sequence
       after @litchar("@") could be parsed either as an operator
       or as part of an opener ending with @litchar("{"), the parse
       as part of an opener takes precedence.}

)

When multiple @italic{braced_text}s parts are part of a @litchar("@")
form, each @italic{braced_text} can use a different @italic{opener}
and @italic{closer}.

The starting @italic{opener} and ending @italic{closer} of a
@italic{braced_text} do not count as part of the @italic{braced_text}
content. Between the starting @italic{opener} and ending
@italic{closer}, nested instances of the @italic{opener} and
@italic{closer} can appear. That is, for each @italic{opener}
encountered within @italic{braced_text}, it is treated as literal, and
a balancing @italic{closer} is also treated as literal instead of the
end of the @italic{braced_text}.

Within @italic{braced_text}, a use of @italic{escape} triggers
at-notation parsing the same as @litchar("@") outside of a
@italic{braced_text}, but @litchar("@//") comment forms are also
allowed.

The conversion of a @italic{braced_text} to @italic{converted_text}
proceeds in steps, starting with the content between @italic{opener}
and @italic{closer}:

@itemlist(

 @item{Comment escapes are discarded. A line comment using
       @italic{escape} followed by @litchar("//") causes the
       terminating newline to be discarded along with leading
       whitespace on the next line.},

 @item{Trailing whitespace on each line is discarded.},

 @item{The remaining content of @italic{braced_text} is split into
       lines, where escapes are treated as atomic elements of a line
       (i.e., within a line, even if the escape internally has
       multiple lines of text). Return characters are @emph{not}
       treated as newlines, but note that a return--linefeed
       sequence will act like linefeed, because the return is stripped
       as trailing whitespace. Each run of literal text between
       escaped items is a single element within the line. When two
       escapes are adjacent with no literal text in between, the
       escaped elements are consecutive in the line, and not separated
       by an empty literal element. Escapes are never treated as
       literal text, even when the result of an escape is an immediate
       string.},

 @item{If the first line contains only whitespace, the line is
       discarded. Similarly, if the last line contains only
       whitespace, the line is discarded. Afterward, if the remaining
       last line ends with a newline, that trailing newline is also
       discarded.},

 @item{The newline associated with each line is split into its own
       element, if it is not a separate element already.},

 @item{Leading whitespace on each line is broken out into its own
       literal element, if it is not a separate element already.

       Note that a line that originally contained only whitespace will
       have just a newline at this point (not not even that, if it's
       the last line), since trailing whitespace was previously
       discarded.},

 @item{Leading whitespace that starts every non-empty line is removed
       from that element, where a line is non-empty if it contains
       more than just a newline element. If a leading-whitespace
       element becomes an empty as a result of removing a shared
       prefix, the element is removed.},

 @item{The elements for all lines are concatenated into a single list,
       and the elements of that list are the
       @italic{converted_text}s.}

)
