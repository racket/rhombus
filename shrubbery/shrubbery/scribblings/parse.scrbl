#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     shrubbery/parse
                     shrubbery/variant))

@title[#:tag "parse"]{Parsing}

@defmodule[shrubbery/parse]

@defproc[(parse-all [in input-port?]
                    [#:source source any/c (object-name in)]
                    [#:mode mode (or/c 'top 'interactive 'line 'text) 'top]
                    [#:start-column start-column exact-nonnegative-integer? 0]
                    [#:variant variant variant? default-variant])
         (or/c eof-object? syntax?)]{

 Parses shrubbery notation from @racket[in] and returns an S-expression
 representation as described in @secref["parsed-rep"] as a syntax object.

 The result syntax object has no scopes, but it has source-location
 information and @tech{raw text} properties. See @secref["raw-text"] for
 more information.

 The default @racket['top] mode reads @racket[in] until an end-of-file,
 and it expects a sequence of groups that are indented consistently
 throughout (i.e., all starting at the same column). The result in
 @racket['top] mode is always a @racket[multi] representation. The
 @racket['text] mode is similar, but it starts in ``text'' mode, as if
 the entire input is inside curly braces of an @litchar["@"] form (see
 @secref["at-notation"]). The result of @racket['text] mode is always a
 @racket[brackets] representation.

 The @racket['interactive] and @racket['line] modes are similar to @racket['top]. They
 are suitable for a read-eval-print loop or reading the continuation of a
 @racket[@#,hash-lang[] @#,racketmodname[shrubbery]] line, respectively.
 In both modes, reading stops when a newline is encountered, unless an
 opener remains to be closed or a @litchar{:} was encountered. If reading
 continues due to a @litchar{:}, then it stops when a blank line is found
 (where a line containing a comment does not count as blank). In
 @racket['line] mode, the result may be empty, while
 @racket['interactive] mode continues past a newline if the result would
 be empty.

 The shrubbery parser directly determines line and column changes for
 the purposes of determining indentation, so it does not require
 @racket[in] to have line counting enabled for that purpose. The
 @racket[start-column] argument supplies a number of characters that
 should be considered before the first character of @racket[in] for
 parsing. Source locations attached to the result syntax objects
 @emph{are} based on positions reported by @racket[in] or line and column
 counting as enabled for @racket[in].

 The @racket[variant] argument selects a variant of shrubbery notation
 to parse; see @racketmodname[shrubbery/variant].

}
