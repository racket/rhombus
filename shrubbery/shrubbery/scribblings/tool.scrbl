#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     racket/gui/base
                     framework
                     syntax-color/color-textoid
                     (only-in (submod shrubbery reader)
                              get-info-proc
                              make-get-info-proc)
                     shrubbery/syntax-color
                     shrubbery/indentation
                     shrubbery/navigation
                     shrubbery/keystroke
                     shrubbery/variant))

@(define dr-doc '(lib "scribblings/tools/tools.scrbl"))

@title[#:tag "tool"]{Tool Support}

Librraies such as @racketmodname[shrubbery/syntax-color] and
@racketmodname[shrubbery/indentation] provide tools for working with
shrubbery-based programs. They generally follow DrRacket's protocols,
but they are also intended to support other tools where the protocols
can be adapted.

@section{Language Configuration}

@defmodule[(submod shrubbery reader)]

The @racketmodname[(submod shrubbery reader)] module provides
@racketidfont{read}, @racketidfont{read-syntax}, and @racket{get-info}
functions as normal for a @racketidfont{reader} submodule, but
@racketidfont{read} and @racketidfont{read-syntax} accept an extra
@racket[#:variant] argument in the sense of
@racketmodname[shrubbery/variant]. To better cooperate with new
languages that are defined with @racketmodname[syntax/module-reader],
the @racketmodname[(submod shrubbery reader)] module also provides
@racket[get-info-proc] and @racket[make-get-info-proc].

@defproc[(get-info-proc [key symbol?]
                        [default any/c]
                        [make-default (-> symbol? any/c -> any/c)]
                        [#:variant variant variant? default-variant])
         any/c]{

 Returns language-configuration results that are suitable for any
 shrubbery-based languages. The results are based on modules such as
 @racketmodname[shrubbery/syntax-color],
 @racketmodname[shrubbery/indentation],
 @racketmodname[shrubbery/navigation], and
 @racketmodname[shrubbery/keystroke].

}

@defproc[(make-get-info-proc [#:variant variant variant? default-variant])
         procedure?]{

 Returns a procedure like @racket[get-info-proc], but with
 @racket[variant] already supplied as the @racket[#:variant] argument.

}

@section{Syntax Coloring}

@defmodule[shrubbery/syntax-color]

@defproc[(shrubbery-lexer [in input-port?]
                          [pos exact-nonnegative-integer?]
                          [status any/c]
                          [#:variant variant variant? default-variant])
         (values (or/c string? eof-object?)
                 (or/c symbol?
                       (and/c (hash/c symbol? any/c) immutable?))
                 (or/c symbol? #f)
                 (or/c number? #f)
                 (or/c number? #f)
                 exact-nonnegative-integer?
                 any/c)]{

 A syntax-coloring lexer that follows the
 @method[color:text<%> start-colorer] protocol
 @seclink["Syntax_Coloring" #:doc dr-doc #:indirect? #t]{used by DrRacket}, but with
 an additional optional @racket[#:variant] argument.

}

@defproc[(shrubbery-text-mode-lexer [in input-port?]
                                    [pos exact-nonnegative-integer?]
                                    [status any/c]
                                    [#:variant variant variant? default-variant])
         (values (or/c string? eof-object?)
                 (or/c symbol?
                       (and/c (hash/c symbol? any/c) immutable?))
                 (or/c symbol? #f)
                 (or/c number? #f)
                 (or/c number? #f)
                 exact-nonnegative-integer?
                 any/c)]{

 Like @racket[shrubbery-lexer], but starting in ``text'' mode as if
 surrounded by an @litchar["@"] form and inside the form's @litchar["{"]
 and @litchar["}"].

}

@deftogether[(
@defproc[(make-shrubbery-lexer [#:variant variant variant? default-variant])
          procedure?]
@defproc[(make-shrubbery-text-mode-lexer [#:variant variant variant? default-variant])
          procedure?]
)]{

 Returns a procedure like @racket[shrubbery-lexer] or @racket[shrubbery-text-mode-lexer],
 but where @racket[variant] is already supplied as the @racket[#:variant] argument.

}


@section{Indentation}

@defmodule[shrubbery/indentation]

@defproc[(shrubbery-indentation [text (is-a?/c color-textoid<%>)]
                                [pos exact-nonnegative-integer?]
                                [#:multi? multi? any/c #f]
                                [#:always? always? any/c multi?]
                                [#:reverse? reverse? any/c (not always?)]
                                [#:stop-pos stop-pos exact-nonnegative-integer? 0]
                                [#:variant variant variant? default-variant])
         (or/c #f
               exact-nonnegative-integer?
               (list/c exact-nonnegative-integer? string?)
               (listof (or/c exact-nonnegative-integer?
                             (list/c exact-nonnegative-integer? string?))))]{

 Returns a suggested indentation for the line containing @racket[pos] in
 @racket[text] following a protocol
 @seclink["Indentation" #:doc dr-doc #:indirect? #t]{used by DrRacket}, but with several
 extra options described below. The result includes an integer and a
 string only when tab characters force a description of indentation in
 terms of tab and space characters.

 If the current indentation matches a valid indentation but others are
 possible, the result when @racket[multi?] is @racket[#f] corresponds to
 the next valid indentation in a sequence of possibilities.

 @itemlist[

 @item{@racket[multi?]: Returns all possible indentations, instead of
   just the first one.}

 @item{@racket[always?]: When @racket[#f], returns the line's current
   indentation if that indentation is valid instead of cycling to the next
   valid indentation. This argument is provided as @racket[#f] when
   indenting for a newly created line, for example.}

 @item{@racket[reverse?]: When true, causes the a sequence of valid
   indentations to be used or returned in reverse order. This argument's
   default is @racket[#t] when @racket[always?] is @racket[#f] so that a
   new line starts at the most-indented possibility.}

 @item{@racket[stop-pos]: Indicates a position in @racket[text] where
   indentation should stop inspecting, instead of considering the effect of
   earlier characters.}

 @item{@racket[variant]: See @racketmodname[shrubbery/variant].}

 ]


 }

@defproc[(shrubbery-range-indentation [text (is-a?/c color-textoid<%>)]
                                      [start-pos exact-nonnegative-integer?]
                                      [end-pos exact-nonnegative-integer?]
                                      [#:reverse? reverse? any/c #f]
                                      [#:variant variant variant? default-variant])
         (or/c #f
               (listof (list/c exact-nonnegative-integer? string?)))]{

 Similar to @racket[shrubbery-indentation], returns a suggested
 indentation for multiple lines following a character-range protocol
 @seclink["Indentation" #:doc dr-doc #:indirect? #t]{used by DrRacket}.

}

@defproc[(shrubbery-range-indentation/reverse-choices [text (is-a?/c color-textoid<%>)]
                                                      [start-pos exact-nonnegative-integer?]
                                                      [end-pos exact-nonnegative-integer?]
                                                      [#:variant variant variant? default-variant])
         (or/c #f
               (listof (list/c exact-nonnegative-integer? string?)))]{

 Like @racket[shrubbery-range-indentation] with @racket[#:reverse? #t].

}


@deftogether[(
@defproc[(make-shrubbery-indentation [#:variant variant variant? default-variant])
         procedure?]
@defproc[(make-shrubbery-range-indentation [#:variant variant variant? default-variant])
         procedure?]
@defproc[(make-shrubbery-range-indentation/reverse-choices [#:variant variant variant? default-variant])
         procedure?]
)]{

 Returns a procedure like @racket[shrubbery-indentation],
 @racket[shrubbery-range-indentation], or
 @racket[shrubbery-range-indentation/reverse-choices], but with
 @racket[variant] already supplied as the @racket[#:variant] argument.
   
}


@section{Term and Group Navigation}

@defmodule[shrubbery/navigation]

@defproc[(shrubbery-grouping-position [text (is-a?/c color-textoid<%>)]
                                      [pos exact-nonnegative-integer?]
                                      [limit-pos exact-nonnegative-integer?]
                                      [direction (or/c 'up 'down 'backward 'forward)]
                                      [#:variant variant variant? default-variant])
         (or/c #f #t natural?)]{

 Returns navigation guidance starting at @racket[pos] in
 @racket[text] following a protocol
 @seclink["Keystrokes" #:doc dr-doc #:indirect? #t]{used by DrRacket}.

}

@defproc[(make-shrubbery-grouping-position [#:variant variant variant? default-variant])
         procedure?]{

 Returns a procedure like @racket[shrubbery-grouping-position], but with
 @racket[variant] already supplied as the @racket[#:variant] argument.

}

@section{Keystrokes}

@defmodule[shrubbery/keystroke]

@defthing[shrubbery-keystrokes (listof (list/c string?
                                               (-> (is-a?/c text%)
                                                   (is-a?/c event%)
                                                   any)))]{

 Extra keystrokes suitable for shrubbery forms following a protocol
 @seclink["Keystrokes" #:doc dr-doc #:indirect? #t]{used by DrRacket}.

}

@defproc[(make-shrubbery-keystrokes [#:variant variant variant? default-variant])
         list?]{

 Returns a list like @racket[shrubbery-keystrokes], but suitable for a
 form of shrubbery notation selected by @racket[variant].

}
