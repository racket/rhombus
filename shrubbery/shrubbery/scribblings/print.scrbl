#lang scribble/manual

@title[#:tag "print"]{Reconstructing Shrubbery Notation}

@defmodule[shrubbery/print]

@defproc[(shrubbery-syntax->string [s syntax?]
                                   [#:use-raw? use-raw? any/c #f]
                                   [#:max-length max-length (or/c #f exact-positive-integer?) #f]
                                   [#:keep-prefix? keep-prefix? any/c #f]
                                   [#:keep-suffix? keep-suffix? any/c #f]
                                   [#:inner? inner? any/c #f]
                                   [#:infer-starting-indentation? infer-starting-indentation? any/c (not keep-prefix?)]
                                   [#:register-stx-range register-stx-range
                                                         (syntax?
                                                          exact-nonnegative-integer?
                                                          exact-nonnegative-integer?
                                                          . -> . any)
                                                         void]
                                   [#:render-stx-hook render-stx-hook
                                                      (syntax? output-port? . -> . any/c)
                                                      (lambda (stx output) #f)])
         string?]{

 Converts a syntax object for an S-expression representation to a string
 form, potentially using @tech{raw text} properties and otherwise falling
 back to @racket[write-shrubbery]. By default, raw text reconstruction is
 used only if raw text is @defterm{consistently available} (as described
 below), but raw text mode can be forced by providing @racket[use-raw?]
 as a true value. When @racket[use-raw?] is true, each syntax object
 without raw text is printed as by @racket[write-shrubbery].

 If @racket[max-length] is a number, the returned string will contain no
 more than @racket[max-length] characters. Internally, conversion to a
 string can take shortcuts once the first @racket[max-length] characters
 have been determined.

 When @racket[keep-suffix?] are @racket[keep-suffix?] are true and raw text mode is used to
 generate the result string, then @racket['raw-prefix] and
 @racket['raw-suffix] text on the
 immediate syntax object are included in the result. Otherwise, prefixes
 and suffixes are rendered only when they appear between @racket['raw]
 text. If @racket[inner?] is true, ``inner'' prefixes and suffixes are
 preserved on the immediate @racket[s] form even if @racket[keep-suffix?]
 and/or @racket[keep-suffix?] are @racket[#false]. If @racket[s] is
 a group or multi-group form, then inner prefixes and suffixes are
 preserved in any case.

 If @racket[infer-starting-indentation?] is true, then a consistent
 amount of leading whitespace is removed from each line of the result
 string.

 The @racket[register-stx-range] and @racket[render-stx-hook] arguments
 provide a hook to record or replace rendering of a syntax object within
 @racket[s]. The @racket[register-stx-range] procedure is called with
 each syntax object in @racket[s] after printing, and the second and
 third arguments report the starting and ending locations in the string
 for the syntax object's printed form. The @racket[render-stx-hook]
 procedure is called before printing each syntax object, and if it
 returns a true value, then printing assumes that the syntax object has
 alerady been rendered to the given output port (which is ultimately
 delivered to a string), and it is not printed in the default way.

 Raw text is @defterm{consistently available} when supplied by
 @racket['raw] syntax properties on all atoms, except that
 @racket['raw-opaque-content] and/or @racket['opaque-raw] properties
 excuse nested atoms from needing @racket['raw] properties. Also,
 a @racket[parsed] form need not have raw text information.

}


@defproc[(shrubbery-syntax->raw [s syntax?]
                                [#:use-raw? use-raw? any/c #f]
                                [#:keep-prefix? keep-prefix? any/c #f]
                                [#:keep-suffix? keep-suffix? any/c #f]
                                [#:inner? inner? any/c #f])
         (values any? any? any?)]{

 Similar to @racket[shrubbery-syntax->string] but delivers raw text
 encodings with the prefix, main, and suffix raw text as separate result
 values. If @racket[keep-prefix?] or @racket[keep-suffix?] is
 @racket[#f], the corresponding result is empty.

}


@defproc[(combine-shrubbery-raw [a any/c] [b any/c]) any/c]{

 Combines @racket[a] and @racket[b] with @racket[cons] if they are both
 non-empty, returns the empty list if both are empty, and otherwise
 returns the argument that is non-empty. In addition to normal raw text
 encodings, a @racket[#f] argument counts as empty.

}
