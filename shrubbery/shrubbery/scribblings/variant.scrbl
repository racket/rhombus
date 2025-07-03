#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     shrubbery/variant
                     shrubbery/parse))

@title[#:tag "variant"]{Shrubbery Variants}

@defmodule[shrubbery/variant]{

 Shrubbery notation is intended to support multiple languages that all
 share a shrubbery-level specification. In some contexts, however,
 adjusting choices in the design of shrubbery notation can avoid
 surprising interactions and reduce confusion, especially in a
 pedagogic context. The @racketmodname[shrubbery/variant] module
 enables certain shrubbery variations that can be selected by calling
 @racket[make-variant], and shrubbery API functions like
 @racket[parse-all] accept a variant as an argument.

}

@defproc[(variant? [v any/c]) boolean?]{

 Returns @racket[#true] if @racket[v] is a shrubbery variant as produced
 by @racket[make-variant], @racket[#f] otherwise.

}


@defproc[(make-variant [#:allow-operator? allow-operator?
                                          (string? . -> . any/c)
                                          (lambda (str) #t)]
                       [#:indented-operator-continue? indented-operator-continue?
                                                      (string? . -> . any/c)
                                                      (lambda (str) #t)])
         variant?]{

 Creates a representaion of a shrubbery variant, where supplying no
 arguments represents the same variant as @racket[default-variant]:

 @itemlist[

 @item{If @racket[allow-operator?] returns @racket[#f] for a string
   representing a shrubbery operator, then it is treated as a syntax
   error instead of an operator.}

 @item{If @racket[indented-operator-continue?] returns @racket[#f] for
   an operator string, then a group cannot be continued on a new line
   by starting the new line with an operator and indentation greater
   than the line to continue.}

 ]

}

@deftogether[(
@defproc[(variant-allow-operator? [v variant?]) (string? . -> . any/c)]
@defproc[(variant-indented-operator-continue? [v variant?]) (string? . -> . any/c)]
)]{

 Accessors for designations in a variant.

}


@defthing[default-variant variant?]{

 Represents the default shrubbery variant as specified in @secref["spec"].

}
