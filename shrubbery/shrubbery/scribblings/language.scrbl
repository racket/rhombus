#lang scribble/manual
@(require "rhm_id.rhm"
          (for-label racket/base
                     racket/contract/base
                     syntax/parse
                     racket/pretty
                     shrubbery/parse
                     shrubbery/write
                     shrubbery/print
                     shrubbery/property))

@title[#:tag "language"]{Shrubbery Language}

@defmodulelang*[(shrubbery shrubbery/text)]

The @racketmodname[shrubbery] meta-language is similar to the
@racketmodname[s-exp] meta-language. It expects a module name after
@racket[@#,hash-lang[] @#,racketmodname[shrubbery]] to serve as the
language of a Racket @racket[module] form, while the body of the module
after the @hash-lang[] line is parsed as shrubbery notation.

Unlike @racketmodname[s-exp], @racketmodname[shrubbery] also works
without another language listed on the @hash-lang[] line. In that case,
running the module writes (using @racket[pretty-write]) the S-expression form of the parsed shrubbery
(see @secref["parsed-rep"]). For example,

@codeblock{
 #lang shrubbery
 1+2
}

prints @racketresult[(multi (group 1 (op +) 2))]. But if @filepath{demo.rkt} contains

@racketmod[#:file "demo.rkt"
 racket/base
 (require (for-syntax racket/base
                      syntax/parse))

 (provide (rename-out [module-begin #%module-begin])
          + - * /)

 (define-syntax (module-begin stx)
   (syntax-parse stx
     #:datum-literals (multi group op)
     [(_ (multi (group n1:number (op o) n2:number)))
      #'(#%module-begin (o 'n1 'n2))]))
 ]

then

@codeblock{
 #lang shrubbery "demo.rkt"
 1+2
}

prints the result @racketresult[3].

A same-line module language for @racketmodname[shrubbery] is determined
by using @racket[parse-all] in @racket['line] mode. As long as the
resulting shrubbery is not empty, it is parsed in the same way that
@racketmodname[rhombus] parses module names for @|rhm-import|.

The @racketmodname[shrubbery/text] meta-language is similar to
@racketmodname[shrubbery], but it parses the module in @racket['text]
mode. For example,

@codeblock|{
 #lang shrubbery/text
 @(1+2)
}|

prints @racketresult[(brackets (group (parens (group 1 (op +) 2))))].
