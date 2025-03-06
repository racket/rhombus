#lang scribble/manual
@(require (for-label rhombus/parse
                     racket/base))

@title{Rhombus Expressions in Racket}

@defmodule[rhombus/parse]

Rhombus shrubbery forms can be written in S-expressions using the
encoding described in @secref[#:doc '(lib "shrubbery/scribblings/shrubbery.scrbl") "parsed-rep"].
The @racket[rhombus-expression] and @racket[rhombus-top] forms treat an encoding
as a Rhombus expression or a module-level Rhombus sequence, respectively.

The forms use Rhombus bindings that are @racket[require]d into the
enclosing context. Binding does not matter for structural tokens like
@racket[group] or @racket[op], but they matter for S-expression
identifiers that represent shrubbery identifiers and operations.

@defform[(rhombus-expression shrubbery)]{

 Parses @racket[shrubbery] as a Rhombus expression for a Racket
 expression context. The @racket[shrubbery] form should represent a
 group, meaning that it should be a list-shaped S-expression that starts
 with the identifier @racket[group].

}

@defform[(rhombus-top shrubbery)]{

 Parses @racket[shrubbery] as a Rhombus module-body sequence for a
 Racket module context. The @racket[shrubbery] form should be a
 list-shaped S-expression that starts with the identifier @racket[multi].
}
