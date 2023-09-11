#lang scribble/manual
@(require (only-in "common.rhm" Rhombus)
          "rhm_id.rhm"
          (for-label compatibility/package
                     racket/base
                     syntax/parse))

@title{Implementation Examples}

The prototype @Rhombus implementation starts with a
@racket[#%module-begin] form that takes a shrubbery sequence wrapped with
@racket[top] as its input. Simplifying somewhat, the implementation uses a
@racket[rhombus-top] helper macro:

@racketblock[
(define-syntax (rhombus-module-begin stx)
  (syntax-parse stx
    #:datum-literals (top)
    [(_ (top . content))
     #`(#%module-begin
        (rhombus-top . content))]))
]

The @racket[rhombus-top] macro tries to parse each top-level form as
either a declaration, definition, or expression. Parsing a declaration
or definition produces sequence of terms as the @racket[parsed]
attribute, while parsing an expression produces a single expression as
@racket[parsed]:

@RACKETBLOCK[
(define-syntax (rhombus-top stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ form . forms)
     #`(begin
         #,(syntax-parse #'form
             [e::declaration #'(begin . e.parsed)]
             [e::definition #'(begin . e.parsed)]
             [e::expression #'(#%expression e.parsed)])
         (rhombus-top . forms))]))
]

This @racket[rhombus-top] macro uses a typical trampolining pattern: the Racket
macro expander will perform any declaration or definition bindings
before expanding the recursive use of @racket[rhombus-top]. which will then
force more declaration, definition, and expression parsing. That way,
Rhombus-level operators can be defined and then used in the same
module.

The @racket[:definition] syntax class is defined using the simplified Rhombus
expander API:

@racketblock[
  (define-transform
    #:syntax-class :definition
    #:desc "definition"
    #:transformer-ref definition-transformer-ref
    #:check-result check-definition-result)
]

Here, @racket[definition-transformer-ref] refers to a function that extracts
a @racket[transformer] structure from a compile-time value (returning @racket[#f] if
no such structure is available). The @racket[check-definition-result] function
makes sure that the low-level transformer returns at least a
list-shaped syntax object, but that's just for earlier error
detection.

A simple implementation of the @racket[def] definition form could be like
this:

@racketblock[
(define-syntax def
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (block)
        (code:comment "match `def <binding>: <form> ...`, where")
        (code:comment "`:` grouping is represented by `block`")
        [(_ b::binding (block rhs ...))
         (build-values-definitions #'b.parsed
                                   #'(rhombus-block rhs ...))]))))
]

Parsing @racket[b] as @racket[:binding] produces a @racket[parsed]
attribute that embeds the identifiers to bind as well as predicates and
conversion to apply to the result of the right-hand side. The right-hand
side is put into a @racket[rhombus-block] form, which bounces back to
(local) definition and expression parsing, roughly like this:

@racketblock[
(define-syntax (rhombus-block stx)
  (syntax-parse stx
    [(_ . tail) #`(let () (rhombus-body . tail))]))

(define-syntax (rhombus-body stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ e::definition . tail)
     #`(begin
         (begin . e.parsed)
         (rhombus-body . tail))]
    [(_ e::expression . tail)
     #`(begin
         (#%expression e.parsed)
         (rhombus-body . tail))]))
]

Here's the definition of the @racket[:expression] syntax class:

@racketblock[
  (define-enforest
    #:syntax-class :expression
    #:prefix-more-syntax-class :prefix-op+expression+tail
    #:infix-more-syntax-class :infix-op+expression+tail
    #:desc "expression"
    #:operator-desc "expression operator"
    #:in-space in-expression-space
    #:name-path-op '|.|
    #:prefix-operator-ref expression-prefix-operator-ref
    #:infix-operator-ref expression-infix-operator-ref
    #:check-result check-expression-result
    #:make-identifier-form make-identifier-expression)
]

Expressions use the default mapping space, so @racket[in-expression-space] is
just the identity function. The @racket[expression-prefix-operator-ref] and
@racket[expression-infix-operator-ref] accessors are analogous to
@racket[definition-transformer-ref], but for expression prefix and infix
operators.

An infix expression operator like @racket[+] is defined roughly like this:

@RACKETBLOCK[
(provide (rename-out [rhombus+ +])) (code:comment "and similar for `rhombus-`, etc.")

(define-syntax rhombus+
  (expression-infix-operator #'rhombus+
                             (list (cons #'rhombus* 'weaker)
                                   (cons #'rhombus/ 'weaker)
                                   (cons #'rhombus- 'same))
                             'automatic
                             (lambda (form1 form2 stx)
                                (code:comment "this is where we compile to Racket's `+`:")
                                (quasisyntax/loc stx (+ #,form1 #,form2)))
                             'left))
]

The actual implementation has more layers of abstraction, deals with
macro scope introductions, supports a @racket[define*]-like forward
definition form, implements more complicated syntax, and so on. Some
part of the language would be built in this low-level way, including
operator- and macro-defining forms like @rhm-operator and
@rhm-expr-macro, and then more of @Rhombus could be built using @|Rhombus|.

