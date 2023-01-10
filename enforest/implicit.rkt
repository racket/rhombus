#lang racket/base
(require syntax/parse/pre)

;; Default implicit handling for `define-enforest`

(provide select-prefix-implicit
         select-infix-implicit
         juxtapose-implicit-name)

;; implicit prefix operator names:
(define parens-implicit-name   '#%parens)      ; parentheses not after an expression
(define brackets-implicit-name '#%brackets)    ; square brackets not after an expression
(define braces-implicit-name   '#%braces)      ; curly braces not after an expression
(define quotes-implicit-name   '#%quotes)      ; quotes
(define block-implicit-name    '#%block)       ; colon
(define alts-implicit-name     '#%alts)        ; vertical bars
(define literal-implicit-name  '#%literal)     ; numbers, strings, etc.

;; implicit infix operator names:
(define call-implicit-name      '#%call)      ; parentheses adjacent to preceding expression
(define ref-implicit-name       '#%ref)       ; square brackets adjacent to preceding expression
(define comp-implicit-name      '#%comp)      ; curly braces adjacent to preceding expression
(define juxtapose-implicit-name '#%juxtapose) ; other exprs with no operator between

;; A selector function takes a term that determines the implicit, and
;; it returns a symbol for the implicit name plus a syntax object to
;; provide lexical context.

(define (select-prefix-implicit head)
  (syntax-parse head
    [((~and tag (~datum parens)) . _)
     (values parens-implicit-name #'tag)]
    [((~and tag (~datum brackets)) . _)
     (values brackets-implicit-name #'tag)]
    [((~and tag (~datum braces)) . _)
     (values braces-implicit-name #'tag)]
    [((~and tag (~datum quotes)) . _)
     (values quotes-implicit-name #'tag)]
    [((~and tag (~datum block)) . _)
     (values block-implicit-name #'tag)]
    [((~and tag (~datum alts)) . _)
     (values alts-implicit-name #'tag)]
    [_
     (values literal-implicit-name head)]))

(define (select-infix-implicit head)
  (syntax-parse head
    [((~and tag (~datum parens)) . _)
     (values call-implicit-name #'tag)]
    [((~and tag (~datum brackets)) . _)
     (values ref-implicit-name #'tag)]
    [((~and tag (~datum braces)) . _)
     (values comp-implicit-name #'tag)]
    [((~and tag (~datum block)) . _)
     (values juxtapose-implicit-name #'tag)]
    [((~and tag (~datum alts)) . _)
     (values juxtapose-implicit-name #'tag)]
    [_
     (values juxtapose-implicit-name head)]))
