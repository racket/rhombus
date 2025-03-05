#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     "dotted-sequence.rkt")
         "parens.rkt")

(provide (for-syntax extract-name
                     extract-who))

(define-for-syntax (extract-name reflect-name-stx stx)
  (cond
    [(not reflect-name-stx) #f]
    [else
     (syntax-parse reflect-name-stx
       #:datum-literals (group)
       [(group _ (_::block (group n::dotted-operator-or-identifier-sequence)))
        (build-dot-symbol (syntax->list #'n) #:skip-dots? #t)]
       [(group _ n::dotted-operator-or-identifier-sequence)
        (build-dot-symbol (syntax->list #'n) #:skip-dots? #t)]
       [_ (raise-syntax-error #f "expected a name" stx reflect-name-stx)])]))

(define-for-syntax (extract-who who-stx stx)
  (cond
    [(not who-stx) #f]
    [else
     (syntax-parse who-stx
       #:datum-literals (group)
       [(group _ (_::block (group n::name))) #'n.name]
       [(group _ n::name) #'n.name])]))
