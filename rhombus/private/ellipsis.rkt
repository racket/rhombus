#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse)
         "provide.rkt"
         "placeholder.rkt"
         "expression.rkt")

(provide (for-spaces (#f
                      rhombus/bind)
                     (rename-out [rhombus... ...])))

(module+ for-parse
  (provide (for-syntax consume-extra-ellipses)))

(define-placeholder-syntax rhombus...
  "misuse outside of a constructor"
  "misuse outside of a binding")

(define-for-syntax (consume-extra-ellipses stx)
  (let loop ([stx stx] [count 0])
    (syntax-parse stx
      #:datum-literals (group)
      [((group op::name) . gs)
       #:when (free-identifier=? #'op.name
                                 (expr-quote rhombus...))
       (loop #'gs (add1 count))]
      [_ (values stx count)])))
