#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide (for-syntax Syntax.error))

(begin-for-syntax
  (define Syntax.error
    (case-lambda
      [(form) (raise-syntax-error (name-of form) "bad syntax" (unwrap form))]
      [(msg form) (raise-syntax-error (name-of form) msg (unwrap form))]
      [(msg form detail) (raise-syntax-error (name-of form) msg (unwrap form) (unwrap detail))]))

  (define (name-of stx)
    (syntax-parse stx
      #:datum-literals (parens)
      [(parens (group who:identifier . _) . _) (syntax-e #'who)]
      [else #f]))

  (define (unwrap stx)
    (syntax-parse stx
      #:datum-literals (parens)
      [(parens d ...) #'(d ...)]
      [else stx])))



