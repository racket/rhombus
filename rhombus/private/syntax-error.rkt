#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print))

(provide (for-syntax Syntax.error))

(begin-for-syntax
  (define Syntax.error
    (case-lambda
      [(form) (raise-syntax-error (name-of form) "bad syntax" (unwrap form))]
      [(msg form) (raise-syntax-error (name-of form) msg (unwrap form))]
      [(msg form detail) (raise-syntax-error (name-of form) msg (unwrap form) (unwrap detail))]))

  (define (name-of stx)
    (syntax-parse stx
      #:datum-literals (group)
      [who:identifier (string->symbol (shrubbery-syntax->string #'who))]
      [(group who:identifier . _) (name-of #'who)]
      [(group . _) '?]
      [(multi (group who:identifier . _) . _) (name-of #'who)]
      [(multi . _) '?]
      [else #f]))

  (define (unwrap stx)
    stx))
