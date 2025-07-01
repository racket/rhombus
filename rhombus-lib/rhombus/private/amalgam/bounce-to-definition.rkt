#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "parse.rkt")

(provide (for-syntax bounce-to-definition))

(define-for-syntax (bounce-to-definition macro-id stx)
  (syntax-parse stx
    [(head . tail)
     #`((group #,(relocate-id #'head macro-id) . tail))]))

