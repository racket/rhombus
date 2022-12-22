#lang racket/base
(require syntax/parse
         "pack.rkt")

(provide unpack-uses
         pack-uses)

(define (unpack-uses v)
  (syntax-parse v
    [(stx ...)
     #'(brackets (group stx) ...)]))

(define (pack-uses v who)
  (syntax-parse v
    #:datum-literals (brackets group)
    [(brackets (group stx) ...)
     #'(stx ...)]))
