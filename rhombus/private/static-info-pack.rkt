#lang racket/base
(require syntax/parse/pre
         "realm.rkt")

(provide unpack-static-infos
         pack-static-infos)

(define (unpack-static-infos who stx)
  (syntax-parse stx
    [((key val) ...)
     #'(parens (group (parens (group key) (group val))) ...)]
    [_ (raise-arguments-error* who rhombus-realm
                               "ill-formed packed static infos"
                               "syntax object" stx)]))

(define (pack-static-infos who stx)
  (syntax-parse stx
    #:datum-literals (parens group)
    [(parens (group (parens (group key) (group val))) ...)
     #'((key val) ...)]
    [_ (raise-arguments-error* who rhombus-realm
                               "ill-formed unpacked static infos"
                               "syntax object" stx)]))
