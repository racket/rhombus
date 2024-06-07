#lang racket/base
(require syntax/parse/pre
         "realm.rkt"
         "parens-sc.rkt")

(provide unpack-uses
         pack-uses)

(define (unpack-uses who stx)
  (syntax-parse stx
    [(stx ...)
     #'(brackets (group stx) ...)]
    [_ (raise-arguments-error* who rhombus-realm
                               "ill-formed unpacked uses"
                               "syntax object" stx)]))

(define (pack-uses who stx)
  (syntax-parse stx
    #:datum-literals (group)
    [(_::brackets (group stx) ...)
     #'(stx ...)]
    [_ (raise-arguments-error* who rhombus-realm
                               "ill-formed packed uses"
                               "syntax object" stx)]))
