#lang racket/base
(require syntax/parse/pre
         "realm.rkt"
         "parens-sc.rkt")

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
    #:datum-literals (group multi)
    [(_::parens (group (_::parens (group key) (group val))) ...)
     #'((key val) ...)]
    [(group t) (pack-static-infos who #'t)]
    [(multi (group t)) (pack-static-infos who #'t)]
    [_ (raise-arguments-error* who rhombus-realm
                               "ill-formed unpacked static infos"
                               "syntax object" stx)]))
