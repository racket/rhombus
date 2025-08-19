#lang racket/base
(require syntax/parse/pre
         "realm.rkt"
         "parens-sc.rkt")

(provide unpack-uses
         pack-uses)

(define (unpack-uses who stx)
  (syntax-parse stx
    [(stx ...)
     #:with (g ...) (for/list ([stx (in-list (syntax->list #'(stx ...)))])
                      (syntax-parse stx
                        [(#:repet (id ...))
                         #'(group #:repet (parens (group (parsed #:rhombus/sequencer id)) ...))]
                        [kw:keyword
                         #'(group kw)]
                        [(#:group g)
                         #'g]))
     #'(brackets g ...)]
    [_ (raise-arguments-error* who rhombus-realm
                               "ill-formed unpacked uses"
                               "syntax object" stx)]))

(define (pack-uses who stx)
  (syntax-parse stx
    #:datum-literals (group)
    [(_::brackets g ...)
     #:with (stx ...) (for/list ([g (in-list (syntax->list #'(g ...)))])
                        (syntax-parse g
                          #:datum-literals (group parens parsed)
                          [(group #:repet (parens (group (parsed #:rhombus/sequencer id:identifier)) ...))
                           #'(#:repet (id ...))]
                          [(group kw:keyword) #'kw]
                          [_ #`(#:group #,g)]))
     #'(stx ...)]
    [_ (raise-arguments-error* who rhombus-realm
                               "ill-formed packed uses"
                               "syntax object" stx)]))
