#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "op-literal.rkt"
         "parens.rkt"
         (only-in "underscore.rkt"
                  [_ rhombus-_]))

(provide (for-syntax simple-call?
                     normal-call?
                     normal-call-repetition?
                     install-normal-call?!))

(define-for-syntax normal-call? #f)
(define-for-syntax normal-call-repetition? #f)

(define-for-syntax (install-normal-call?! proc repet-proc)
  (set! normal-call? proc)
  (set! normal-call-repetition? repet-proc))

(define-for-syntax (simple-call? stx
                                 #:ellipsis-ok? [ellipsis-ok? #f]
                                 #:repetition? [repetition? #f])
  (syntax-parse stx
    [(_ (tag arg-g ...) . _)
     (and (if repetition?
              (normal-call-repetition? #'tag)
              (normal-call? #'tag))
          (for/and ([arg-g (in-list (syntax->list #'(arg-g ...)))])
            (syntax-parse arg-g
              #:literals (rhombus-_)
              #:datum-literals (group)
              [(group _::&-expr . _) #f]
              [(group _::~&-expr . _) #f]
              [(group _:keyword (~optional (_::block . _))) #f]
              [(group _::...-expr) ellipsis-ok?]
              [(group rhombus-_) #f]
              [(group . _) #t])))]
    [_ #f]))
