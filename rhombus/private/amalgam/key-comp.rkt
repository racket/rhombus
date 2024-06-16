#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "introducer.rkt"))

(provide define-key-comp-syntax)

(begin-for-syntax
  (provide (struct-out key-comp)
           key-comp-maker
           in-key-comp-space
           key-comp-ref)
  
  (define in-key-comp-space (make-interned-syntax-introducer/add 'rhombus/key_comp))

  (struct key-comp (name-sym map?-id
                             map-build-id map-pair-build-id map-for-form-id
                             mutable-map?-id mutable-map-build-id
                             weak-mutable-map?-id weak-mutable-map-build-id
                             empty-stx
                             set?-id
                             set-build-id set-build*-id set-for-form-id
                             mutable-set?-id mutable-set-build-id
                             weak-mutable-set?-id weak-mutable-set-build-id))

  (struct key-comp-maker (proc))

  (define (key-comp-ref v) (or (and (key-comp? v) v)
                               (and (key-comp-maker? v)
                                    ((key-comp-maker-proc v))))))

(define-syntax (define-key-comp-syntax stx)
  (syntax-parse stx
    [(_ id rhs)
     #`(define-syntax #,(in-key-comp-space #'id) rhs)]))
