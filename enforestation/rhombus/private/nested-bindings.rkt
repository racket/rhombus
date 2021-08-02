#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/unsafe/undefined
         "binding.rkt"
         "parse.rkt"
         "forwarding-sequence.rkt")

(provide nested-bindings)

(define-syntax (nested-bindings stx)
  (syntax-parse stx
    [(_ who try-next failure body) #'(let () body)]
    [(_ who try-next failure (arg-id arg::binding-form arg-pat arg-default) . tail)
     #:with arg-rhs (if (syntax-e #'arg-default)
                        #'(if (eq? arg-id unsafe-undefined)
                              (let ([arg.arg-id (rhombus-expression arg-default)])
                                arg.arg-id)
                              arg-id)
                        #'arg-id)
     #`(let ([arg-id arg-rhs])
         (arg.matcher-id arg-id
                         arg.data
                         if-block
                         (nested-bindings who try-next failure . tail)
                         (if try-next
                             (try-next)
                             (failure 'who arg-id 'arg-pat))))]))

(define-syntax (if-block stx)
  (syntax-parse stx
    [(_ tst thn els)
     #`(if tst (racket-block thn) (racket-block els))]))

(define-syntax (racket-block stx)
  (syntax-parse stx
    [(_ e)
     #`(let ()
         (rhombus-forwarding-sequence
          #:need-end-expr #,stx
          e))]))
