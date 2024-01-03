#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print)
         racket/unsafe/undefined
         "binding.rkt"
         "parse.rkt"
         "forwarding-sequence.rkt"
         "composite.rkt"
         "static-info.rkt")

(provide nested-bindings)

(define-syntax (nested-bindings stx)
  (syntax-parse stx
    [(_ who try-next failure #f ... body) #'(let () body)]
    [(_ who try-next failure #f ... (arg-id arg::binding-info arg-pat arg-default) . tail)
     #:with arg-rhs (if (syntax-e #'arg-default)
                        #'(if (eq? arg-id unsafe-undefined)
                              (let ([arg-info.name-id (rhombus-expression arg-default)])
                                arg-info.name-id)
                              arg-id)
                        #'arg-id)
     #`(let ([arg-id arg-rhs])
         (arg.matcher-id arg-id
                         arg.data
                         if-block
                         #,(if (syntax-e #'try-next)
                               #`(nested-bindings who try-next failure . tail)
                               #`(begin
                                   (arg.committer-id arg-id arg.data)
                                   (arg.binder-id arg-id arg.data)
                                   (begin
                                     (define-static-info-syntax/maybe arg.bind-id arg.bind-static-info ...)
                                     ...)
                                   (nested-bindings who try-next failure . tail)))
                         (static-if try-next
                                    (try-next)
                                    (failure 'who arg-id 'arg.annotation-str))))]))

(define-syntax (if-block stx)
  (syntax-parse stx
    [(_ #t thn els) #`(racket-block thn)]
    [(_ #f thn els) #`(racket-block els)]
    [(_ tst thn els)
     #`(if tst (racket-block thn) (racket-block els))]))

(define-syntax (racket-block stx)
  (syntax-parse stx
    [(_ e)
     #`(let ()
         (rhombus-block-forwarding-sequence
          #:orig #,stx
          e))]))

(define-syntax (static-if stx)
  (syntax-parse stx
    [(_ #f thn els) #'els]
    [(_ _ thn els) #'thn]))
