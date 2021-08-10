#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/unsafe/undefined
         "binding.rkt"
         "parse.rkt"
         "forwarding-sequence.rkt"
         "composite.rkt")

(provide nested-bindings)

(define-syntax (nested-bindings stx)
  (syntax-parse stx
    [(_ who try-next failure rest body)
     (syntax-parse #'rest
       [#f #'(let () body)]
       [(rest-id rest-getter-id rest::binding-form rest-pat)
        #`(let ([rest-getter-id
                 #,(make-rest-match #'rest-id #'(rest.bind-id ...) #'values
                                    #'rest.arg-id #'rest.matcher-id #'rest.binder-id
                                    #'rest.data
                                    #'(lambda (arg)
                                        (static-if try-next
                                                   #f
                                                   (failure 'who arg 'rest-pat))))])
            (if (static-if try-next
                           rest-getter-id
                           #t)
                (let ()
                  body)
                (static-if try-next
                           (try-next)
                           (failure 'who rest-id 'list))))])]
    [(_ who try-next failure (arg-id arg::binding-form arg-pat arg-default) rest . tail)
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
                         (nested-bindings who try-next failure rest . tail)
                         (static-if try-next
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

(define-syntax (static-if stx)
  (syntax-parse stx
    [(_ #f thn els) #'els]
    [(_ _ thn els) #'thn]))
