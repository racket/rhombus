#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
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
    [(_ who try-next failure rest body)
     (syntax-parse #'rest
       [#f #'(let () body)]
       [(rest-getter-id rest-pat rest-id rest-info::binding-info)
        #`(let ([rest-getter-id
                 #,(make-rest-match #'rest-id #'values #'rest-info
                                    #`(lambda (arg)
                                        (static-if try-next
                                                   #f
                                                   (failure 'who arg 'rest-info.annotation-str))))])
            (if (static-if try-next
                           rest-getter-id
                           #t)
                (let ()
                  body)
                (static-if try-next
                           (try-next)
                           (failure 'who rest-id 'list))))])]
    [(_ who try-next failure (arg-id arg::binding-info arg-pat arg-default) rest . tail)
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
                               #`(nested-bindings who try-next failure rest . tail)
                               #`(begin
                                   (arg.binder-id arg-id arg.data)
                                   (begin
                                     (define-static-info-syntax/maybe arg.bind-id arg.bind-static-info ...)
                                     ...)
                                   (nested-bindings who try-next failure rest . tail)))
                         (static-if try-next
                                    (try-next)
                                    (failure 'who arg-id 'arg.annotation-str))))]))

(define-syntax (if-block stx)
  (syntax-parse stx
    [(_ tst thn els)
     #`(if tst (racket-block thn) (racket-block els))]))

(define-syntax (racket-block stx)
  (syntax-parse stx
    [(_ e)
     #`(let ()
         (rhombus-forwarding-sequence
          #:block #:need-end-expr #,stx
          e))]))

(define-syntax (static-if stx)
  (syntax-parse stx
    [(_ #f thn els) #'els]
    [(_ _ thn els) #'thn]))
