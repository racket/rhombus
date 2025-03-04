#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/unsafe/undefined
         "binding.rkt"
         "parse.rkt"
         "static-info.rkt"
         "if-blocked.rkt")

(provide nested-bindings)

(define-syntax (nested-bindings stx)
  (syntax-parse stx
    [(_ who try-next eager-bind? failure
        (arg-id arg-info arg-pat arg-default) ...
        body)
     (for/foldr ([next #'(let () body)])
                ([arg-id (in-list (syntax->list #'(arg-id ...)))]
                 [arg-info (in-list (syntax->list #'(arg-info ...)))]
                 [arg-default (in-list (syntax->list #'(arg-default ...)))])
       (syntax-parse arg-info
         [arg::binding-info
          #:with arg-id arg-id
          #:with arg-default arg-default
          #:with arg-rhs (if (syntax-e #'arg-default)
                             #'(if (eq? arg-id unsafe-undefined)
                                   (let ([arg.name-id (rhombus-expression arg-default)])
                                     arg.name-id)
                                   arg-id)
                             #'arg-id)
          #`(let ([arg-id arg-rhs])
              (arg.matcher-id arg-id
                              arg.data
                              if/blocked
                              #,(if (not (syntax-e #'eager-bind?))
                                    next
                                    #`(begin
                                        (arg.committer-id arg-id arg.evidence-ids arg.data)
                                        (arg.binder-id arg-id arg.evidence-ids arg.data)
                                        (define-static-info-syntax/maybe arg.bind-id arg.bind-static-info ...)
                                        ...
                                        #,next))
                              #,(if (syntax-e #'try-next)
                                    #'(try-next)
                                    #'(failure 'who arg-id 'arg.annotation-str))))]))]))
