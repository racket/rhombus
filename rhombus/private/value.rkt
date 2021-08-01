#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "infer-name.rkt")
         "definition.rkt"
         "binding.rkt"
         "parse.rkt")

(provide value)

(module+ for-define
  (provide (for-syntax build-value-definitions
                       build-values-definitions)))

(define-syntax value
  (definition-transformer
    (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block alts op)
       [(form-id (~optional (~literal values)) (parens g ...) (~and rhs (block body ...)))
        (build-values-definitions #'form-id
                                  #'(g ...) #'rhs)]
       [(form-id any ... (~and rhs (block body ...)))
        (build-value-definitions #'form-id
                                 #'(group any ...)
                                 #'rhs)]))))

(define-for-syntax (build-value-definitions form-id g-stx rhs-stx)
  (syntax-parse g-stx
    [lhs::binding
     #:with lhs-e::binding-form #'lhs.parsed
     #:with rhs rhs-stx
     (list
      #'(define tmp-id (let ([lhs-e.arg-id (rhombus-expression (group rhs))])
                         lhs-e.arg-id))
      #`(lhs-e.matcher-id tmp-id
                          lhs-e.data
                          flattened-if
                          (void)
                          (rhs-binding-failure '#,form-id tmp-id 'lhs))
      #`(lhs-e.binder-id tmp-id lhs-e.data))]))

(define-for-syntax (build-values-definitions form-id gs-stx rhs-stx)
  (syntax-parse gs-stx
    [(lhs::binding ...)
     #:with (lhs-e::binding-form ...) #'(lhs.parsed ...)
     #:with rhs rhs-stx
     #:with (tmp-id ...) (generate-temporaries #'(lhs-e.arg-id ...))
     (list
      #'(define-values (tmp-id ...) (let-values ([(lhs-e.arg-id ...) (rhombus-expression (group rhs))])
                                      (values lhs-e.arg-id ...)))
      #`(begin
          (lhs-e.matcher-id tmp-id
                            lhs-e.data
                            flattened-if
                            (begin)
                            (rhs-binding-failure '#,form-id tmp-id 'lhs))
          ...
          (lhs-e.binder-id tmp-id lhs-e.data)
          ...))]))

(define-syntax (flattened-if stx)
  (syntax-parse stx
    [(_ check-expr success-expr fail-expr)
     #'(begin
         (unless check-expr fail-expr)
         success-expr)]))

(define (rhs-binding-failure who val binding)
  (raise-binding-failure who "value" val binding))
