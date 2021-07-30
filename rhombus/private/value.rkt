#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "infer-name.rkt")
         "definition.rkt"
         "binding.rkt"
         "parse.rkt"
         "nested-bindings.rkt")

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
        (build-values-definitions #'(g ...) #'rhs)]
       [(form-id any ... (~and rhs (block body ...)))
        (build-value-definitions #'(group any ...)
                                 #'rhs)]))))

(define-for-syntax (build-value-definitions g-stx rhs-stx)
  (syntax-parse g-stx
    [lhs::binding
     #:with lhs-e::binding-form #'lhs.parsed
     #:with name-id (infer-name #'lhs-e.var-ids)
     #:with rhs rhs-stx
     (list
      #'(define-values lhs-e.var-ids
          (let ([tmp-id (let ([name-id (rhombus-expression (group rhs))])
                          name-id)])
            (let-values ([(match? . lhs-e.var-ids)
                          (lhs-e.check-proc-expr tmp-id)])
              (unless match?
                (rhs-binding-failure 'form-id tmp-id 'lhs))
              (values . lhs-e.var-ids))))
      #'lhs-e.post-defn)]))

(define-for-syntax (build-values-definitions gs-stx rhs-stx)
  (syntax-parse gs-stx
    [(lhs::binding ...)
     #:with (lhs-e::binding-form ...) #'(lhs.parsed ...)
     #:with (name-id ...) (map infer-name (syntax->list #'(lhs-e.var-ids ...)))
     #:with (tmp-id ...) (generate-temporaries #'(name-id ...))
     #:with rhs rhs-stx
     (list
      #'(define-values (lhs-e.var-id ... ...)
          (let-values ([(tmp-id ...)
                        (let-values ([(name-id ...) (rhombus-expression (group rhs))])
                          (values name-id ...))])
            (nested-bindings
             form-id
             #f rhs-binding-failure
             (begin)
             (tmp-id lhs-e lhs #f)
             ...
             (values lhs-e.var-id ... ...))))
      #'(begin
          lhs-e.post-defn ...))]))

(define (rhs-binding-failure who val binding)
  (raise-binding-failure who "value" val binding))
