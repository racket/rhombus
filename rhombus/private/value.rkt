#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "infer-name.rkt")
         "definition.rkt"
         "binding.rkt"
         "parse.rkt"
         "implicit.rkt"
         "contract.rkt"
         (submod "struct.rkt" for-call))

(provide val)

(module+ for-define
  (provide (for-syntax build-value-definitions
                       build-values-definitions)))

(define-syntax val
  (definition-transformer
    (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block alts op)
       [(form-id (~optional (~literal values)) (parens g ...) (~and rhs (block body ...)))
        (build-values-definitions #'form-id
                                  #'(g ...) #'rhs
                                  values)]
       [(form-id any ... (~and rhs (block body ...)))
        (build-value-definitions #'form-id
                                 #'(group any ...)
                                 #'rhs
                                 values)]))))

(define-for-syntax (build-value-definitions form-id g-stx rhs-stx wrap-definition)
  (syntax-parse (maybe-add-struct-contract g-stx rhs-stx)
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
      (wrap-definition
       #`(lhs-e.binder-id tmp-id lhs-e.data)))]))

(define-for-syntax (build-values-definitions form-id gs-stx rhs-stx wrap-definition)
  (syntax-parse gs-stx
    [(lhs::binding ...)
     #:with (lhs-e::binding-form ...) #'(lhs.parsed ...)
     #:with rhs rhs-stx
     #:with (tmp-id ...) (generate-temporaries #'(lhs-e.arg-id ...))
     (list
      #'(define-values (tmp-id ...) (let-values ([(lhs-e.arg-id ...) (rhombus-expression (group rhs))])
                                      (values lhs-e.arg-id ...)))
     (wrap-definition
      #`(begin
          (lhs-e.matcher-id tmp-id
                            lhs-e.data
                            flattened-if
                            (begin)
                            (rhs-binding-failure '#,form-id tmp-id 'lhs))
          ...
          (lhs-e.binder-id tmp-id lhs-e.data)
          ...)))]))

(define-syntax (flattened-if stx)
  (syntax-parse stx
    [(_ check-expr success-expr fail-expr)
     #'(begin
         (unless check-expr fail-expr)
         success-expr)]))

(define (rhs-binding-failure who val binding)
  (raise-binding-failure who "value" val binding))

(define-for-syntax (maybe-add-struct-contract g-stx rhs-stx)
  ;; Ad hoc contracting rule: if we have a plain identifier
  ;; and the right-hand side is a struct constrcution, then
  ;; add the structure type a contract on the variable
  (syntax-parse g-stx
    #:datum-literals (group)
    [(group id:identifier)
     (syntax-parse rhs-stx
       #:datum-literals (block group parens)
       [(block (group rator:identifier ((~and tag parens) . _)))
        #:when (and (syntax-local-struct-contract #'rator)
                    (free-identifier=? #'#%call (datum->syntax #'tag '#%call)))
        #'(group id (op ::) rator)]
       [_ g-stx])]
    [_ g-stx]))
