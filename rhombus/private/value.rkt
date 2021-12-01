#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     shrubbery/print
                     shrubbery/property
                     "infer-name.rkt")
         "definition.rkt"
         "binding.rkt"
         "parse.rkt"
         "implicit.rkt"
         "annotation.rkt"
         "call-result-key.rkt"
         "static-info.rkt"
         "forwarding-sequence.rkt")

(provide val
         (rename-out [rhombus-let let]))

(module+ for-define
  (provide (for-syntax build-value-definitions
                       build-values-definitions)))

(define-for-syntax (make-val #:wrap-definition [wrap-definition values]
                             #:check-context [check-context void])
  (definition-transformer
    (lambda (stx)
      (check-context stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts op)
        [(form-id (~optional (~literal values)) (parens g ...) (~and rhs (block body ...)))
         (build-values-definitions #'form-id
                                   #'(g ...) #'rhs
                                   wrap-definition)]
        [(form-id any ... (~and rhs (block body ...)))
         #:with g-tag (syntax-raw-property (datum->syntax #f 'group) "")
         (build-value-definitions #'form-id
                                  #'(g-tag any ...)
                                  #'rhs
                                  wrap-definition)]))))

(define-syntax val
  (make-val))

(define-syntax rhombus-let
  (make-val #:wrap-definition (lambda (defn) #`(rhombus-forward #,defn))
            #:check-context (lambda (stx)
                              (when (eq? (syntax-local-context) 'top-level)
                                (raise-syntax-error #f
                                                    "not allowed in a top-level context"
                                                    stx)))))

(define-for-syntax (build-value-definitions form-id g-stx rhs-stx wrap-definition)
  (syntax-parse g-stx
    [lhs::binding
     #:with lhs-e::binding-form #'lhs.parsed
     #:with rhs (rhombus-local-expand (enforest-expression-block rhs-stx))
     #:with static-infos (extract-static-infos #'rhs)
     #:with lhs-impl::binding-impl #'(lhs-e.infoer-id static-infos lhs-e.data)
     #:with lhs-i::binding-info #'lhs-impl.info
     (list
      #'(define tmp-id (let ([lhs-i.name-id rhs])
                         lhs-i.name-id))
      #`(lhs-i.matcher-id tmp-id
                          lhs-i.data
                          flattened-if
                          (void)
                          (rhs-binding-failure '#,form-id tmp-id '#,(shrubbery-syntax->string #'lhs)))
      (wrap-definition
       #`(begin
           (lhs-i.binder-id tmp-id lhs-i.data)
           (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
           ...)))]))

(define-for-syntax (build-values-definitions form-id gs-stx rhs-stx wrap-definition)
  (syntax-parse gs-stx
    [(lhs::binding ...)
     #:with (lhs-e::binding-form ...) #'(lhs.parsed ...)
     #:with rhs rhs-stx
     #:with (lhs-impl::binding-impl ...) #'((lhs-e.infoer-id () lhs-e.data) ...)
     #:with (lhs-i::binding-info ...) #'(lhs-impl.info ...)
     #:with (tmp-id ...) (generate-temporaries #'(lhs-i.name-id ...))
     #:with (lhs-str ...) (for/list ([lhs (in-list (syntax->list #'(lhs ...)))])
                            (shrubbery-syntax->string lhs))
     (list
      #'(define-values (tmp-id ...) (let-values ([(lhs-i.name-id ...) (rhombus-body-expression rhs)])
                                      (values lhs-i.name-id ...)))
     (wrap-definition
      #`(begin
          (lhs-i.matcher-id tmp-id
                            lhs-i.data
                            flattened-if
                            (begin)
                            (rhs-binding-failure '#,form-id tmp-id 'lhs-str))
          ...
          (lhs-i.binder-id tmp-id lhs-i.data)
          ...
          (begin
            (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
            ...)
          ...)))]))

(define-syntax (flattened-if stx)
  (syntax-parse stx
    [(_ check-expr success-expr fail-expr)
     #'(begin
         (unless check-expr fail-expr)
         success-expr)]))

(define (rhs-binding-failure who val binding-str)
  (raise-binding-failure who "value" val binding-str))
