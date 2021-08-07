#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "infer-name.rkt")
         "definition.rkt"
         "binding.rkt"
         "parse.rkt"
         "implicit.rkt"
         "contract.rkt"
         "result.rkt"
         "static-info.rkt")

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
      (wrap-definition
       (or (generate-dot-provider-binding #'lhs-e.matcher-id #'lhs-e.arg-id #'tmp-id rhs-stx)
           #`(lhs-e.binder-id tmp-id lhs-e.data))))]))

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

(begin-for-syntax
  (define-syntax-class :simple-rator
    #:datum-literals (group parens)
    (pattern rator:identifier)
    (pattern ((~and tag parens) (group f::simple-rator))
             #:when (free-identifier=? #'#%parens (datum->syntax #'tag '#%parens))
             #:attr rator #'f.rator))
  (define-syntax-class :simple-call
    #:datum-literals (group parens)
    (pattern (group r::simple-rator ((~and tag parens) . _))
             #:attr rator #'r.rator)
    (pattern (group ((~and p-tag parens) (group f::simple-call)))
             #:when (free-identifier=? #'#%parens (datum->syntax #'p-tag '#%parens))
             #:attr tag #'f.tag
             #:attr rator #'f.rator)))

(define-for-syntax (generate-dot-provider-binding matcher-id bind-id tmp-id rhs-stx)
  (cond
    [(free-identifier=? matcher-id #'identifier-succeed)
     (syntax-parse rhs-stx
       #:datum-literals (block)
       [(block f::simple-call)
        #:when (free-identifier=? #'#%call (datum->syntax #'f.tag '#%call))
        #:do [(define result-static-infos (or (syntax-local-static-info #'f.rator #'#%result)
                                              #'()))]
        #`(begin
            (define #,bind-id #,tmp-id)
            (define-static-info-syntax/maybe #,bind-id . #,result-static-infos))]
       [_ #f])]
    [else #f]))
