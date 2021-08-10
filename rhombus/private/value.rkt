#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "infer-name.rkt")
         "definition.rkt"
         "binding.rkt"
         "parse.rkt"
         "implicit.rkt"
         "contract.rkt"
         "call-result-key.rkt"
         "indexed-ref-key.rkt"
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
       (with-syntax ([((_ . static-infos) ...)
                      (extend-bind-input (syntax->list #'lhs-e.bind-ids)
                                         (infer-static-infos rhs-stx))])
           #`(begin
               (lhs-e.binder-id tmp-id lhs-e.data)
               (define-static-info-syntax/maybe lhs-e.bind-id . static-infos)
               ...))))]))

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
          ...
          (begin
            (define-static-info-syntax/maybe lhs-e.bind-id lhs-e.bind-static-info ...)
            ...)
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
             #:attr rator #'f.rator))

  (define-syntax-class :simple-indexed
    #:datum-literals (group parens)
    (pattern indexed:identifier)
    (pattern ((~and tag parens) (group i::simple-indexed))
             #:when (free-identifier=? #'#%parens (datum->syntax #'tag '#%parens))
             #:attr indexed #'i.indexed))
  (define-syntax-class :simple-ref
    #:datum-literals (group brackets)
    (pattern (group i::simple-indexed ((~and tag brackets) . _))
             #:attr indexed #'i.indexed)
    (pattern (group ((~and p-tag brackets) (group f::simple-call)))
             #:when (free-identifier=? #'#%array (datum->syntax #'p-tag '#%array))
             #:attr tag #'i.tag
             #:attr indexed #'i.indexed)))

(define-for-syntax (infer-static-infos rhs-stx)
  (syntax-parse rhs-stx
    #:datum-literals (block brackets)
    [(block f::simple-call)
     #:when (free-identifier=? #'#%call (datum->syntax #'f.tag '#%call))
     (or (syntax-local-static-info #'f.rator #'#%call-result)
         #'())]
    [(block ((~and tag brackets) . _))
     #'((#%indexed-ref list-ref))]
    [(block a::simple-ref)
     #:when (free-identifier=? #'#%array (datum->syntax #'a.tag '#%array))
     (or (syntax-local-static-info #'a.indexed #'#%ref-result)
         #'())]
    [_ #'()]))
