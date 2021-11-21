#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         (only-in rhombus
                  def val fun ::)
         (only-in "rhombus.rhm"
                  rhombusblock)
         (only-in rhombus/parse
                  rhombus-expression))

(provide typeset-doc)

(define-syntax (typeset-doc stx)
  (syntax-parse stx
    #:datum-literals (parens group brackets)
    [(_ (parens (group form ...) ...
                (group
                 (brackets content-group ...))))
     (define forms (syntax->list #'((group form ...) ...)))
     (define vars (for/fold ([vars #hasheq()]) ([form (in-list forms)])
                    (extract-metavariables form vars)))
     #`(let-syntax #,(for/list ([id (in-hash-values vars)])
                       `[#,id (rhombus-metavariable
                               (quote-syntax (racketvarfont #,(symbol->string (syntax-e id)))))])
         (list
          (rhombus-expression (group rhombusblock (block (group form ...))))
          ...
          (rhombus-expression content-group)
          ...))]))

(define-for-syntax (extract-metavariables stx vars)
  (syntax-parse stx
    #:literals (def val fun ::)
    #:datum-literals (parens group)
    [(group def id:identifier (parens g ...) . _)
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-binding-metavariables g vars))]
    [(group def id:identifier . _) vars]
    [(group val id:identifier . _) vars]
    [_ vars]))

(define-for-syntax (extract-binding-metavariables stx vars)
  (syntax-parse stx
    #:literals (def val fun ::)
    #:datum-literals (parens group)
    [(group id :: _)
     (hash-set vars (syntax-e #'id) (or (hash-ref vars (syntax-e #'id) #f) #'id))]
    [_ vars]))
