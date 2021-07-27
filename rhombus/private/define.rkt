#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "transformer.rkt"
                     "consistent.rkt")
         "binding.rkt"
         "expression.rkt"
         "parse.rkt"
         "function.rkt"
         (submod "function.rkt" for-call)
         "quasiquote.rkt"
         (for-syntax "parse.rkt"))

(provide (rename-out [rhombus-define define]))

(begin-for-syntax
  (struct fcase (args arg-expandeds rhs))
  (define (group-by-counts fcases)
    (define ht
      (for/fold ([ht #hasheqv()]) ([fc (in-list fcases)])
        (define n (length (fcase-args fc)))
        (hash-set ht n (cons fc (hash-ref ht n '())))))
    (for/list ([sames (in-hash-values ht)])
      (reverse sames))))

(define-syntax rhombus-define
  (definition-transformer
    (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block alts op)
       [(form-id ((~and alts-tag alts) (block (group id:identifier (parens arg::binding ...)
                                                     (~and rhs (block body ...))))
                                       ...+))
        (define ids (syntax->list #'(id ...)))
        (define the-id (car ids))
        (check-consistent stx ids "name")
        (define argss (map syntax->list (syntax->list #'((arg ...) ...))))
        (define arg-expandedss (map syntax->list (syntax->list #'((arg.expanded ...) ...))))
        (define rhss (syntax->list #'(rhs ...)))
        (values
         (list
          #`(define #,the-id
              #,(build-case-function the-id argss arg-expandedss rhss #'form-id #'alts-tag)))
         null)]
       [(form-id id::non-binding-identifier ((~and parens-tag parens) arg::kw-opt-binding ...)
                 (~and rhs (block body ...)))
        #:with (arg-id ...) (generate-temporaries #'(arg ...))
        (values
         (list
          #`(define id
              #,(build-function #'id #'(arg.kw ...) #'(arg ...) #'(arg.expanded ...) #'(arg.default ...) #'rhs #'form-id #'parens-tag)))
         null)]
       [(form-id (~literal values) (parens g ...) (~and rhs (block body ...)))
        #:with (lhs::binding ...) #'(g ...)
        #:with (lhs-e::binding-form ...) #'(lhs.expanded ...)
        #:with (name-id ...) (map infer-name (syntax->list #'(lhs-e.var-ids ...)))
        #:with (tmp-id ...) (generate-temporaries #'(name-id ...))
        (values
         (list
          #'(define-values (lhs-e.var-id ... ...)
              (let-values ([(tmp-id ...)
                            (let-values ([(name-id ...) (rhombus-expression (group rhs))])
                              (values name-id ...))])
                (nested-bindings
                 form-id
                 #f
                 (begin)
                 (tmp-id lhs-e lhs)
                 ...
                 (values lhs-e.var-id ... ...))))
          #'(begin
              lhs-e.post-defn ...))
         null)]
       [(form-id any ... (~and rhs (block body ...)))
        #:with lhs::binding #'(group any ...)
        #:with lhs-e::binding-form #'lhs.expanded
        #:with name-id (infer-name #'lhs-e.var-ids)
        (values
         (list
          #'(define-values lhs-e.var-ids
              (let ([tmp-id (let ([name-id (rhombus-expression (group rhs))])
                              name-id)])
                (let-values ([(match? . lhs-e.var-ids)
                              (lhs-e.check-proc-expr tmp-id)])
                  (unless match?
                    (rhs-binding-failure 'form-id tmp-id 'lhs))
                  (values . lhs-e.var-ids))))
          #'lhs-e.post-defn)
         null)]))))

(define (rhs-binding-failure who val binding)
  (raise-binding-failure who "value" val binding))

(define-for-syntax (infer-name var-ids)
  (syntax-parse var-ids
    [(id) #'id]
    [_ (car (generate-temporaries (list #'rhs)))]))
