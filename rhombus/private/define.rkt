#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "consistent.rkt")
         "definition.rkt"
         "binding.rkt"
         "expression.rkt"
         "parse.rkt"
         "function.rkt"
         (submod "function.rkt" for-call)
         "quasiquote.rkt"
         (for-syntax "parse.rkt")
         "forwarding-sequence.rkt")

(provide (rename-out [rhombus-define define])
         forward)

(begin-for-syntax
  (struct fcase (args arg-parseds rhs))
  (define (group-by-counts fcases)
    (define ht
      (for/fold ([ht #hasheqv()]) ([fc (in-list fcases)])
        (define n (length (fcase-args fc)))
        (hash-set ht n (cons fc (hash-ref ht n '())))))
    (for/list ([sames (in-hash-values ht)])
      (reverse sames))))

(define-for-syntax (make-define wrap-definition)
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
        (define arg-parsedss (map syntax->list (syntax->list #'((arg.parsed ...) ...))))
        (define rhss (syntax->list #'(rhs ...)))
        (list
         (wrap-definition
          #`(define #,the-id
              #,(build-case-function the-id argss arg-parsedss rhss #'form-id #'alts-tag))))]
       [(form-id id::non-binding-identifier ((~and parens-tag parens) arg::kw-opt-binding ...)
                 (~and rhs (block body ...)))
        #:with (arg-id ...) (generate-temporaries #'(arg ...))
        (list
         (wrap-definition
          #`(define id
              #,(build-function #'id #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...) #'rhs #'form-id #'parens-tag))))]
       [(form-id (~literal values) (parens g ...) (~and rhs (block body ...)))
        #:with (lhs::binding ...) #'(g ...)
        #:with (lhs-e::binding-form ...) #'(lhs.parsed ...)
        #:with (name-id ...) (map infer-name (syntax->list #'(lhs-e.var-ids ...)))
        #:with (tmp-id ...) (generate-temporaries #'(name-id ...))
        (list
         (wrap-definition
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
                 (values lhs-e.var-id ... ...)))))
         (wrap-definition
          #'(begin
              lhs-e.post-defn ...)))]
       [(form-id any ... (~and rhs (block body ...)))
        #:with lhs::binding #'(group any ...)
        #:with lhs-e::binding-form #'lhs.parsed
        #:with name-id (infer-name #'lhs-e.var-ids)
        (list
         (wrap-definition
          #'(define-values lhs-e.var-ids
              (let ([tmp-id (let ([name-id (rhombus-expression (group rhs))])
                              name-id)])
                (let-values ([(match? . lhs-e.var-ids)
                              (lhs-e.check-proc-expr tmp-id)])
                  (unless match?
                    (rhs-binding-failure 'form-id tmp-id 'lhs))
                  (values . lhs-e.var-ids)))))
         (wrap-definition
          #'lhs-e.post-defn))]))))

(define-syntax rhombus-define
  (make-define (lambda (defn) defn)))

(define-syntax forward
  (make-define (lambda (defn) #`(rhombus-forward #,defn))))

(define (rhs-binding-failure who val binding)
  (raise-binding-failure who "value" val binding))

(define-for-syntax (infer-name var-ids)
  (syntax-parse var-ids
    [(id) #'id]
    [_ (car (generate-temporaries (list #'rhs)))]))
