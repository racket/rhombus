#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/transformer
                     "consistent.rkt"
                     "infer-name.rkt"
                     (for-syntax racket/base
                                 syntax/parse)
                     "syntax-rhs.rkt")
         "definition.rkt"
         "binding.rkt"
         "expression.rkt"
         "parse.rkt"
         (submod "function.rkt" for-build)
         "quasiquote.rkt"
         (for-syntax "parse.rkt")
         (submod "value.rkt" for-define)
         "syntax.rkt"
         (submod "expression-syntax.rkt" for-define))

(provide (rename-out [rhombus-define def]))

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
       [(form-id ((~and alts-tag alts) (block (group id:identifier (parens arg::non-...-binding ... rest::maybe-arg-rest)
                                                     ret::ret-annotation
                                                     (~and rhs (block body ...))))
                                       ...+))
        (define ids (syntax->list #'(id ...)))
        (define the-id (car ids))
        (check-consistent stx ids "name")
        (list
         (wrap-definition
          #`(define #,the-id
              #,(build-case-function the-id
                                     #'((arg ...) ...) #'((arg.parsed ...) ...)
                                     #'(rest.arg ...) #'(rest.parsed ...)
                                     #'(ret.predicate ...)
                                     #'(rhs ...)
                                     #'form-id #'alts-tag))))]
       [(form-id id::non-binding-identifier ((~and parens-tag parens) arg::kw-opt-binding ... rest::maybe-arg-rest)
                 ret::ret-annotation
                 (~and rhs (block body ...)))
        #:with (arg-id ...) (generate-temporaries #'(arg ...))
        (list
         (wrap-definition
          #`(define id
              #,(build-function #'id
                                #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                                #'rest.arg #'rest.parsed
                                #'ret.predicate
                                #'rhs
                                #'form-id
                                #'parens-tag))))]
       [(form-id (~optional (~literal values)) (parens g ...) (~and rhs (block body ...)))
        (build-values-definitions #'form-id #'(g ...) #'rhs
                                  wrap-definition)]
       [(form-id ((~and alts-tag alts) (block (group q::operator-syntax-quote
                                                     (~and rhs (block body ...))))
                                       ...+))
        (list
         (wrap-definition
          (parse-operator-definitions 'rule
                                      stx
                                      (syntax->list #'(q.g ...))
                                      (syntax->list #'(rhs ...))
                                      in-expression-space
                                      #'rules-rhs)))]
       [(form-id q::operator-syntax-quote
                 (~and rhs (block body ...)))
        (list
         (wrap-definition
          (parse-operator-definition 'rule
                                     #'q.g
                                     #'rhs
                                     in-expression-space
                                     #'rule-rhs)))]
       [(form-id any ...+ (~and rhs (block body ...)))
        (build-value-definitions #'form-id #'(group any ...) #'rhs
                                 wrap-definition)]))))

(define-syntax rhombus-define
  (make-define (lambda (defn) defn)))

(begin-for-syntax
  (define-syntax (rules-rhs stx)
    (syntax-parse stx
      [(_ orig-stx pre-parsed ...)
       (parse-operator-definitions-rhs #'orig-stx (syntax->list #'(pre-parsed ...))
                                       #'make-expression-prefix-operator
                                       #'make-expression-infix-operator
                                       #'expression-prefix+infix-operator)]))
  (define-syntax (rule-rhs stx)
    (syntax-parse stx
      [(_ pre-parsed)
       (parse-operator-definition-rhs #'pre-parsed
                                      #'make-expression-prefix-operator
                                      #'make-expression-infix-operator)])))
