#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse)
         "expression.rkt")

(provide (for-syntax
          is-static-context?
          is-static-context/tail?
          add-dynamism-context))

(module+ for-dynamic-static
  (provide #%static
           #%dynamic))

(define-for-syntax invalid-as-expression
  (expression-transformer
   (lambda (stx)
     (raise-syntax-error #f "invalid as expression" stx))))

(define-syntax #%static invalid-as-expression)
(define-syntax #%dynamic invalid-as-expression)

(define-for-syntax (is-static-context? tag)
  (free-identifier=? (datum->syntax tag '#%dynamism)
                     (expr-quote #%static)))

(define-for-syntax (is-static-context/tail? tail)
  (syntax-parse tail
    [(n::name . _)
     (is-static-context? #'n.name)]
    [_
     (error 'is-static-context/tail? "tail doesn't start with a name: ~s" tail)]))

(define-for-syntax (add-dynamism-context stx static? space)
  (define b0 (identifier-binding (if space
                                     ((make-interned-syntax-introducer space) stx 'add)
                                     stx)))
  (define b (identifier-binding (if static? (expr-quote #%static) (expr-quote #%dynamic))))
  (let* ([binds (syntax-binding-set)]
         [binds (if (and b0 (not (eq? (syntax-e stx) '#%dynamism)))
                    (syntax-binding-set-extend binds
                                               (syntax-e stx)
                                               (syntax-local-phase-level)
                                               (car b0)
                                               #:source-symbol (cadr b0)
                                               #:source-phase (list-ref b0 4))
                    binds)]
         [binds (syntax-binding-set-extend binds
                                           '#%dynamism
                                           (syntax-local-phase-level)
                                           (car b)
                                           #:source-symbol (cadr b)
                                           #:source-phase (list-ref b 4))])
    (syntax-binding-set->syntax binds (syntax-e stx))))
