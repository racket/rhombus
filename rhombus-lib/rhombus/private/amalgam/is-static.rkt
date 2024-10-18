#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse)
         "expression.rkt")

(provide (for-syntax
          is-static-context?
          is-static-context/tail?))

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
