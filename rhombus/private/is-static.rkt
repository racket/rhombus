#lang racket/base
(require (for-syntax racket/base)
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
  (is-static-context? (car (syntax-e tail))))
