#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "operator-parse.rkt")
         "expression.rkt"
         "binding.rkt")

(provide define-placeholder-syntax)

(define-syntax (define-placeholder-syntax stx)
  (syntax-parse stx
    [(_ id expr-misuse bind-misuse)
     #'(begin
         (define-syntax id
           (expression-transformer
            (lambda (stx)
              (syntax-parse stx
                [(op::operator . tail)
                 (raise-syntax-error #f
                                     expr-misuse
                                     #'op.name)]))))

         (define-binding-syntax id
           (binding-transformer
            (lambda (stx)
              (syntax-parse stx
                [(op::operator . tail)
                 (raise-syntax-error #f
                                     bind-misuse
                                     #'op.name)])))))]))
