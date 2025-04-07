#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/deprecated))

(provide define-deprecated)

(define-syntax (define-deprecated stx)
  (syntax-parse stx
    [(_ id name (space ...) date to-id)
     #`(begin
         #,@(for/list ([space (in-list (syntax->list #'(space ...)))])
              (define intro
                (if (syntax-e space)
                    (make-interned-syntax-introducer (syntax-e space))
                    (lambda (x) x)))
              #`(define-syntax #,(intro #'id)
                  (make-deprecated-rename-transformer (quote-syntax #,(intro #'to-id)) 'name date))))]))
