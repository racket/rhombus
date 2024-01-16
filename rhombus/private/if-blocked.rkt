#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide if/blocked
         if/flattened)

(define-syntax (if/blocked stx)
  (syntax-parse stx
    [(_ #t success-form _)
     #'(let () success-form)]
    [(_ check-expr success-form fail-expr)
     #'(if check-expr
           (let () success-form)
           fail-expr)]))

(define-syntax (if/flattened stx)
  (syntax-parse stx
    [(_ #t success-form _)
     #'success-form]
    [(_ check-expr success-form fail-expr)
     #'(begin
         (unless check-expr fail-expr)
         success-form)]))
