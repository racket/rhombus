#lang racket/base

(provide values->list)

(define-syntax-rule (values->list expr)
  (call-with-values (λ () expr)
                    list))
