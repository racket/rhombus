#lang racket/base

(provide make-raise-misuse)

(define (make-raise-misuse what)
  (lambda (self stx)
    (raise-syntax-error #f
                        (format "misuse of ~a as an expression"
                                what)
                        stx)))
