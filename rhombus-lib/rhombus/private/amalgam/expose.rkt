#lang racket/base

(provide make-expose)

(define (make-expose scope-stx base-stx)
  (let ([intro (make-syntax-delta-introducer scope-stx base-stx)])
    (lambda (stx)
      (intro stx 'remove))))
