#lang racket/base
(require "../proc-name.rkt")

(provide check-is-syntax)

(define (check-is-syntax val proc)
  (unless (syntax? val)
    (raise-result-error (proc-name proc) "syntax?" val))
  val)
