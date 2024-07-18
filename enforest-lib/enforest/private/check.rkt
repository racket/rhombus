#lang racket/base
(require "../proc-name.rkt")

(provide check-is-syntax
         parsed-wrong-context-error)

(define (check-is-syntax val proc . env)
  (unless (syntax? val)
    (raise-result-error (proc-name proc) "syntax?" val))
  val)

(define (parsed-wrong-context-error what stx)
  (raise-syntax-error #f
                      (string-append "term is for a context other than " what)
                      stx))
