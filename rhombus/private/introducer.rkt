#lang racket/base

(provide make-interned-syntax-introducer/add)

(define (make-interned-syntax-introducer/add sym)
  (define proc (make-interned-syntax-introducer sym))
  (lambda (stx [mode 'add]) (proc stx mode)))
