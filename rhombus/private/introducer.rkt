#lang racket/base

(provide make-interned-syntax-introducer/add
         space->introducer)

(define (make-interned-syntax-introducer/add sym)
  (define proc (make-interned-syntax-introducer sym))
  (lambda (stx [mode 'add]) (proc stx mode)))

(define (space->introducer space-sym)
  (if space-sym
      (make-interned-syntax-introducer/add space-sym)
      (lambda (id) id)))
