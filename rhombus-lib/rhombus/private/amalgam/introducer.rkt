#lang racket/base

(provide make-interned-syntax-introducer/add
         space->introducer)

;; hack: a macro so that a name is inferred
(define-syntax-rule (make-interned-syntax-introducer/add sym)
  (let ([proc (make-interned-syntax-introducer sym)])
    (lambda (stx [mode 'add]) (proc stx mode))))

(define (space->introducer space-sym)
  (if space-sym
      (make-interned-syntax-introducer/add space-sym)
      (lambda (id) id)))
