#lang racket/base

(provide adjust-procedure-name)

(define renames
  #hasheq((length . ConsList.length)
          (vector-length . Array.length)
          (hash-count . Map.count)))

(define (adjust-procedure-name name realm)
  (cond
    [(eq? realm 'racket/primitive)
     (hash-ref renames name name)]
    [else name]))
