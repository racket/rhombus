#lang racket/base

(provide =
         hash-code
         ground-representative)

(require "interface.rkt"
         "util.rkt"
         memoize)

(define (= a b)
  (and (eq? (hash-code a) (hash-code b)) ; can use eq? since fixnums are interned
       (equal? (ground-representative a)
               (ground-representative b))))

(define/memo (hash-code v)
  (equal-hash-code
   (if (struct? v)
       (cons (type-tag v)
             (ground-representative v))
       v)))

(define/memo (ground-representative v)
  (let ([result (full-key v)])
    (if (equal? v result)
        result
        (ground-representative result))))
