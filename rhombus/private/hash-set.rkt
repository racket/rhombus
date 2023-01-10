#lang racket/base

;; lightweight sets as hash tables, for internal use

(provide list->set
         set->list
         set-intersect
         set-union
         subset?)

(define (list->set l)
  (for/hasheq ([v (in-list l)])
    (values v #t)))

(define (set->list s)
  (hash-keys s))

(define set-intersect
  (case-lambda
    [(a b)
     (if ((hash-count a) . <= . (hash-count b))
         (for/hasheq ([k (in-hash-keys b)]
                      #:when (hash-ref a k #f))
           (values b #t))
         (set-intersect b a))]
    [as (for/fold ([s #hasheq()])
                  ([a (in-list as)])
          (set-intersect s a))]))         
    
(define set-union
  (case-lambda
    [(a b)
     (if ((hash-count a)  . >= . (hash-count b))
         (for/fold ([a a]) ([k (in-hash-keys b)])
           (hash-set a k #t))
         (set-union b a))]))

(define (subset? a b)
  (hash-keys-subset? a b))
