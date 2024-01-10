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

(define (set-intersect a b)
  (let-values ([(a b)
                (if ((hash-count a) . <= . (hash-count b))
                    (values a b)
                    (values b a))])
    (for/hasheq ([k (in-hash-keys b)]
                 #:when (hash-ref a k #f))
      (values k #t))))

(define (set-union a b)
  (let-values ([(a b)
                (if ((hash-count a) . >= . (hash-count b))
                    (values a b)
                    (values b a))])
    (for/fold ([a a]) ([k (in-hash-keys b)])
      (hash-set a k #t))))

(define (subset? a b)
  (hash-keys-subset? a b))
