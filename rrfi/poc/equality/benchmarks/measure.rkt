#lang racket

(require "data/objects.rkt"
         "../util.rkt"
         "data/util.rkt"
         (prefix-in b: "../base/api.rkt")
         (prefix-in k: "../key/api.rkt")
         (prefix-in d: data/collection))

(define (exerciser eqf a b n)
  (for ([i n])
    (eqf a b)))

(define (hash-exerciser hf v n)
  (for ([i n])
    (hf v)))

(define (measure-comparison eqf [n 10000])
  (for*/list ([a all-values]
              [b all-values])
    (let* ([time-ms (measure-ms exerciser eqf a b n)]
           [is-equal? (eqf a b)])
      (vector-immutable a b (type a) (type b) is-equal? time-ms))))

(define (measure-hashing hf [n 100000])
  (for/list ([v all-values])
    (let ([time-ms (measure-ms hash-exerciser hf v n)])
      (vector-immutable v
                        (type v)
                        time-ms))))

(define (run)
  (define base-comparison-results (measure-comparison b:=))
  (define key-comparison-results (measure-comparison k:=))
  (define base-hashing-results (measure-hashing b:hash-code))
  (define key-hashing-results (measure-hashing k:hash-code))
  (list (list (d:apply + (map d:last base-comparison-results))
              (d:apply + (map d:last key-comparison-results)))
        (list (d:apply + (map d:last base-hashing-results))
              (d:apply + (map d:last key-hashing-results)))))

(module+ main
  (run))
