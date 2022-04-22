#lang racket

(provide run)

(require "data/objects.rkt"
         "util.rkt"
         "data/util.rkt"
         (prefix-in b: "base/api.rkt")
         (prefix-in k: "key/api.rkt")
         data/collection)

(define (exerciser eqf a b n)
  (for ([i n])
    (eqf a b)))

(define (measure eqf [n 10000])
  (for*/list ([a all-values]
              [b all-values])
    (let* ([time-ms (measure-ms exerciser eqf a b n)]
           [is-equal? (eqf a b)])
      (vector-immutable a b (type a) (type b) is-equal? time-ms))))

(define (run)
  (define base-results (measure b:=))
  (define key-results (measure k:=))
  (list (apply + (map last base-results))
        (apply + (map last key-results))))

(module+ main
  (display-results (run)))
