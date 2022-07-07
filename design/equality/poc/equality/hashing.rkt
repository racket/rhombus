#lang racket

(provide run)

(require "data/objects.rkt"
         "util.rkt"
         "data/util.rkt"
         (prefix-in b: "base/api.rkt")
         (prefix-in k: "key/api.rkt")
         data/collection)

(define (exerciser hf v n)
  (for ([i n])
    (hf v)))

(define (measure hf [n 100000])
  (for/list ([v all-values])
    (let ([time-ms (measure-ms exerciser hf v n)])
      (vector-immutable v
                        (type v)
                        time-ms))))

(define (run)
  (define base-results (measure b:hash-code))
  (define key-results (measure k:hash-code))
  (list (apply + (map last base-results))
        (apply + (map last key-results))))

(module+ main
  (display-results (run)))
