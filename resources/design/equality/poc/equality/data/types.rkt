#lang racket/base

(provide (struct-out person))

(require racket/generic
         "../key/interface.rkt")

(struct person (name age)
  #:transparent

  #:methods gen:comparable
  [(define (key this)
     (vector-immutable (person-name this)
                       (person-age this)))]

  #:methods gen:equal+hash
  [(define (equal-proc this other recur)
     (and (recur (person-name this)
                 (person-name other))
          (recur (person-age this)
                 (person-age other))))
   (define (hash-proc this recur)
     (+ (* 3 (recur (person-name this)))
        (recur (person-age this))))
   (define (hash2-proc this recur)
     (+ (recur (person-name this))
        (recur (person-age this))))])
