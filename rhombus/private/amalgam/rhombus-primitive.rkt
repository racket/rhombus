#lang racket/base
(provide set-primitive-contract!
         set-primitive-contract-combinator!
         get-primitive-contract
         set-primitive-who!
         get-primitive-who)

(define primitive-contract-table (make-hash))

(define primitive-contract-combinator-table (make-hasheq))

(define primitive-who-table (make-hasheq))

(define (set-primitive-contract! contract/rkt contract/rhm)
  (hash-set! primitive-contract-table contract/rkt contract/rhm))

(define (set-primitive-contract-combinator! head handler)
  (hash-set! primitive-contract-combinator-table head handler))

(define (get-primitive-contract contract/rkt)
  (cond
    [(and (pair? contract/rkt)
          (hash-ref primitive-contract-combinator-table (car contract/rkt) #f))
     => (lambda (handler)
          (handler contract/rkt))]
    [else (hash-ref primitive-contract-table contract/rkt #f)]))

(define (set-primitive-who! who/rkt who/rhm)
  (hash-set! primitive-who-table who/rkt who/rhm))

(define (get-primitive-who who/rkt)
  (hash-ref primitive-who-table who/rkt #f))
