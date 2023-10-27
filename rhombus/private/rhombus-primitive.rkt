#lang racket/base
(provide set-primitive-contract!
         get-primitive-contract
         set-primitive-who!
         get-primitive-who)

(define primitive-contract-table (make-hash))

(define primitive-who-table (make-hasheq))

(define (set-primitive-contract! contract/rkt contract/rhm)
  (hash-set! primitive-contract-table contract/rkt contract/rhm))

(define (get-primitive-contract contract/rkt)
  (hash-ref primitive-contract-table contract/rkt #f))

(define (set-primitive-who! who/rkt who/rhm)
  (hash-set! primitive-who-table who/rkt who/rhm))

(define (get-primitive-who who/rkt)
  (hash-ref primitive-who-table who/rkt #f))

(set-primitive-contract! 'exact-nonnegative-integer? "NonnegInt")

(set-primitive-who! 'application '|function call|)
