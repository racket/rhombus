#lang racket/base
(provide set-primitive-contract!
         set-primitive-contract-combinator!
         set-primitive-subcontract!
         set-primitive-who!
         get-primitive-contract
         get-or-format-primitive-contract
         get-primitive-subcontract
         get-primitive-who
         primitive-who-table-key)

(define primitive-contract-table (make-hash))

(define primitive-contract-combinator-table (make-hash))

(define primitive-subcontract-table (make-hash))

(define primitive-who-table (make-hasheq))

(define primitive-who-table-key (gensym 'who-table))

(define (set-primitive-contract! contract/rkt contract/rhm)
  (hash-set! primitive-contract-table contract/rkt contract/rhm))

(define (set-primitive-contract-combinator! head handler)
  (hash-set! primitive-contract-combinator-table head handler))

(define (set-primitive-subcontract! contracts/rkt contract/rkt)
  (hash-set! primitive-subcontract-table contracts/rkt contract/rkt))

(define (set-primitive-who! who/rkt who/rhm)
  (hash-set! primitive-who-table who/rkt who/rhm))

(define (get-primitive-contract contract/rkt)
  (cond
    [(hash-ref primitive-contract-table contract/rkt #f)
     => (lambda (contract/rhm) contract/rhm)]
    [(and (pair? contract/rkt)
          (hash-ref primitive-contract-combinator-table (car contract/rkt) #f))
     => (lambda (handler)
          (handler contract/rkt))]
    [else #f]))

(define (get-or-format-primitive-contract ctc)
  (or (get-primitive-contract ctc)
      (cond
        [(not ctc) "False"]
        [(and (pair? ctc)
              (eq? 'quote (car ctc))
              (= 2 (length ctc)))
         (format "matching(~a)"
                 ((error-value->string-handler) (cadr ctc) (error-print-width)))]
        [else
         ((error-value->string-handler) ctc (error-print-width))])))

(define (get-primitive-subcontract contracts/rkt)
  (cond
    [(hash-ref primitive-subcontract-table contracts/rkt #f)
     => (lambda (contract/rkt)
          (hash-ref primitive-contract-table contract/rkt #f))]
    [else #f]))

(define (get-primitive-who who/rkt)
  (or (hash-ref (continuation-mark-set-first #f primitive-who-table-key #hasheq()) who/rkt #f)
      (hash-ref primitive-who-table who/rkt #f)))
