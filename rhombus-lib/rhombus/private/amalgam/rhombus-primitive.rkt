#lang racket/base
(provide set-primitive-contract!
         set-primitive-contract-combinator!
         set-primitive-who!
         get-primitive-contract
         get-or-format-primitive-contract
         get-combined-primitive-contract
         get-primitive-who
         primitive-who-table-key)

(define primitive-contract-table (make-hash))

(define primitive-contract-combinator-table (make-hash))

(define primitive-who-table (make-hasheq))

(define primitive-who-table-key (gensym 'who-table))

(define (set-primitive-contract! contract/rkt contract/rhm)
  (hash-set! primitive-contract-table contract/rkt contract/rhm))

(define (set-primitive-contract-combinator! head handler)
  (hash-set! primitive-contract-combinator-table head handler))

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

(define (get-combined-primitive-contract sep-op-sep form)
  ;; We won't get here if a full combination is recognized, like
  ;; `(and/c hash? immutable?)`. We might need to combine the head
  ;; combinator with subsets of the elements of `form` to support
  ;; something like `(and/c X? hash? immutable?)`, but that
  ;; doesn't seem needed, yet.
  (apply string-append
         (get-or-format-primitive-contract (cadr form))
         (for/list ([contract/rkt (in-list (cddr form))])
           (string-append sep-op-sep (get-or-format-primitive-contract contract/rkt)))))

(define (get-primitive-who who/rkt)
  (or (let ([t (continuation-mark-set-first #f primitive-who-table-key #hasheq())])
        (cond
          [(not t) #t]
          [(pair? t) (and (eq? (car t) who/rkt) (cdr t))]
          [else (hash-ref t who/rkt #f)]))
      (hash-ref primitive-who-table who/rkt #f)))
