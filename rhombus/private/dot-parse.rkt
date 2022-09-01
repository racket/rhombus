#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt")

(provide (for-syntax dot-parse-dispatch)
         method1)

(define-for-syntax (dot-parse-dispatch k)
  (lambda (lhs dot-stx field-stx tail more-static? success-k fail-k)
    (define (ary n n-k no-k)
      (define (bad)
        (raise-syntax-error #f
                            (format "expected parentheses afterward with ~a arguments" n)
                            field-stx))
      (syntax-parse tail
        #:datum-literals (parens)
        [((parens g ...) . new-tail)
         (define gs (syntax->list #'(g ...)))
         (cond
           [(= (length gs) n)
            (success-k (apply n-k (syntax->list #'((rhombus-expression g) ...)))
                       #'new-tail)]
           [else
            (if more-static?
                (bad)
                (values (no-k) tail))])]
        [_
         (if more-static?
             (bad)
             (values (no-k) tail))]))

    (define (0ary id)
      (ary 0
           (lambda () #`(#,id #,lhs))
           (lambda () #`(let ([#,id (lambda () (#,id #,lhs))])
                          #,id))))

    (define (nary id n direct-id)
      (ary n
           (lambda args #`(#,direct-id #,lhs . #,args))
           (lambda () #`(let ([#,direct-id (lambda () (#,id #,lhs))])
                          #,direct-id))))

    (k (syntax-e field-stx) ary 0ary nary fail-k)))

(define (method1 proc)
  (lambda (v)
    (lambda ()
      (proc v))))
