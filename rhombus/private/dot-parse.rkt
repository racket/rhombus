#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "statically-str.rkt")
         "parse.rkt"
         "parens.rkt")

(provide (for-syntax dot-parse-dispatch)
         method1)

(define-for-syntax (dot-parse-dispatch k)
  (lambda (lhs dot-stx field-stx tail more-static? success-k fail-k)
    (define (ary n n-k no-k)
      (define (bad msg)
        (raise-syntax-error #f msg field-stx))
      (syntax-parse tail
        #:datum-literals ()
        [((_::parens g ...) . new-tail)
         (define gs (syntax->list #'(g ...)))
         (cond
           [(if (n . < . 0)
                (> (length gs) (- 1 n))
                (= (length gs) n))
            (success-k (apply n-k (syntax->list #'((rhombus-expression g) ...)))
                       #'new-tail)]
           [else
            (if more-static?
                (bad (string-append "wrong number of arguments in method call" statically-str))
                (values (no-k) tail))])]
        [_
         (if more-static?
             (bad "expected parentheses afterward")
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

    (define (field mk) (values (mk lhs) tail))

    (k (syntax-e field-stx) field ary 0ary nary fail-k)))

(define (method1 proc)
  (lambda (v)
    (lambda ()
      (proc v))))
