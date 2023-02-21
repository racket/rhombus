#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "statically-str.rkt")
         "parse.rkt"
         "parens.rkt"
         "static-info.rkt")

(provide (for-syntax dot-parse-dispatch)
         method1)

(define-for-syntax (dot-parse-dispatch k)
  (lambda (lhs dot-stx field-stx tail more-static? success-k fail-k)
    (define (ary mask n-k no-k)
      (define (bad msg)
        (raise-syntax-error #f msg field-stx))
      (syntax-parse tail
        #:datum-literals ()
        [((_::parens g ...) . new-tail)
         (define gs (syntax->list #'(g ...)))
         (cond
           [(bitwise-bit-set? mask (length gs))
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

    (define (0ary id [static-infos #'()])
      (ary 1
           (lambda () (wrap-static-info*
                       #`(#,id #,lhs)
                       static-infos))
           (lambda () (wrap-static-info*
                       #`(let ([#,id (lambda () (#,id #,lhs))])
                           #,id)
                       static-infos))))

    (define (nary id mask direct-id [static-infos #'()])
      (ary mask
           (lambda args (wrap-static-info*
                         #`(#,direct-id #,lhs . #,args)
                         static-infos))
           (lambda () (wrap-static-info*
                       #`(let ([#,direct-id (lambda () (#,id #,lhs))])
                           #,direct-id)
                       static-infos))))

    (define (field mk) (values (mk lhs) tail))

    (k (syntax-e field-stx) field ary 0ary nary fail-k)))

(define (method1 proc)
  (lambda (v)
    (lambda ()
      (proc v))))
