#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "group.rkt")
         "parse.rkt"
         "expression.rkt"
         "static-info.rkt")

(provide #%effect)

(define (print-values . vals)
  (for-each (current-print) vals)
  (apply values vals))

(define-syntax #%effect
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        #:with e::expression (regroup #'tail)
        (values #`(call-with-values
                   (lambda () #,(discard-static-infos #'e.parsed))
                   print-values)
                #'())]))))
